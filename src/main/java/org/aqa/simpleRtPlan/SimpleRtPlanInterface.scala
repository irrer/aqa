/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.simpleRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.IsInput
import org.aqa.web.WebUtil.ToHtml
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

/**
  * Generate a simple DICOM RTPLAN file customized for the user's environment.
  */
class SimpleRtPlanInterface extends Restlet with SubUrlAdmin with Logging {

  private val pageTitleSelect = "Simple RTPlan"

  /** Default tolerance table label. */
  private val defaultToleranceTableLabel = "PELVIS"

  /** Maximum beam width in mm. */
  private val maxWidth_mm = 240.0

  /** Maximum beam height in mm. */
  private val maxHeight_mm = 240.0

  /** Default beam width in mm. */
  private val defaultWidth_mm = 240.0

  /** Default beam height in mm. */
  private val defaultHeight_mm = 180.0

  /** Default beam treatment limit is seconds. */
  private val defaultBeamDeliveryDurationLimit = 0.6

  private def energySelectList = Seq(("6", "6"), ("16", "16"))

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match planning system name", false)

  private def planName = new WebInputText("Plan Name", true, 2, 0, "Name to distinguish this plan from others", false)

  private def machineName = new WebInputText("Machine Name", true, 2, 0, "", false)

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private lazy val templateFiles = {
    if (true) {
      val f = new TemplateFiles().ofModality("RTPLAN").head.file
      Trace.trace("------------------------------------------------------------")
      Trace.trace(DicomUtil.attributeListToString(new DicomFile(f).attributeList.get))
      Trace.trace("------------------------------------------------------------")
    }
    new TemplateFiles
  }

  /** Inserts vertical spacing above beam list. */
  private def header = {
    val imgElem = {
      <a rel="/static/images/CoordinateDiagram.png" class="screenshot" href="/static/images/CoordinateDiagram.png" title="">
        <img src="/static/images/CoordinateDiagram.png" height="65"/>
      </a>
    }
    new WebPlainText(label = "header", showLabel = false, col = 5, offset = 0, html = _ => <center style="margin-top:50px;"><b><h4>Beam Parameters {imgElem}</h4></b></center>)
  }

  private var idCount = 0
  private def uniqueId: String =
    idCount.synchronized {
      idCount = idCount + 1
      "id" + idCount
    }

  private def makePlainText(text: String, title: Option[String] = None, col: Int = 1): WebPlainText = {
    val elem =
      if (title.isDefined)
        <center title={title.get}><b>{text}</b></center>
      else
        <center><b>{text}</b></center>
    new WebPlainText(label = uniqueId, showLabel = false, col = col, offset = 0, html = _ => elem)
  }

  private def headerRow: WebRow = {
    List(
      makePlainText(text = "Gantry Angle", Some("In degrees")),
      new WebPlainText(label = uniqueId, showLabel = false, col = 1, offset = 0, html = _ => <b title="Beam name in RTPLAN">Beam</b>), // do not center this
      makePlainText(text = "Energy", Some("Beam Energy")),
      makePlainText(
        text = "Width mm",
        Some("Width of beam in mm (along the X or Y axis," + titleNewline + "e.g. Anterior to posterior or Sagittal" + titleNewline + "left to right distance.)")
      ),
      makePlainText(text = "Height mm", Some("Height of beam in mm (along the Z axis," + titleNewline + "e.g. Superior to inferior distance.)"))
    )
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  case class BeamColumn(rtplan: AttributeList, beamAl: AttributeList) {

    private val templateBeamName = beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString()
    private val prefix = templateBeamName

    val beamNumber: Int = beamAl.get(TagByName.BeamNumber).getIntegerValues.head

    val beamRef: AttributeList = {
      val fractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence)
      val referencedBeamSequenceList = fractionGroupSequence.flatMap(fgs => DicomUtil.seqToAttr(fgs, TagByName.ReferencedBeamSequence))
      val seqOfInterest = referencedBeamSequenceList.find(rbs => rbs.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == beamNumber).get

      val isT = {
        val bms = seqOfInterest.get(TagByName.BeamMeterset)
        (bms != null) && (bms.getIntegerValues.head > 0)
      }

      // if this is a treatment beam and there is no BeamDeliveryDurationLimit, then add a BeamDeliveryDurationLimit
      if (isT && seqOfInterest.get(TagByName.BeamDeliveryDurationLimit) == null) {
        val bms = AttributeFactory.newAttribute(TagByName.BeamDeliveryDurationLimit)
        bms.addValue(defaultBeamDeliveryDurationLimit)
        seqOfInterest.put(bms)
      }
      seqOfInterest
    }

    // true if this is a treatment beam.
    val isTreat: Boolean = {
      val attr = beamRef.get(TagByName.BeamMeterset)
      (attr != null) && (attr.getIntegerValues.head > 0)
    }

    //noinspection TypeAnnotation
    object EntryType extends Enumeration {
      val Numeric = Value // user can enter data, must be valid numeric
      val Text = Value // user can enter arbitrary text
      val Display = Value // user not can enter data, for display only
    }

    def noValidation(valueMap: ValueMapT, col: Col): StyleMapT = styleNone

    def noPut(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = simpleBeamSpecification

    case class Col(
        name: String,
        entryType: EntryType.Value,
        init: () => String,
        validate: (ValueMapT, Col) => StyleMapT = noValidation,
        put: (ValueMapT, SimpleBeamSpecification, Col) => SimpleBeamSpecification = noPut
    ) {

      val label: String = prefix + name

      def field: IsInput with ToHtml = {
        if (entryType.toString.equals(EntryType.Display.toString)) {
          new WebPlainText(label = name, showLabel = false, col = 1, offset = 0, _ => <span>{init()}</span>)
        } else {
          new WebInputText(label = name, showLabel = false, col = 1, offset = 0, placeholder = "", aqaAlias = false)
        }
      }
    }

    import EntryType._

    private def beamList(tag: AttributeTag): Seq[Attribute] = DicomUtil.findAllSingle(beamAl, tag)

    private def beamDbl(tag: AttributeTag): Double = {
      beamList(tag).head.getDoubleValues.head
    }

    private def beamDblS(tag: AttributeTag): String = beamDbl(tag).toString
    Trace.trace("beamRef: " + DicomUtil.attributeListToString(beamRef))
    private def beamRefList(tag: AttributeTag): Seq[Attribute] = DicomUtil.findAllSingle(beamRef, tag)

    private def beamRefDbl(tag: AttributeTag): Double = {
      val j1 = beamRefList(tag)
      val j2 = j1.head
      val j3 = j2.getDoubleValues
      val j4 = j3.head
      Trace.trace(j4)

      beamRefList(tag).head.getDoubleValues.head
    }

    private def beamRefDblS(tag: AttributeTag): String = {
      beamRefDbl(tag).toString
    }

    val typeX = Seq("X", "ASYMX")
    val typeY = Seq("Y", "ASYMY")

    private def jawDim(index: Int, jawType: Seq[String]): Double = {
      val list = beamList(TagByName.BeamLimitingDevicePositionSequence)
        .flatMap(s => DicomUtil.alOfSeq(s.asInstanceOf[SequenceAttribute]))
        .find(al => jawType.contains(al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString))
        .get
      val positions = list.get(TagByName.LeafJawPositions).getDoubleValues
      positions(index)
    }

    /**
      * Get the jaw opening size for either X or Y.
      * @param jawType List of types of jow we are looking for.
      * @return String representing jaw opening size.
      */
    private def initJawSize(jawType: Seq[String]): String = {
      (jawDim(index = 1, jawType) - jawDim(index = 0, jawType)).toString
    }

    private def validBeamName(valueMap: ValueMapT, col: Col): StyleMapT = {
      val value = valueMap(col.label)
      0 match {
        case _ if value.trim.isEmpty   => WebUtil.Error.make("", "Can not be empty")
        case _ if value.length > 64    => WebUtil.Error.make("", "Can not be longer than 64 characters")
        case _ if value.contains('\\') => WebUtil.Error.make("", "Can not contain backslash")
        case _                         => styleNone
      }
    }

    val maxJaw_cm = 24

    private def validateJaw(valueMap: ValueMapT, col: Col): StyleMapT = {
      val text = valueMap(col.label)
      0 match {
        case _ if WebUtil.stringToDouble(text).isEmpty         => Error.make(col.label, "Must be a valid floating point number.")
        case _ if WebUtil.stringToDouble(text).get < 0         => Error.make(col.label, "Negative values not allowed.")
        case _ if WebUtil.stringToDouble(text).get > maxJaw_cm => Error.make(col.label, "Value greater than " + maxJaw_cm + " not allowed.")
        case _                                                 => styleNone
      }
    }

    private def validAngle90(valueMap: ValueMapT, col: Col): StyleMapT = {

      val rightAngles = Seq(0.0, 90.0, 180.0, 270.0)

      val text = valueMap(col.label)
      0 match {
        case _ if WebUtil.stringToDouble(text).isEmpty                    => Error.make(col.label, "Must be a valid floating point number.")
        case _ if !rightAngles.contains(WebUtil.stringToDouble(text).get) => Error.make(col.label, "Angle must be one of " + rightAngles.mkString(" ") + ".")
        case _                                                            => styleNone
      }
    }

    private def validateNonNegDouble(valueMap: ValueMapT, col: Col): StyleMapT = {
      val text = valueMap(col.label)
      0 match {
        case _ if WebUtil.stringToDouble(text).isEmpty => Error.make(col.label, "Must be a valid floating point number.")
        case _ if WebUtil.stringToDouble(text).get < 0 => Error.make(col.label, "Negative values not allowed.")
        case _                                         => styleNone
      }
    }

    private def putGantryAngle(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(GantryAngle_deg = valueMap(col.label).toDouble)
    }

    private def putMU(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(BeamMeterset = valueMap(col.label).toDouble)
    }

    private def putDimX1(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(X1_mm = valueMap(col.label).toDouble * 10)
    }

    private def putDimX2(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(X1_mm = valueMap(col.label).toDouble * 10)
    }

    private def putDimY1(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(X1_mm = valueMap(col.label).toDouble * 10)
    }

    private def putDimY2(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
      simpleBeamSpecification.copy(X1_mm = valueMap(col.label).toDouble * 10)
    }

    val colList: Seq[Col] =
      Seq(
        Col("Field Order/Type", Display, init = () => { beamNumber + " / " + { if (isTreat) "Treat" else "Setup" } }),
        // Col("Field ID", if (isTreat) Text else Display, init = () => templateBeamName, validate = validLO),
        Col("Field Name", if (isTreat) Text else Display, init = () => templateBeamName, validate = validBeamName),
        Col("Technique", Display, init = () => beamAl.get(TagByName.BeamType).getSingleStringValueOrEmptyString),
        Col("Scale", Display, init = () => "Varian IEC"),
        Col("Energy", Display, init = () => beamDblS(TagByName.NominalBeamEnergy)),
        Col("Dose Rate [MU/min]", Display, init = () => beamDblS(TagByName.DoseRateSet)),
        Col("MU", Display, init = () => if (isTreat) beamRefDblS(TagByName.BeamMeterset) else "", validate = validateNonNegDouble, put = putMU),
        // Col("Dose to Beam Dose Point", Display, init = ???),
        // Col("Dose to CBCT SIM", Display, init = ???),
        Col(
          "Backup Timer [min]",
          Display,
          init = () => if (isTreat) Util.fmtDbl(beamRefDbl(TagByName.BeamDeliveryDurationLimit) / 60) else "",
          validate = validateNonNegDouble,
          put = (v: ValueMapT, sbs: SimpleBeamSpecification, col: Col) => sbs.copy(BeamDeliveryDurationLimit_sec = v(col.label).toDouble * 60) // convert from minutes to seconds
        ),
        Col("Tol. Table", Display, init = () => DicomUtil.findAllSingle(rtplan, TagByName.ToleranceTableLabel).head.getSingleStringValueOrEmptyString()),
        // Col("Calculated SSD [cm]", Display, init = ???),
        // Col("Planned SSD [cm]", Display, init = ???),
        Col("Gantry Rtn [deg]", Display, init = () => beamDblS(TagByName.GantryAngle), validate = validAngle90, put = putGantryAngle),
        Col("Coll Rtn [deg]", Display, init = () => beamDblS(TagByName.BeamLimitingDeviceAngle)),
        //
        Col("Field X [cm]", Display, init = () => initJawSize(typeX)),
        Col("X1 [cm]", Display, init = () => jawDim(index = 0, jawType = typeX).toString, validate = validateJaw, put = putDimX1),
        Col("X2 [cm]", Display, init = () => jawDim(index = 1, jawType = typeX).toString, validate = validateJaw, put = putDimX2),
        //
        Col("Field Y [cm]", Display, init = () => initJawSize(typeY)),
        Col("Y1 [cm]", Display, init = () => jawDim(index = 0, jawType = typeY).toString, validate = validateJaw, put = putDimY1),
        Col("Y2 [cm]", Display, init = () => jawDim(index = 1, jawType = typeY).toString, validate = validateJaw, put = putDimY2),
        //
        Col("Couch Vrt [cm]", Display, init = () => (beamDbl(TagByName.TableTopVerticalPosition) / 10).toString),
        Col("Couch Lng [cm]", Display, init = () => (beamDbl(TagByName.TableTopLongitudinalPosition) / 10).toString),
        Col("Couch Lat [cm]", Display, init = () => (beamDbl(TagByName.TableTopLateralPosition) / 10).toString),
        Col("Couch Rtn [deg]", Display, init = () => beamDblS(TagByName.TableTopEccentricAngle))
      )

    Trace.trace(colList.size + "  col list: " + colList.map(_.name).mkString("\n"))

    /**
      * Fill in beam values.
      *
      * @param valueMap User entries.
      * @return A single beam specification.
      */
    def toBeamSpecification(valueMap: ValueMapT): SimpleBeamSpecification = {

      val simpleBeamSpecification = SimpleBeamSpecification

      @tailrec
      def putList(sbs: SimpleBeamSpecification, cl: Seq[Col]): SimpleBeamSpecification = {
        if (cl.isEmpty) sbs
        else {
          putList(cl.head.put(valueMap, sbs, cl.head), cl.tail)
        }
      }

      val sbsInit = SimpleBeamSpecification()
      val sbsFinal = putList(sbsInit, colList)
      sbsFinal
    }

    /**
      * Values to use as defaults when starting with a blank form.
      *
      * @return List of label+value pairs.
      */
    def defaultValueMap(): ValueMapT = {
      colList.map(f => (f.label, f.init())).toMap
    }
  }

  /**
    * List of beam parameters.
    *
    * @return Parameters for all beams.
    */

  def beamColSeq: Seq[BeamColumn] = {
    val rtplan = new DicomFile(templateFiles.ofModality("RTPLAN").head.file).attributeList.get
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamAlList.map(beamAl => BeamColumn(rtplan, beamAl))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def row1: WebRow = List(patientID, patientName)
  private def row2: WebRow = List(toleranceTable, planName, machineName)
  private def row3: WebRow = List(header)
  private def row4: WebRow = headerRow

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create RTPlan", ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(makePlainText(" "), createButton, makePlainText(" "), cancelButton)

  private def makeWebForm() = {
    val beamSeq = beamColSeq

    // define the rows representing the beam data
    val beamRowSeq: List[WebRow] = {

      /**
        * Make one horizontal row if the input fields.
        * @param rowIndex Index of row
        * @return One horizontal WebRow
        */
      def makeRow(rowIndex: Int): WebRow = {
        val name = beamSeq.head.colList(rowIndex)
        val header = new WebPlainText(label = "rowHeader" + rowIndex, showLabel = false, col = 1, offset = 0, html = _ => <span>{name.name}</span>)
        val fieldList = beamSeq.map(beam => beam.colList(rowIndex).field)
        Trace.trace(rowIndex + " WebRow " + name.name)
        (header +: fieldList).toList
      }

      // make each row
      val rowList = beamSeq.head.colList.indices.map(makeRow).toList
      rowList
    }

    new WebForm(pathOf, List(row1, row2, row3, row4) ++ beamRowSeq ++ List(assignButtonList))
  }

  /**
    * Create a list of initial values.
    *
    * @return List fo initial values.
    */
  private def beamInitialValueMap: ValueMapT = {
    val all = beamColSeq.map(b => b.colList).flatten
    val valueMap = all.map(col => (col.label, col.init())).toMap
    valueMap
  }

  private def formSelect(valueMap: ValueMapT, response: Response): Unit = {
    val form = makeWebForm()
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val uniqueText = new SimpleDateFormat("yyyy-MM-dd").format(new Date)

    val defaultPatient = "$Simple_" + uniqueText

    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val planNameMap = if (empty(planName.label)) Map((planName.label, "Simple" + uniqueText.replace('-', ' '))) else emptyValueMap
    val toleranceTableMap = if (empty(toleranceTable.label)) Map((toleranceTable.label, defaultToleranceTableLabel)) else emptyValueMap

    val valMap = valueMap ++ beamInitialValueMap ++ patientIdMap ++ patientNameMap ++ planNameMap ++ toleranceTableMap
    form.setFormResponse(valMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def validateEntryFields(valueMap: ValueMapT): StyleMapT = {
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty
    val tolErr = if (empty(toleranceTable.label)) Error.make(toleranceTable, "A tolerance table name must be given.") else styleNone
    val planNameErr = if (empty(planName.label)) Error.make(planName, "A plan name must be given.") else styleNone
    val machErr = if (empty(machineName.label)) Error.make(machineName, "A machine name must be given.") else styleNone
    val patIdErr = if (empty(patientID.label)) Error.make(patientID, "A patient ID must be given.") else styleNone
    val patNameErr = if (empty(patientName.label)) Error.make(patientName, "A patient name must be given.") else styleNone
    val patIdTooLongErr = if (valueMap(patientID.label).length > 64) Error.make(patientID, "Patient ID can not be over 64 characters..") else styleNone
    val planNameTooLongErr = if (valueMap(planName.label).length > 16) Error.make(patientID, "Plan Name can not be over 16 characters..") else styleNone
    // removed this check because it might stop people from generating a plan.  Technically the DICOM spec says that it
    // must be 16 characters or shorter, but the reality is that a lot of systems handle longer strings.
    //val machTooLongErr = if (valueMap(machineName.label).size > 16) Error.make(patientID, "Machine Name can not be over 16 characters..") else styleNone

    tolErr ++ planNameErr ++ machErr ++ patIdErr ++ patNameErr ++ patIdTooLongErr ++ planNameTooLongErr // ++ machTooLongErr
  }

  private def validateBeamFields(valueMap: ValueMapT): StyleMapT = {
    val beamList = beamColSeq
    val errorList = beamList.map(b => b.colList.flatMap(c => c.validate(valueMap, c)))
    errorList.flatten.asInstanceOf[StyleMapT]
  }

  /**
    * Make sure that the configuration is set up.  If broken, the user can not fix it, but does know what
    * to request of the system administrator.
    *
    * @return Error on failure, empty list on success.
    */
  private def validateConfigAndFiles() = {
    val templateDir = Config.SimpleRtplanTemplateDir
    def rtplanFile = Util.listDirFiles(templateDir.get).find(f => f.getName.toLowerCase().contains("rtplan"))

    if (Config.SimpleRtplanTemplateDir.isDefined && rtplanFile.isDefined && rtplanFile.get.canRead)
      styleNone
    else
      Error.make(
        planName.label,
        "RTPLAN file used as template has not been set up in the configuration.  Have the system administrator configure SimpleRtplanTemplateDir."
      )
  }

  /**
    * Make sure configuration and entered fields are valid.
    *
    * @param valueMap Values entered by user.
    * @return List of errors.  List is empty if everything is ok.
    */
  private def validate(valueMap: ValueMapT): StyleMapT = {
    val errorList = validateConfigAndFiles ++ validateEntryFields(valueMap) ++ validateBeamFields(valueMap)
    errorList
  }

  private val downloadList = scala.collection.mutable.Map[String, ModifiedPlan]()

  private def downloadZip(valueMap: ValueMapT, response: Response): Unit = {
    Trace.trace()
    val zip = {
      Trace.trace()
      val uid = valueMap("download")
      Trace.trace()
      val modifiedPlan = downloadList(uid)
      Trace.trace()
      val data = modifiedPlan.zippedContent
      Trace.trace()
      data
    }
    Trace.trace()

    Trace.trace()
    val entity = new ByteArrayRepresentation(zip, MediaType.APPLICATION_GNU_ZIP)
    Trace.trace()
    response.setEntity(entity)
  }

  private def showDownload(modifiedPlan: ModifiedPlan, valueMap: ValueMapT, response: Response): Unit = {

    Trace.trace(valueMap)
    val downloadUrl = {
      val name = FileUtil.replaceInvalidFileNameCharacters(valueMap(patientID.label), '_').replace(' ', '_') + ".zip"
      val url = pathOf + "/" + name + "?download=" + modifiedPlan.rtplanUID
      Trace.trace(url)
      url
    }

    val downloadLink = new WebPlainText("Download", false, 3, 0, _ => { <h4> <a href={downloadUrl} title="Click to download zipped DICOM RTPLAN and supporting files.">Download</a></h4> })

    val dicomViewHtml = {
      val elem = {
        <div>
          <pre title="DICOM meta-data">{WebUtil.nl + modifiedPlan.rtplanText}</pre>
        </div>
      }

      elem
    }

    val dicomView = new WebPlainText("Download", false, 10, 0, _ => dicomViewHtml)

    val summary = {
      // val elem = { <pre>{WebUtil.nl + valueMapToString(valueMap).replaceAll("\n", WebUtil.nl)}</pre> }
      val elem = <div>hey</div>
      new WebPlainText("Summary", false, 10, 0, _ => elem)
    }
    /*
     */

    val rowA: WebRow = List(new WebPlainText("SummaryHeader", false, 10, 0, _ => { <h4>RTPlan Summary</h4> }))
    val rowB: WebRow = List(summary)
    val rowC: WebRow = List(downloadLink)
    val rowD: WebRow = List(new WebPlainText("DetailHeader", false, 10, 0, _ => { <h4>RTPlan as Text</h4> }))
    val rowE: WebRow = List(dicomView)

    val form = new WebForm(pathOf, List(rowA, rowB, rowC, rowD, rowE))
    form.setFormResponse(valueMap, styleNone, "Download Simple RTPLAN", response, Status.SUCCESS_OK)
  }

  private case class BeamReference(beam: AttributeList, fractionReference: AttributeList) {}

  private def createRtplan(valueMap: ValueMapT): ModifiedPlan = {

    val beamSpecificationList = beamColSeq.map(b => b.toBeamSpecification(valueMap))

    val makeRtPlan = new MakeRtPlan(
      PatientID = valueMap(patientID.label),
      PatientName = valueMap(patientName.label),
      ???, // machineName = valueMap(machineName.label), // TODO
      RTPlanLabel = valueMap(planName.label),
      ToleranceTableLabel = valueMap(toleranceTable.label),
      beamSpecificationList
    )

    val modifiedPlan = makeRtPlan.makeZipWithSupportingFiles()
    downloadList.synchronized { downloadList.put(modifiedPlan.rtplanUID, modifiedPlan) }
    modifiedPlan
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  /**
    * Show the user a message saying why the creation of a custom rtplan failed.
    */
  private def showFailedCustomize(valueMap: ValueMapT, styleMap: StyleMapT, response: Response): Unit = {
    makeWebForm().setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def valueMapToString(valueMap: ValueMapT): String = {
    val text =
      "Patient ID: " + valueMap(patientID.label) + "\n" +
        "Patient Name: " + valueMap(patientName.label) + "\n" +
        "Tolerance Table: " + valueMap(toleranceTable.label) + "\n" +
        "Plan Name: " + valueMap(planName.label) + "\n" +
        "Machine: " + valueMap(machineName.label) + "\n" +
        "hey" // beamRowSeq.map(_.toText(valueMap)).mkString("\n")
    text
  }

  private def validateAndMakePlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap: StyleMapT = validate(valueMap)
    if (styleMap.isEmpty) {
      logger.info("Creating plan.  Parameters: \n" + valueMapToString(valueMap))
      val modifiedPlan = createRtplan(valueMap)
      showDownload(modifiedPlan, valueMap, response)
    } else {
      showFailedCustomize(valueMap, styleMap, response)
    }
  }

  /**
    * Quit working on this.
    *
    * @param response Response to user.
    */
  private def quit(response: Response): Unit = response.redirectSeeOther("/static/admin.html")

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)

      0 match {
        case _ if user.isEmpty                     => quit(response)
        case _ if buttonIs(valueMap, cancelButton) => quit(response)
        case _ if valueMap.contains("download")    => downloadZip(valueMap, response)
        case _ if buttonIs(valueMap, createButton) => validateAndMakePlan(valueMap, response)
        case _                                     => formSelect(valueMap, response) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
