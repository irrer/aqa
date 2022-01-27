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

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

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

  private def energySelectList = Seq(("6", "6"), ("16", "16"), ("None", "None"))

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match planning system name", false)

  private def planName = new WebInputText("Plan Name", true, 2, 0, "Name to distinguish this plan from others", false)

  private def machineName = new WebInputText("Machine Name", true, 2, 0, "", false)

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

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
      makePlainText(text = "Energy", Some("Beam Enegy")),
      makePlainText(
        text = "Width mm",
        Some("Width of beam in mm (along the X or Y axis," + titleNewline + "e.g. Anterior to posterior or Sagittal" + titleNewline + "left to right distance.)")
      ),
      makePlainText(text = "Height mm", Some("Height of beam in mm (along the Z axis," + titleNewline + "e.g. Superior to inferior distance.)"))
    )
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  case class BeamRow(gantryAngle: Int, beamName: String) {

    private val uniqueName = "G" + gantryAngle.formatted("%03d")

    private def makeBeamName(): WebPlainText = new WebPlainText(label = uniqueName + "BeamId", showLabel = false, col = 1, offset = 0, html = _ => <b style="white-space: nowrap;">{beamName}</b>)

    private def makeEnergy() = new WebInputSelect(label = uniqueName + "Energy", showLabel = false, col = 1, offset = 0, selectList = _ => energySelectList, aqaAlias = false)

    def makeWidth() = new WebInputText(label = uniqueName + "Width", showLabel = false, col = 1, offset = 0, "", aqaAlias = false)

    def makeHeight() = new WebInputText(label = uniqueName + "Height", showLabel = false, col = 1, offset = 0, placeholder = "", aqaAlias = false)

    def makeRow(): WebRow = List(makePlainText(gantryAngle.toString), makeBeamName(), makeEnergy(), makeWidth(), makeHeight())

    def toText(valueMap: ValueMapT): String = {
      "Gantry Angle " + gantryAngle.formatted("%3d") +
        "    Energy: " + valueMap(makeEnergy().label).formatted("%5s") +
        "    Width mm: " + valueMap(makeWidth().label).formatted("%8s") +
        "    Height mm: " + valueMap(makeHeight().label).formatted("%8s")
    }

    private def beamEnergyOf(valueMap: ValueMapT): Double = {
      val text = valueMap(makeEnergy().label)
      try {
        val d = text.toDouble
        d
      } catch {
        case _: Throwable => 0
      }
    }

    def toBeamSpecification(valueMap: ValueMapT): SimpleSpecification = {
      SimpleSpecification(
        GantryAngle_deg = gantryAngle,
        BeamName = beamName,
        NominalBeamEnergy = beamEnergyOf(valueMap),
        X_mm = valueMap(makeWidth().label).toDouble,
        Y_mm = valueMap(makeHeight().label).toDouble
      )
    }

    /**
      * Values to use as defaults when starting with a blank form.
      *
      * @return List of label+value pairs.
      */
    def defaultValueMap(): ValueMapT = {
      Map(
        (uniqueName + "Width", defaultWidth_mm.toString),
        (uniqueName + "Height", defaultHeight_mm.toString)
      )
    }
  }

  /**
    * List of beam parameters.
    *
    * @return Parameters for all beams.
    */
  def beamRowSeq =
    Seq(
      BeamRow(0, Config.SimpleRtplanBeamNameG000),
      BeamRow(90, Config.SimpleRtplanBeamNameG090),
      BeamRow(180, Config.SimpleRtplanBeamNameG180),
      BeamRow(270, Config.SimpleRtplanBeamNameG270)
    )

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

  private def makeWebForm() = new WebForm(pathOf, List(row1, row2, row3, row4) ++ beamRowSeq.map(b => b.makeRow()) ++ List(assignButtonList))

  private def formSelect(valueMap: ValueMapT, response: Response): Unit = {
    val form = makeWebForm()
    val beamDefaults = beamRowSeq.flatMap(b => b.defaultValueMap())
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val uniqueText = new SimpleDateFormat("yyyy-MM-dd").format(new Date)

    val defaultPatient = "$Simple_" + uniqueText

    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val planNameMap = if (empty(planName.label)) Map((planName.label, "Simple" + uniqueText.replace('-', ' '))) else emptyValueMap
    val toleranceTableMap = if (empty(toleranceTable.label)) Map((toleranceTable.label, defaultToleranceTableLabel)) else emptyValueMap

    val valMap = valueMap ++ beamDefaults ++ patientIdMap ++ patientNameMap ++ planNameMap ++ toleranceTableMap
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

    def parseDouble(text: String): Option[Double] = {
      try {
        val d = text.toDouble
        Some(d)
      } catch {
        case _: Throwable => None
      }
    }

    def checkMin(d: Double, label: String, name: String): StyleMapT = {
      if (d < 0)
        Error.make(label, "Negative values not allowed for " + name)
      else
        styleNone
    }

    def checkMax(d: Double, max: Double, label: String, name: String): StyleMapT = {
      if (d > max)
        Error.make(label, "Given value of " + d + " is above the limit of " + max + " mm for " + name + ".")
      else
        styleNone
    }

    def validateWidth(label: String): StyleMapT = {
      Trace.trace(label)
      val value = valueMap(label)
      parseDouble(value) match {
        case Some(d) => checkMin(d, label, "width") ++ checkMax(d, maxWidth_mm, label, "width")
        case _       => Error.make(label, "Not a valid floating point number.")
      }
    }

    def validateHeight(label: String): StyleMapT = {
      Trace.trace(label)
      val value = valueMap(label)
      parseDouble(value) match {
        case Some(d) => checkMin(d, label, "height") ++ checkMax(d, maxHeight_mm, label, "height")
        case _       => Error.make(label, "Not a valid floating point number.")
      }
    }

    def validateBeamParameters(beamRow: BeamRow): StyleMapT = {
      validateWidth(beamRow.makeWidth().label) ++ validateHeight(beamRow.makeHeight().label)
    }

    val errorList = beamRowSeq.foldLeft(styleNone)((s, b) => s ++ validateBeamParameters(b))
    errorList
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

  private def showDownload(modifiedPlan: ModifiedPlan, valueMap: ValueMapT, response: Response): Unit = {

    val downloadUrl = pathOf + "?download"

    val downloadLink = new WebPlainText("Download", false, 3, 0, _ => { <h4> <a href={downloadUrl} title="Click to download DICOM RTPLAN file(s).">Download</a></h4> })



    val dicomViewHtml = {
      val elem = {
        <div>
          <pre title="DICOM meta-data">{WebUtil.nl + modifiedPlan.rtplanText}</pre>
        </div>
      }

      elem
    }

    val dicomView = new WebPlainText("Download", false, 10, 0, _ => dicomViewHtml)
    val form = makeWebForm()
    form.setFormResponse(valueMap, styleNone, "Download RTPLAN", response, Status.SUCCESS_OK)
  }

  private case class BeamReference(beam: AttributeList, fractionReference: AttributeList) {}

  private def createRtplan(valueMap: ValueMapT, response: Response): ModifiedPlan = {
    val makeRtPlan = new MakeRtPlan(
      PatientID = valueMap(patientID.label),
      PatientName = valueMap(patientName.label),
      machineName = valueMap(machineName.label),
      RTPlanLabel = valueMap(planName.label),
      ToleranceTableLabel = valueMap(toleranceTable.label),
      beamList = beamRowSeq.map(_.toBeamSpecification(valueMap)).sortBy(_.GantryAngle_deg)
    )

    val modifiedPlan = makeRtPlan.makeZipWithSupportingFiles()
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
        beamRowSeq.map(_.toText(valueMap)).mkString("\n")
    text
  }

  private def validateAndMakePlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap: StyleMapT = validate(valueMap)
    if (styleMap.isEmpty) {
      logger.info("Creating plan.  Parameters: \n" + valueMapToString(valueMap))
      val modifiedPlan = createRtplan(valueMap, response)
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
        case _ if buttonIs(valueMap, createButton) => validateAndMakePlan(valueMap, response)
        case _                                     => formSelect(valueMap, response) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
