package org.aqa.simpleRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.Error
import org.aqa.web.WebUtil.IsInput
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ToHtml
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.WebInputSelect
import org.aqa.web.WebUtil.WebInputText
import org.aqa.web.WebUtil.WebPlainText
import org.aqa.web.WebUtil.emptyValueMap
import org.aqa.web.WebUtil.styleNone
import org.restlet.Response

import scala.annotation.tailrec

case class BeamInterface(rtplan: AttributeList, beamAl: AttributeList) extends Logging {

  private val templateBeamName = beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString()
  private val beamNumber: Int = beamAl.get(TagByName.BeamNumber).getIntegerValues.head
  private val prefix = beamNumber.formatted("%02d") + ":: "
  private def makeColLabel(colName: String) = prefix + colName

  val labelMU = "MU"

  /** If the MU is this large or larger, then warn the user. */
  val MUWarnLimit = 400

  /** If the MU is this large or larger, then reject the plan. */
  val MULimit = 1000

  val labelDoseRate = "Dose Rate [MU/min]"
  val labelBackupTimer = "Backup Timer [min]"
  val labelFieldX = "Field X [cm]"
  val labelX1 = "X1 [cm]"
  val labelX2 = "X2 [cm]"

  val labelFieldY = "Field Y [cm]"
  val labelY1 = "Y1 [cm]"
  val labelY2 = "Y2 [cm]"

  val labelEnergy = "Energy"

  private def energySelectList() = {
    //noinspection ScalaUnusedSymbol
    def makeSelectList(response: Option[Response]): List[(String, String)] = {
      List(("6", "6X"), ("16", "16X"))
    }
    new WebInputSelect(label = makeColLabel(labelEnergy), showLabel = false, col = 1, offset = 0, selectList = makeSelectList, aqaAlias = false)
  }

  private def gantryAngleSelectList() = {
    //noinspection ScalaUnusedSymbol
    def makeSelectList(response: Option[Response]): List[(String, String)] = {
      List(("0", "0"), ("90", "90"), ("180", "180"), ("270", "270"))
    }
    new WebInputSelect(label = makeColLabel("Gantry Rtn [deg]"), showLabel = false, col = 1, offset = 0, selectList = makeSelectList, aqaAlias = false)
  }

  /** Default beam treatment limit is seconds. */
  private val defaultBeamDeliveryDurationLimit = 0.6

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
    // val attr = beamRef.get(TagByName.BeamMeterset)
    // (attr != null) && (attr.getIntegerValues.head > 0)
    DicomUtil.findAllSingle(beamAl, TagByName.TreatmentDeliveryType) match {
      case list if list.nonEmpty => list.head.getSingleStringValueOrEmptyString().trim.equalsIgnoreCase("TREATMENT")
      case _                     => false
    }
  }

  //noinspection TypeAnnotation
  object EntryType extends Enumeration {
    val Input = Value // user can enter arbitrary text
    val Energy = Value // from a select list
    val GantryAngle = Value // gantry angle
    val Display = Value // user not can enter data, for display only
    val DisplayBold = Value // user not can enter data, for display only, in bold
  }

  //def noValidation(valueMap: ValueMapT, col: Col): StyleMapT = styleNone

  //def noPut(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = simpleBeamSpecification

  case class Col(
      name: String,
      entryType: EntryType.Value,
      init: ValueMapT => String,
      //validate: (ValueMapT, Col) => StyleMapT = noValidation,
      validate: (ValueMapT, Col) => StyleMapT = (_, _) => styleNone,
      put: (ValueMapT, SimpleBeamSpecification, Col) => SimpleBeamSpecification = (_, simpleBeamSpecification: SimpleBeamSpecification, _) => simpleBeamSpecification
  ) {

    val label: String = makeColLabel(name)

    def field(valueMap: ValueMapT): IsInput with ToHtml = {
      entryType match {
        case EntryType.DisplayBold => new WebPlainText(label = label, showLabel = false, col = 1, offset = 0, _ => <b> {init(valueMap)} </b>)
        case EntryType.Display     => new WebPlainText(label = label, showLabel = false, col = 1, offset = 0, _ => <span id={label}>{init(valueMap)}</span>)
        case EntryType.Energy      => energySelectList()
        case EntryType.GantryAngle => gantryAngleSelectList()
        case _                     => new WebInputText(label = label, showLabel = false, col = 1, offset = 0, placeholder = "", aqaAlias = false)
      }
    }
  }

  import EntryType._

  private def beamList(tag: AttributeTag): Seq[Attribute] = DicomUtil.findAllSingle(beamAl, tag)

  private def beamDbl(tag: AttributeTag): Double = {
    beamList(tag).head.getDoubleValues.head
  }

  private def beamDblS(tag: AttributeTag): String = beamDbl(tag).toString

  val typeX = Seq("X", "ASYMX")
  val typeY = Seq("Y", "ASYMY")

  private def jawDim(index: Int, jawType: Seq[String]): Double = {
    val list = beamList(TagByName.BeamLimitingDevicePositionSequence)
      .flatMap(s => DicomUtil.alOfSeq(s.asInstanceOf[SequenceAttribute]))
      .find(al => jawType.contains(al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString))
      .get
    val positions = list.get(TagByName.LeafJawPositions).getDoubleValues
    val pos = positions(index) / 10.0
    index match {
      case 0 => -pos
      case 1 => pos
    }
  }

  /**
    * Get the jaw opening size for either X or Y.
    * @param jawType List of types of jow we are looking for.
    * @return String representing jaw opening size.
    */
  private def initJawSize(valueMap: ValueMapT, d1Label: String, d2Label: String, jawType: Seq[String]): String = {
    val default = (jawDim(index = 1, jawType) + jawDim(index = 0, jawType)).toString
    (valueMap.get(makeColLabel(d1Label)), valueMap.get(makeColLabel(d2Label))) match {
      case (Some(textX1), Some(textX2)) =>
        try {
          val text = (textX1.toDouble + textX2.toDouble).toString
          text
        } catch {
          case _: Throwable => default
        }
      case _ => default
    }

  }

  //noinspection SameParameterValue
  private def validateJaw(valueMap: ValueMapT, col: Col, commonName: String, min: Double, max: Double): StyleMapT = {
    val text = valueMap(col.label).trim
    0 match {
      case _ if WebUtil.stringToDouble(text).isEmpty   => Error.make(col.label, commonName + " must be a valid floating point number.")
      case _ if WebUtil.stringToDouble(text).get < min => Error.make(col.label, commonName + " value less than " + min + " not allowed.")
      case _ if WebUtil.stringToDouble(text).get > max => Error.make(col.label, commonName + " value greater than " + max + " not allowed.")
      case _                                           => styleNone
    }
  }

  /**
    * Validate the second jaw (X2 or Y2). Make sure that it is a valid number, is
    * in range, and does not collide with its opposing jaw.
    * @param valueMap All user entered values.
    * @param d2Col Column for X2 or Y2.
    * @param d2CommonName Name that user will recognize.
    * @param min Minimum allowed value.
    * @param max Maximum allowed value.
    * @param d1Name Name of opposing (X1 or Y1) jaw.
    * @return Error if something is wrong, empty if all is ok.
    */
  //noinspection SameParameterValue
  private def validateJawPair(valueMap: ValueMapT, d2Col: Col, d2CommonName: String, min: Double, max: Double, d1Name: String): StyleMapT = {
    validateJaw(valueMap, d2Col, d2CommonName, min, max) match {
      case err if err.nonEmpty =>
        err
      case _ =>
        val d2 = valueMap(d2Col.label).trim.toDouble
        val d1 = valueMap(makeColLabel(d1Name)).trim.toDouble
        if ((d2 + d1) < 0)
          Error.make(d2Col.label, d2CommonName + " would collide with opposing jaw.")
        else
          styleNone
    }
  }

  private def validateSSD(valueMap: ValueMapT, col: Col): StyleMapT = {
    val min = 50
    val max = 100
    val text = valueMap(col.label).trim
    val result = 0 match {
      case _ if WebUtil.stringToDouble(text).isEmpty   => Error.make(col.label, col.name + " must be a valid floating point number.")
      case _ if WebUtil.stringToDouble(text).get < min => Error.make(col.label, col.name + " must be " + min + " or greater.")
      case _ if WebUtil.stringToDouble(text).get > max => Error.make(col.label, col.name + " must be " + max + " or less.")
      case _                                           => styleNone
    }
    result
  }

  private def validateJawX1(valueMap: ValueMapT, col: Col): StyleMapT = validateJaw(valueMap, col, commonName = "X1", min = -10, max = 20)
  private def validateJawX2(valueMap: ValueMapT, col: Col): StyleMapT = {
    validateJawPair(valueMap, col, d2CommonName = "X2", min = -2, max = 20, labelX1)
  }

  private def validateJawY1(valueMap: ValueMapT, col: Col): StyleMapT = validateJaw(valueMap, col, commonName = "Y1", min = -10, max = 20)
  private def validateJawY2(valueMap: ValueMapT, col: Col): StyleMapT = {
    validateJawPair(valueMap, col, d2CommonName = "Y2", min = -10, max = 20, labelY1)
  }

  private def putEnergy(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    val e = valueMap(col.label).toDouble
    simpleBeamSpecification.copy(NominalBeamEnergy = e)
  }

  private def putMU(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(BeamMeterset = valueMap(col.label).toDouble)
  }

  private def putSSD(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(SourceToSurfaceDistance = valueMap(col.label).toDouble * 10)
  }

  private def putDimX1(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(X1_mm = valueMap(col.label).toDouble * -10)
  }

  private def putDimX2(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(X2_mm = valueMap(col.label).toDouble * 10)
  }

  private def putDimY1(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(Y1_mm = valueMap(col.label).toDouble * -10)
  }

  private def putDimY2(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(Y2_mm = valueMap(col.label).toDouble * 10)
  }

  private val toleranceColName = "Tol. Table"

  /**
    * The the beam's tolerance table.
    * @param valueMap What the user entered.
    * @return The name of the tolerance table.
    */
  def getToleranceTable(valueMap: ValueMapT): String = {
    valueMap(makeColLabel(toleranceColName))
  }

  /**
    * Validate the user entered tolerance table.
    *
    * @param valueMap All user entered values.
    * @param col Column to validate
    * @return Empty list on error, or error description on error.
    */
  private def validateToleranceTable(valueMap: ValueMapT, col: Col): StyleMapT = {
    if (isTreat) {
      val tolTableToValidate = valueMap(makeColLabel(toleranceColName)).trim
      if (tolTableToValidate.isEmpty) {
        Error.make(makeColLabel(toleranceColName), "Tolerance table can not be empty.")
      } else
        styleNone
    } else
      styleNone
  }

  //noinspection ScalaUnusedSymbol
  private def validateMU(valueMap: ValueMapT, col: Col): StyleMapT = {
    val value = valueMap(col.label)
    def err(msg: String): StyleMapT = Error.make(col.label, msg)

    WebUtil.stringToDouble(value.trim) match {
      case Some(d) if d < 0        => err("MU must be 0 or greater, not negative.")
      case Some(d) if d >= MULimit => err("MU must be less than 1000.")
      case Some(d)                 => styleNone
      case _                       => err("MU must be valid floating point from 0 to 999")
    }
  }

  private def getToleranceTableLabel: String = {
    DicomUtil.findAllSingle(rtplan, TagByName.ToleranceTableLabel).head.getSingleStringValueOrEmptyString()
  }

  /**
    * Generate tolerance table column.
    *
    * Currently this is not in use but may be implemented in the
    * future to allow users to select a different tolerance table.
    */
  //noinspection ScalaUnusedSymbol
  private val tolTable = Col(
    toleranceColName,
    if (isTreat) Input else Display,
    init = _ => DicomUtil.findAllSingle(rtplan, TagByName.ToleranceTableLabel).head.getSingleStringValueOrEmptyString(),
    validate = validateToleranceTable
  )

  private def fetchBackupTimer(valueMap: ValueMapT): Double = {
    val text =
      "\n-----\n" +
        "beamAl BeamNumber: " + beamAl.get(TagByName.BeamNumber).getSingleStringValueOrEmptyString() + "\n" +
        "beamAl BeamName: " + beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString() + "\n" +
        "beamRef:\n" + DicomUtil.attributeListToString(beamRef) + "\n"
    Trace.trace(text)

    val DoseRateSet = DicomUtil.findAllSingle(beamAl, TagByName.DoseRateSet).head.getDoubleValues.head
    val BeamMeterset = DicomUtil.findAllSingle(beamRef, TagByName.BeamMeterset).head.getDoubleValues.head

    val mu = valueMap.get(makeColLabel(labelMU))

    val maxTime = if (mu.isDefined) {
      (mu.get.toDouble / DoseRateSet) + 0.1
    } else {
      (BeamMeterset / DoseRateSet) + 0.1
    }

    logger.info("Using " + labelBackupTimer + " of " + maxTime + " minutes")
    maxTime
  }

  private def initEnergy(valueMap: ValueMapT): String = {
    val label = makeColLabel(labelEnergy)
    valueMap.get(label) match {
      case Some(text) => text
      case _          => beamDbl(TagByName.NominalBeamEnergy).round + "X"
    }
  }

  private def initMU(valueMap: ValueMapT): String = {
    if (isTreat) {
      val label = makeColLabel(labelMU)
      valueMap.get(label) match {
        case Some(text) => text
        case _ =>
          DicomUtil.findAllSingle(beamRef, TagByName.BeamMeterset).head.getDoubleValues.head.toString
      }
    } else {
      ""
    }
  }

  val colList: Seq[Col] =
    Seq(
      Col("Field Order/Type", DisplayBold, init = _ => { beamNumber + " / " + { if (isTreat) "Treat" else "Setup" } }),
      // Col("Field ID", if (isTreat) Text else Display, init = () => templateBeamName, validate = validLO),
      Col("Field Name", Display, init = _ => templateBeamName),
      Col("Technique", Display, init = _ => beamAl.get(TagByName.BeamType).getSingleStringValueOrEmptyString),
      Col("Scale", Display, init = _ => "Varian IEC"),
      Col(labelEnergy, if (isTreat) Energy else Display, init = initEnergy, put = putEnergy),
      Col(labelDoseRate, Display, init = _ => beamDblS(TagByName.DoseRateSet)),
      Col(labelMU, if (isTreat) Input else Display, init = initMU, validate = validateMU, put = putMU),
      Col(
        labelBackupTimer,
        Display,
        init = valueMap => if (isTreat) Util.fmtDbl(fetchBackupTimer(valueMap)) else "",
        put = (valueMap: ValueMapT, sbs: SimpleBeamSpecification, _: Col) => sbs.copy(MaximumTreatmentTime_min = fetchBackupTimer(valueMap))
      ),
      Col(toleranceColName, Display, init = _ => getToleranceTableLabel), // tolTable, // for possible future use.
      Col("Calculated SSD [cm]", if (isTreat) Input else Display, init = _ => Util.fmtDbl(beamDbl(TagByName.SourceToSurfaceDistance) / 10), validate = validateSSD, put = putSSD),
      Col("Gantry Rtn [deg]", Display, init = _ => beamDbl(TagByName.GantryAngle).round.toString),
      Col("Coll Rtn [deg]", Display, init = _ => beamDblS(TagByName.BeamLimitingDeviceAngle)),
      //
      Col(labelFieldX, Display, init = valueMap => initJawSize(valueMap, labelX1, labelX2, typeX)),
      Col(labelX1, if (isTreat) Input else Display, init = _ => jawDim(index = 0, jawType = typeX).toString, validate = validateJawX1, put = putDimX1),
      Col(labelX2, if (isTreat) Input else Display, init = _ => jawDim(index = 1, jawType = typeX).toString, validate = validateJawX2, put = putDimX2),
      //
      Col(labelFieldY, Display, init = valueMap => initJawSize(valueMap, labelY1, labelY2, typeY)),
      Col(labelY1, if (isTreat) Input else Display, init = _ => jawDim(index = 0, jawType = typeY).toString, validate = validateJawY1, put = putDimY1),
      Col(labelY2, if (isTreat) Input else Display, init = _ => jawDim(index = 1, jawType = typeY).toString, validate = validateJawY2, put = putDimY2),
      //
      Col("Couch Vrt [cm]", Display, init = _ => (beamDbl(TagByName.TableTopVerticalPosition) / 10).toString),
      Col("Couch Lng [cm]", Display, init = _ => (beamDbl(TagByName.TableTopLongitudinalPosition) / 10).toString),
      Col("Couch Lat [cm]", Display, init = _ => (beamDbl(TagByName.TableTopLateralPosition) / 10).toString),
      Col("Couch Rtn [deg]", Display, init = _ => beamDblS(TagByName.TableTopEccentricAngle))
    )

  // Trace.trace(colList.size + "  col list: " + colList.map(_.name).mkString("\n"))

  /**
    * Fill in beam values.
    *
    * @param valueMap User entries.
    * @return A single beam specification.
    */
  def toBeamSpecification(valueMap: ValueMapT): SimpleBeamSpecification = {

    @tailrec
    def putList(sbs: SimpleBeamSpecification, cl: Seq[Col]): SimpleBeamSpecification = {
      if (cl.isEmpty) sbs
      else {
        putList(cl.head.put(valueMap, sbs, cl.head), cl.tail)
      }
    }

    val sbsInit = SimpleBeamSpecification(BeamNumber = beamNumber)
    val sbsFinal = putList(sbsInit, colList)
    sbsFinal
  }

  /**
    * Values to use as defaults when starting with a blank form.
    *
    * @return List of label+value pairs.
    */
  def initialValueMap(): ValueMapT = {
    val vm = colList.filter(_.entryType.toString.equals(EntryType.Input.toString)).map(f => (f.label, f.init(emptyValueMap))).toMap
    vm
  }

  def validateBeam(valueMap: ValueMapT): StyleMapT = {
    if (isTreat) {
      val list = colList.flatMap(c => c.validate(valueMap, c))
      list.toMap
    } else styleNone
  }

}
