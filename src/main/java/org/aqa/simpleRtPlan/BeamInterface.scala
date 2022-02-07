package org.aqa.simpleRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
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
import org.aqa.web.WebUtil.styleNone
import org.restlet.Response

import scala.annotation.tailrec

case class BeamInterface(rtplan: AttributeList, beamAl: AttributeList) {

  private val templateBeamName = beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString()
  private val beamNumber: Int = beamAl.get(TagByName.BeamNumber).getIntegerValues.head
  private val prefix = beamNumber.formatted("%02d") + ":: "
  private def makeColLabel(colName: String) = prefix + colName

  private def energySelectList() = {
    def makeSelectList(response: Option[Response]): List[(String, String)] = {
      List(("6", "6X"), ("16", "16X"))
    }
    new WebInputSelect(label = makeColLabel("Energy"), showLabel = false, col = 1, offset = 0, selectList = makeSelectList _, aqaAlias = false)
  }

  private def gantryAngleSelectList() = {
    def makeSelectList(response: Option[Response]): List[(String, String)] = {
      List(("0", "0"), ("90", "90"), ("180", "180"), ("270", "270"))
    }
    new WebInputSelect(label = makeColLabel("Gantry Rtn [deg]"), showLabel = false, col = 1, offset = 0, selectList = makeSelectList _, aqaAlias = false)
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
    val attr = beamRef.get(TagByName.BeamMeterset)
    (attr != null) && (attr.getIntegerValues.head > 0)
  }

  //noinspection TypeAnnotation
  object EntryType extends Enumeration {
    val Input = Value // user can enter arbitrary text
    val Energy = Value // from a select list
    val GantryAngle = Value // gantry angle
    val Display = Value // user not can enter data, for display only
    val DisplayBold = Value // user not can enter data, for display only, in bold
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

    val label: String = makeColLabel(name)

    def field: IsInput with ToHtml = {
      entryType match {
        case EntryType.DisplayBold => new WebPlainText(label = label, showLabel = false, col = 1, offset = 0, _ => <b>{init()}</b>)
        case EntryType.Display     => new WebPlainText(label = label, showLabel = false, col = 1, offset = 0, _ => <span id={label}>{init()}</span>)
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
    val pos = positions(index)
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
  private def initJawSize(jawType: Seq[String]): String = {
    (jawDim(index = 1, jawType) + jawDim(index = 0, jawType)).toString
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
    simpleBeamSpecification.copy(X2_mm = valueMap(col.label).toDouble * -10)
  }

  private def putDimY1(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(Y1_mm = valueMap(col.label).toDouble * 10)
  }

  private def putDimY2(valueMap: ValueMapT, simpleBeamSpecification: SimpleBeamSpecification, col: Col): SimpleBeamSpecification = {
    simpleBeamSpecification.copy(Y2_mm = valueMap(col.label).toDouble * -10)
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
    val tolTableToValidate = valueMap(makeColLabel(toleranceColName)).trim
    if (tolTableToValidate.isEmpty) {
      Error.make(makeColLabel(toleranceColName), "Tolerance table can not be empty.")
    } else
      styleNone
  }

  private val tolTable = Col(
    toleranceColName,
    if (isTreat) Input else Display,
    init = () => DicomUtil.findAllSingle(rtplan, TagByName.ToleranceTableLabel).head.getSingleStringValueOrEmptyString(),
    validate = validateToleranceTable
  )

  val colList: Seq[Col] =
    Seq(
      Col("Field Order/Type", DisplayBold, init = () => { beamNumber + " / " + { if (isTreat) "Treat" else "Setup" } }),
      // Col("Field ID", if (isTreat) Text else Display, init = () => templateBeamName, validate = validLO),
      Col("Field Name", if (isTreat) Input else Display, init = () => templateBeamName, validate = validBeamName),
      Col("Technique", Display, init = () => beamAl.get(TagByName.BeamType).getSingleStringValueOrEmptyString),
      Col("Scale", Display, init = () => "Varian IEC"),
      Col("Energy", if (isTreat) Energy else Display, init = () => beamDbl(TagByName.NominalBeamEnergy).round + "X"),
      Col("Dose Rate [MU/min] ", Display, init = () => beamDblS(TagByName.DoseRateSet)),
      Col("MU", if (isTreat) Input else Display, init = () => if (isTreat) beamRefDblS(TagByName.BeamMeterset) else "", validate = validateNonNegDouble, put = putMU),
      // Col("Dose to Beam Dose Point", Display, init = ???),
      // Col("Dose to CBCT SIM", Display, init = ???),
      Col(
        "Backup Timer [min]",
        if (isTreat) Input else Display,
        init = () => if (isTreat) Util.fmtDbl(beamRefDbl(TagByName.BeamDeliveryDurationLimit) / 60) else "",
        validate = validateNonNegDouble,
        put = (v: ValueMapT, sbs: SimpleBeamSpecification, col: Col) => sbs.copy(BeamDeliveryDurationLimit_sec = v(col.label).toDouble * 60) // convert from minutes to seconds
      ),
      tolTable,
      // Col("Calculated SSD [cm]", Display, init = ???),
      // Col("Planned SSD [cm]", Display, init = ???),
      Col("Gantry Rtn [deg]", if (isTreat) GantryAngle else Display, init = () => beamDbl(TagByName.GantryAngle).round.toString, put = putGantryAngle),
      Col("Coll Rtn [deg]", Display, init = () => beamDblS(TagByName.BeamLimitingDeviceAngle)),
      //
      Col("Field X [cm]", Display, init = () => initJawSize(typeX)),
      Col("X1 [cm]", if (isTreat) Input else Display, init = () => jawDim(index = 0, jawType = typeX).toString, validate = validateJaw, put = putDimX1),
      Col("X2 [cm]", if (isTreat) Input else Display, init = () => jawDim(index = 1, jawType = typeX).toString, validate = validateJaw, put = putDimX2),
      //
      Col("Field Y [cm]", Display, init = () => initJawSize(typeY)),
      Col("Y1 [cm]", if (isTreat) Input else Display, init = () => jawDim(index = 0, jawType = typeY).toString, validate = validateJaw, put = putDimY1),
      Col("Y2 [cm]", if (isTreat) Input else Display, init = () => jawDim(index = 1, jawType = typeY).toString, validate = validateJaw, put = putDimY2),
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
  def initialValueMap(): ValueMapT = {
    val vm = colList.filter(_.entryType.toString.equals(EntryType.Input.toString)).map(f => (f.label, f.init())).toMap
    vm
  }

}
