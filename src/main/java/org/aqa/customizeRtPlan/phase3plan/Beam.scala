package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

/**
  * Encapsulate beam information.
  *
  * @param al Attribute list of beam extracted from RTPLAN.
  */
case class Beam(al: AttributeList) {

  /** Name of beam. */
  def beamName: String = al.get(TagByName.BeamName).getSingleStringValueOrEmptyString()

  /** Beam number in metadata. */
  def beamNumber: Int = al.get(TagByName.BeamNumber).getIntegerValues.head

  /** Beam energy in MeV. */
  def energy_MeV: Double = DicomUtil.findAllSingle(al, TagByName.NominalBeamEnergy).head.getDoubleValues.head

  /** True if this is an FFF beam. */
  def isFFF: Boolean = Util.isFFF(al)

  /** First collimator angle in fraction sequence. */
  def colAngle_deg: Double = Util.collimatorAngle(al)

  /** Collimator angle rounded to 90. */
  def colAngle_roundedDeg: Double = Util.angleRoundedTo90(Util.collimatorAngle(al))

  /** First gantry angle in fraction sequence. */
  def gantryAngle_deg: Double = Util.gantryAngle(al)

  override def toString: String = {
    val j0 = beamName + ""
    Trace.trace(j0)
    val j1 = energy_MeV + ""
    Trace.trace(j1)
    val j2 = isFFF + ""
    Trace.trace(j2)
    val j3 = colAngle_roundedDeg + ""
    Trace.trace(j3)
    val j4 = gantryAngle_deg + ""
    Trace.trace(j4)
    s"$beamName | $energy_MeV Mev | isFFF: $isFFF | col angle: $colAngle_roundedDeg  | gantry: $gantryAngle_deg "
  }

}
