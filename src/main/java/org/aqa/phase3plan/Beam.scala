package org.aqa.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.phase2.MeasureTBLREdges.TBLR

/**
  * Encapsulate beam information.
  *
  * @param name Beam name
  * @param energy_MeV Photon energy in MeV.
  * @param isFFF True if this is an FFF beam.
  * @param isJaw True if all edges are defined by jaw.  If one or more edges are not jaw, then this is false.
  * @param colAngle_deg Collimator angle.
  * @param gantryAngle_deg Gantry angle in degrees.
  * @param prototype Prototype beam from RTPLAN used to construct the final beam.
  */
case class Beam(name: String, energy_MeV: Double, isFFF: Boolean, isJaw: Boolean, colAngle_deg: Int, gantryAngle_deg: Int, prototype: AttributeList, rectangle: Option[TBLR] = None) {


}
