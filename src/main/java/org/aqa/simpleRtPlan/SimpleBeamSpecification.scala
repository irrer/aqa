package org.aqa.simpleRtPlan

/** Represent the specifications given by the user that can be used to modify one beam in the RTPLAN.
  * These values are in the units expected by DICOM, which are different from what the user sees.  For
  * example, X1 for the user is in cm, but DICOM is in mm.
  */
case class SimpleBeamSpecification(
    GantryAngle_deg: Double = -1,
    BeamName: String = "to be defined",
    BeamDose: Double = -1,
    BeamMeterset: Double = -1,
    BeamDeliveryDurationLimit_sec: Double = -1,
    DoseRateSet: Double = -1,
    NominalBeamEnergy: Double = -1,
    X1_mm: Double = -1,
    X2_mm: Double = -1,
    Y1_mm: Double = -1,
    Y2_mm: Double = -1
) {

  override def toString: String = {
    BeamName + " ==> " +
      "Gantry: " + GantryAngle_deg +
      "    BeamDose: " + BeamDose +
      "    DoseRateSet: " + DoseRateSet +
      "    NominalBeamEnergy: " + NominalBeamEnergy +
      "    BeamDeliveryDurationLimit_sec: " + BeamDeliveryDurationLimit_sec +
      "    X1,X2 mm: " + X1_mm + "," + X2_mm +
      "    Y1,Y2 mm: " + Y1_mm + "," + Y2_mm
  }
}
