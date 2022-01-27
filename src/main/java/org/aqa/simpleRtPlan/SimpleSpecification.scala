package org.aqa.simpleRtPlan

/** Represent the specifications given by the user that can be used to modify the beams in the RTPLAN. */
case class SimpleSpecification(
    GantryAngle_deg: Double,
    BeamName: String,
    NominalBeamEnergy: Double,
    X_mm: Double,
    Y_mm: Double
) {
  override def toString: String = {
    BeamName + " ==> " +
      "Gantry: " + GantryAngle_deg + "    " +
      "Energy: " + NominalBeamEnergy + "    " +
      "X_mm, Y_mm: " + X_mm + ", " + Y_mm
  }
}
