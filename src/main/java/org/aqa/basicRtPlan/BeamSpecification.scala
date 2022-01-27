package org.aqa.basicRtPlan

case class BeamSpecification(
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
