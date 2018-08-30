package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util

/**
 * Define the value found at a given point and which image it came from.
 */
case class SymmetryAndFlatnessPointEvaluated(safPoint: SymmetryAndFlatnessPoint, value: Double, beamName: String) extends Logging {

  override def toString = safPoint + "  value: " + value.formatted("%12.3f") + "  " + beamName
}
