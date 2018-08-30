package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util

/**
 * Define a configured point to be used for Symmetry and flatness.
 */
case class SymmetryAndFlatnessPoint(name: String, x_mm: Double, y_mm: Double) extends Logging {

  override def toString = name.formatted("%-12s") + "  x, y: " + x_mm.formatted("%7.1f") + ", " + y_mm.formatted("%7.1f")
}
