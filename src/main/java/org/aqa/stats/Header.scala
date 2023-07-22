package org.aqa.stats

case class Header(columns: Seq[String]) {
  val procedureColumnIndex: Int = columns.indexWhere(c => c.contains("Procedure"))
  val machineColumnIndex: Int = columns.indexWhere(c => c.contains("Machine"))
  val beamNameColumnIndex: Int = columns.indexWhere(c => c.contains("Beam_Name"))
}
