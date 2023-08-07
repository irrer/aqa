package org.aqa.stats

case class Row(columnList: Seq[String]) {
  // def procedure(header: Header): String = columnList(header.procedureColumnIndex)

  def machine(header: Header): String = columnList(header.machineColumnIndex)

  def beamName(header: Header): String =
    columnList(header.beamNameColumnIndex)
}
