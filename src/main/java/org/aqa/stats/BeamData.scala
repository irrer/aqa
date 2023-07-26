package org.aqa.stats

case class BeamData(header: Header, rowList: Seq[Row]) {

  //val institution: String = ???
  //val machine: String = ???
  //val procedure: String = ???
  //val beam: String = ???

  val urlColumn = header.columns.indexWhere(_.equals(AnUtil.TagUrl))

  private def makeColumn(index: Int): Option[Column] = {

    def colToNumeric(col: Int): Seq[Double] = {
      try {
        rowList.map(_.columnList(col).toDouble)
      } catch {
        case _: Throwable => Seq() // empty list means this is not a numeric column
      }
    }

    val list = colToNumeric(index)
    if (list.isEmpty)
      None
    else
      Some(Column(index = index, header.columns(index), list, this))

  }

  val numericColumnList: Seq[Column] = {
    val candidates = header.columns.indices.filterNot(i => AnUtil.ignoreColumnNameSet.contains(header.columns(i)))
    candidates.flatMap(makeColumn).toSeq
  }

  def stats: String = {
    val colList = rowList.head.columnList
    val procedure = colList(header.columns.indexWhere(_.equals(AnUtil.TagProcedure)))
    val mach = "%-7s".format(colList(AnUtil.IndexMachine))
    val beamName = rowList.head.beamName(header)
    val count = rowList.size

    s"\n$procedure    $mach    $beamName   Count: $count\n" +
      Column.header + "\n" +
      numericColumnList.filter(_.isAbnormal).map(_.toString).mkString("\n")
  }

}
