package org.aqa.stats

case class BeamData(header: Header, rowList: Seq[Row]) {

  def beamName = rowList.head.beamName(header).replace(' ', '_')

  def beamNameNoBlanks = beamName.replace(' ', '_')

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

  private def colList = rowList.head.columnList
  private def procedure = colList(header.columns.indexWhere(_.equals(AnUtil.TagProcedure)))
  def mach = colList(AnUtil.IndexMachine)
  val machText = "%-7s".format(mach)

  private def prefix = {
    val width = 30
    machText + "    " + s"%${width}s".format(beamNameNoBlanks.take(width)) + "    "
  }

  def stats: String = {
    val count = rowList.size

    s"\n$procedure    $machText    $beamName   Count: $count\n" +
      s"%${prefix.length}s".format(" ") + Column.header + "\n" +
      numericColumnList.filter(_.isOfInterest).map(c => prefix + c.toString).mkString("\n")
  }

}
