package org.aqa.stats

case class BeamData(header: Header, rowList: Seq[Row]) {

  //val institution: String = ???
  //val machine: String = ???
  //val procedure: String = ???
  //val beam: String = ???

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
      Some(Column(index = index, header.columns(index), list))

  }

  val numericColumnList: Seq[Column] = {
    val candidates = header.columns.indices.filterNot(i => AnUtil.ignoreColumnNameSet.contains(header.columns(i)))
    candidates.flatMap(makeColumn).toSeq
  }

  def stats: String = {
    Column.header + "\n" +
      numericColumnList.map(_.toString).mkString("\n")
  }

}
