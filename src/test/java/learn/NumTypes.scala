package learn

object NumTypes {
  def main(args: Array[String]): Unit = {

    val n: Int = 7 * 11

    val formatN: String = s"%${n.toString.length + 1}d"

    def fmt(i: Int): String = formatN.format(i)
    def fmtL(l: Seq[Int]): String = l.map(fmt).mkString("  ")

    val sq = (0 until n).map(i => (i * i) % n).distinct.sorted

    val notSq = (0 until n).diff(sq).distinct.sorted

    println("   " + fmtL(sq))

    def doRow(i: Int): String = {
      val text = fmt(i) + fmtL(sq.map(sqI => (i * sqI) % n))
      text
    }

    println(notSq.map(doRow).mkString("\n"))

    println

    notSq.map (i => {
      val list = sq.map(s => (i * s) % n).sorted
      println("listX: " + fmtL(list))
    })

  }

}
