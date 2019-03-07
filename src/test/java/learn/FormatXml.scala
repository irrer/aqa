package learn

object FormatXml {
  def main(args: Array[String]): Unit = {
    println("starting")
    val stuff1 = {
      <div><span>hello</span></div>
    }

    val stuff2 = {
      <div>
        <span>hello</span>
      </div>
    }

    println("stuff1:\n====" + stuff1.toString + "====")
    println("stuff2:\n====" + stuff2.toString + "====")

    println("finished")
  }

}