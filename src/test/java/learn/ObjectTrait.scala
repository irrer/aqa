package learn

import edu.umro.ScalaUtil.Trace

object ObjectTrait {

  trait Trate {
    def defA = Trace.trace("defA")
    def defB = Trace.trace("defB")
    val valA = "valA"
    val valB = "vaB"

    def def1: String;
    def def2: String;
    val val1: String;
    val val2: String;
  }

  object Obj1 extends Trate {
    override def def1 = "def1"
    def def2 = "def2"
    override val val1 = "val1"
    val val2 = "val2"
  }

  object Runner {
    def runIt(r: Trate) = {
      Trace.trace(r.def1)
      Trace.trace(r.def2)
      Trace.trace(r.valA)
    }
  }

  def main(args: Array[String]): Unit = {
    Runner.runIt(Obj1)
  }

}