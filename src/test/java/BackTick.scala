
import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object BackTick {

  private class Thingy(id: Int) {
    val v = "me: " + id
    println("v: " + v)
  }

  def main(args: Array[String]): Unit = {

    lazy val lzy = new Thingy(5)
    lazy val j1 = new Thingy(1)
    lazy val j2 = new Thingy(2)
    lazy val j3 = new Thingy(3)

    j1 match {
      case `j2` => println("got j2")
      case `lzy` => println("got lzy")
      case `j3` => println("got j3")
      case `j1` => println("got j1")
      case _ => println("nuthin")
    }

  }

}