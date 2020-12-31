
import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object Angles {

  def main(args: Array[String]): Unit = {

    def show(deg: Double) = {
      val rad = Math.toRadians(deg)
      val s = (Math.sin(rad) * 1000).round / 1000.0
      val c = (Math.cos(rad) * 1000).round / 1000.0
      println(deg.formatted("%8.1f") + "    rad: " + rad.formatted("%6.3f") + "    sin: " + s.formatted("%6.3f") + "    cos: " + c.formatted("%6.3f"))
    }

    for (deg <- -360 until 420 by 30) show(deg)

  }

}