
import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object ParIt {

  def main(args: Array[String]): Unit = {

    val rand = new Random

    def rr(i: Int): Int = {
      val ms = rand.nextInt(1000)
      println("start  i: " + i + "    ms: " + ms)
      Thread.sleep(ms)
      println("finish i: " + i + "    ms: " + ms)
      i
    }

    val seq = Seq(() => rr(1), () => rr(2), () => rr(3), () => rr(4), () => rr(5))

    val text = seq.par.map(f => f()).toSeq.mkString("  ")
    println("text: " + text)

  }

}