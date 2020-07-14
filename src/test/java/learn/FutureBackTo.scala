package learn

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import edu.umro.ScalaUtil.Trace

object FutureBackTo {

  def paramsRus(s: String = "boring", i: Int = 5, d: Double = 56.24, t: (Int, String) = (32, "thirty-two")) = {
    Trace.trace("s: " + s +
      "    i: " + i +
      "    d: " + d +
      "    t: " + t)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    def elapsed: String = (System.currentTimeMillis - start).formatted("%8d    ")

    paramsRus(d = 77.77, i = 8)

    val future = Future {
      Thread.sleep(1000)
      12
    }

    val timeout = Duration(1500, TimeUnit.MILLISECONDS)
    try {
      val res = Await.result(future, timeout)
      Trace.trace(res)
    } catch {
      case t: Throwable => {
        Trace.trace(t)
      }
    }

    Trace.trace

    println(elapsed + "exiting")
    System.exit(0)
  }

}
