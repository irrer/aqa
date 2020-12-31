package learn

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

object ScalaFuturesAndExceptions extends App {

  try {

    val startTime = System.currentTimeMillis

    val f1 = Future {
      Thread.sleep(2000)
      1
    }
    val f2 = Future {
      Thread.sleep(550)
      throw new Exception("Ka-boom!")
      2
    }
    val f3 = Future {
      Thread.sleep(1000)
      3
    }

    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield (r1 + r2 + r3)

    result.onComplete {
      case Success(x) => {
        // the code won't come here
        println(s"\nresult = $x")
      }
      case Failure(e) => {
        // the code comes here because of the intentional exception
        val finishTime = currentTime
        val delta = finishTime - startTime
        System.err.println(s"delta = $delta")
        System.err.println("Failure happened!")
        // just a short message; i don't care about the full exception
        System.err.println("Got exception: " + e.getMessage)
      }
    }

    // important for a little parallel demo: keep the main
    // thread of the jvm alive
    sleep(4000)

    def sleep(time: Long) = Thread.sleep(time)
    def currentTime = System.currentTimeMillis()

  } catch {
    case t: Throwable => {
      println("Badness caught")
      t.printStackTrace
    }

  }

}
