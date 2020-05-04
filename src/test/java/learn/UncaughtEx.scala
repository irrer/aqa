package learn

/**
 * Failed experiment.  Could not get this to catch an out of memory/java heap space error.
 */

object UncaughtEx {

  def grabMem = {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.util.{ Failure, Success }
    val grabby = Future {
      try {
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler(2))
        Thread.sleep(1 * 1000)
        println("========================== getting grabby ==========================")

        val mem = scala.collection.mutable.ArrayBuffer[Int](2, 3, 5, 7, 11, 13, 17, 19)
        while (true) {
          Thread.sleep(20)
          println("mem.size: " + mem.size)
          mem ++= mem.take(mem.size / 3)
        }
      } catch {
        case t: Throwable => println("grabby caught: " + t)
      }
    }
  }

  /**
   * If there is an out of memory error then restart the service.
   */
  private class UncaughtExceptionHandler(id: Int) extends Thread.UncaughtExceptionHandler {
    override def uncaughtException(t: Thread, e: Throwable) = {
      // Use the simplest printing of information as possible.  More sophisticated
      // methods may fail because they might try to get more memory.
      println("id : " + id + " unexpected error in UncaughtExceptionHandler : " + e)
      e.printStackTrace
      if (e.isInstanceOf[OutOfMemoryError]) {
        println("Whoa!")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler(1))
      grabMem

      Thread.sleep(20 * 60 * 1000)
      println("Exiting")
      System.exit(0)
    } catch {
      case t: Throwable => println("main caught: " + t)
    }
  }

}
