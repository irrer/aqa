package org.aqa

import edu.umro.ScalaUtil.Trace

import java.lang.management.ManagementFactory
import java.lang.management.ThreadInfo

object ThreadMonitor extends Logging {

  /** List of threads that have already been logged. */
  private val threadIdSet = scala.collection.mutable.Set[Long]()

  /** Previous number of threads. */
  private var threadCount = 0

  /**
    * Log information on any new threads.
    *
    * This is a diagnostic tool to help determine why, in some situations, many threads are created by
    * showing where they originated from.
    */
  def logNewThreads(): Unit = {
    val threadBean = ManagementFactory.getThreadMXBean
    val threadIdList = threadBean.getAllThreadIds
    val threadInfoList = {
      val til =
        if ((!threadBean.isObjectMonitorUsageSupported) || (!threadBean.isSynchronizerUsageSupported))
          threadBean.getThreadInfo(threadIdList)
        else
          threadBean.getThreadInfo(threadIdList, true, true)
      // do not print nulls or those that have already been printed
      til.filterNot(_ == null).filterNot(ti => threadIdSet.contains(ti.getThreadId))
    }

    val newIdList = threadInfoList.map(ti => ti.getThreadId).toIterable

    newIdList.foreach(threadIdSet.add)

    def show(ti: ThreadInfo): String = {
      val stackText = ti.getStackTrace.mkString("\n    ")

      def has(pattern: String): String = {
        "    " + pattern + ": " + { if (stackText.contains(pattern)) "1" else "0" }
      }

      val text = "---------------------------------------------------------------\n" +
        "ThreadId: " + ti.getThreadId +
        "    state: " + ti.getThreadState +
        has("org.aqa") +
        has("slick") +
        has("restlet") +
        "\n    " + stackText
      text
    }

    if (threadInfoList.nonEmpty)
    Trace.trace(threadInfoList.sortBy(_.getThreadId).map(show).mkString("\n\n"))
  }

  /**
    * Periodically print the number of threads to the log.
    *
    * @param interval_ms Print it this often.
    */
  def monitorThreads(interval_ms: Long): Unit = {
    class MonitorThreads extends Runnable {
      override def run(): Unit = {
        while (true) {
          val numberOfThreads = Thread.getAllStackTraces.keySet().size()
          if (numberOfThreads != threadCount) {
            logger.info("Number of threads: " + numberOfThreads)
            logNewThreads()
            threadCount = numberOfThreads
          }
          Thread.sleep(interval_ms)
        }
      }
      new Thread(this).start()
    }

    new MonitorThreads
  }

}
