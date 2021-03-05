package org.aqa.webrun.dailyQA

import java.text.SimpleDateFormat

/**
 * Keep track of when the last change was to DailyQA data.  This is used to know
 * when the DailyQA web page should be updated.
 *
 * Technically this flag should be one per institution as opposed to one for all
 * institutions, but it is doubtful that the extra updates will ever be a problem.
 */
object DailyQAActivity {

  /** The time when DailyQA data changed. */
  private var latest: Long = 0


  /** Put date into a human friendly format. */
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")


  /**
   * Get a string indicating the last time a DailyQA value changed.
   *
   * @return Date and time string.
   */
  def get: String = latest.synchronized({
    val text = dateFormat.format(latest)
    text
  })


  /**
   * Change to reflect that an update has happened.
   */
  def update(): Unit = latest.synchronized({
    //noinspection LoopVariableNotUpdated
    while (latest == System.currentTimeMillis()) {
      Thread.sleep(10)
    }
    latest = System.currentTimeMillis()
  })

}
