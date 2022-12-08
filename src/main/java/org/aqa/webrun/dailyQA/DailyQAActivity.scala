/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.dailyQA

import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.text.SimpleDateFormat
import java.util.Date
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  /** Entries cached this long are considered invalid. */
  private val timeoutInterval_ms = 10 * 60 * 1000

  /**
    * Get a string indicating the last time a DailyQA value changed.
    *
    * @return Date and time string.
    */
  def get: String =
    latest.synchronized({
      // timeout if the contents are stale
      val timedOut = (latest + timeoutInterval_ms) < System.currentTimeMillis()
      if (timedOut)
        latest = System.currentTimeMillis
      val text = dateFormat.format(latest)
      text
    })

  private def keyOf(institutionPK: Long, rawDate: Date): String = {
    val day = Util.dateTimeToDate(rawDate)
    val key: String = institutionPK.toString + " : " + Util.formatDate(Util.standardDateFormat, day)
    key
  }

  private class CachedResult(val institutionPK: Long, rawDate: Date, val result: String) {
    val day: Date = Util.dateTimeToDate(rawDate)
    val key: String = keyOf(institutionPK, day)
    val cacheDate: Long = System.currentTimeMillis()

    /**
      * Determine if this entry is valid.
      * @return True if valid.
      */
    def isValid: Boolean = {
      (cacheDate + timeoutInterval_ms) > System.currentTimeMillis()
    }
  }

  private val cachedResultList = scala.collection.mutable.HashMap[String, CachedResult]()

  def putCache(institutionPK: Long, dataDate: Date, result: String): Unit = {
    Trace.trace()
    val cr = new CachedResult(institutionPK, dataDate, result)
    Trace.trace()
    cachedResultList.synchronized {
      Trace.trace()
      cachedResultList.put(cr.key, cr)
      Trace.trace()
    }
    Trace.trace()
  }

  def getCache(institutionPK: Long, dataDate: Date): Option[String] = {
    Trace.trace()
    val result = cachedResultList.synchronized {
      val date = Util.dateTimeToDate(new Date(dataDate.getTime))
      val key = keyOf(institutionPK, date)
      val cr = cachedResultList.get(key)
      if (cr.isDefined) {
        if (cr.get.isValid)
          Some(cr.get.result)
        else {
          val key = keyOf(institutionPK, dataDate)
          cachedResultList.remove(key)
          None
        }
      } else
        None
    }
    Trace.trace()
    result
  }

  private def clearCache(institutionPK: Long, rawDate: Date): Unit = {
    Trace.trace()
    cachedResultList.synchronized {
      val key = keyOf(institutionPK, rawDate)
      cachedResultList.remove(key)
    }
    Trace.trace()
  }

  /**
    * Change to reflect that an update has happened now.
    */
  private def updateNow(institutionPK: Long, dataDate: Date): Unit =
    cachedResultList.synchronized({
      //noinspection LoopVariableNotUpdated
      while (latest == System.currentTimeMillis()) {
        Thread.sleep(10)
      }
      clearCache(institutionPK, dataDate)
      latest = System.currentTimeMillis()
    })

  /**
    * Change to reflect that an update has happened after waiting the given amount of time.
    *
    * @param delay_ms Delay in ms to wait before updating.  0 or less means do it now.
    */
  def update(institutionPK: Long, dataDate: Date, delay_ms: Long = 0): Unit =
    if (delay_ms > 0) {
      Future {
        Thread.sleep(delay_ms)
        DailyQAActivity.updateNow(institutionPK, dataDate)
      }
    } else
      updateNow(institutionPK, dataDate)
}
