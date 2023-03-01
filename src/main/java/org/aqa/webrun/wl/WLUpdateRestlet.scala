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

package org.aqa.webrun.wl

import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.webrun.wl.WLUpdateRestlet.getLatestWLUpdate
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.text.SimpleDateFormat
import java.util.Date

/**
  * Support for generating JS scripts on request.
  */
object WLUpdateRestlet extends Logging {
  private val path = new String((new WLUpdateRestlet).pathOf)

  private var latestWLUpdate: String = "No activity since server started."

  private val dateFormat = new SimpleDateFormat("EEE dd MMM YYYY  h:mm:ss.SSS aa")

  /**
    * Indicate that there is new or updated Winston Lutz content.
    */
  def updateWL(): Unit = {
    WLUpdateRestlet.latestWLUpdate.synchronized(latestWLUpdate = dateFormat.format(new Date))
    logger.info("Updated Winston Lutz latest change: " + latestWLUpdate)
  }

  /**
    * Get the latest update time for Winston Lutz.
    *
    * This is done in a thread safe manner.
    *
    * @return Latest Winston Lutz time.
    */
  def getLatestWLUpdate: String = {
    WLUpdateRestlet.latestWLUpdate.synchronized(latestWLUpdate)
  }

  private val updateTime_ms = 5 * 1000

  /**
    * Make javascript that checks for a WL update. This is to be loaded into the web page.
    * @return
    */
  def makeJS: String = {
    val tag = "@@STATE@@"
    val script = s"""
      |
      |    // Reload the page when there is new data, indicated by
      |    // a change in latestWLUpdate.
      |
      |    var latestWLUpdate='$tag';
      |    var baseUrl='$path';
      |    var WebRefreshTime=$updateTime_ms;
      |
      |    function watchStatus() {
      |        $$.ajax({url:baseUrl,
      |            success:function(result){
      |                if (latestWLUpdate == result) {
      |                    setTimeout(watchStatus, WebRefreshTime);
      |                }
      |                else {
      |                    window.location = window.location.href;
      |                    location.reload();
      |                }
      |            },
      |            error:function(result){
      |                setTimeout(watchStatus, WebRefreshTime);
      |            }
      |        });
      |    }
      |
      |    setTimeout(watchStatus, WebRefreshTime);
      |
      |""".stripMargin.replace(tag, getLatestWLUpdate)
    script
  }

}

class WLUpdateRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      // super.handle(request, response)
      val text = getLatestWLUpdate
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(text, MediaType.TEXT_PLAIN)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
