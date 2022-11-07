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

package org.aqa.webrun.phase2.phase2csv

import org.aqa.Logging
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType

object Phase2CsvRestlet {
  private val restlet = new Phase2CsvRestlet
  val pathOf: String = restlet.pathOf
  val pathOfRegenerate: String = pathOf + "?regenerate=true"
}

class Phase2CsvRestlet extends Restlet with SubUrlRoot with Logging {
  override def handle(request: Request, response: Response): Unit = {
    try {

      super.handle(request, response)
      val valueMap = getValueMap(request)
      if (valueMap.contains("regenerate")) {
        val html = {
          <div>
            <center>
              <h2>CSV Re-generating</h2>
              Please wait for the CSV files to be re-genarerated, and then return to the <a href={this.pathOf}>download page</a>.
              <br/>
              Note that it may be necessary to refresh the download page with your browser's refresh button.
            </center>
          </div>
        }

        Phase2CsvMakeAll.makeAllInFuture(100)

        val text = WebUtil.wrapBody(html, "CSV Re-generating")
        response.setEntity(text, MediaType.TEXT_HTML)

      } else {
        val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
        val dir = Phase2Csv.institutionCsvDir(institutionPK)
        response.redirectSeeOther(WebServer.urlOfResultsFile(dir))
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
