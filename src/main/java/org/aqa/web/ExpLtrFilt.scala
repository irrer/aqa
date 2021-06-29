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

package org.aqa.web

import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.Context
import java.util.Date
import org.restlet.routing.Filter

object ExpLtrFilt {
  val path = "/UserList"

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class ExpLtrFilt(context: Context, interval: Long) extends Filter(context) {

  private def setExpiration(response: Response): Unit = {
    val expirationDate = new Date(System.currentTimeMillis + interval)
    println("expirationDate: " + expirationDate) // TODO rm
    val ent = response.getEntity // TODO rm
    if (ent == null) // TODO rm
      println("badness") // TODO rm
    if (response.getEntity != null) response.getEntity.setExpirationDate(expirationDate)
  }

  override def beforeHandle(request: Request, response: Response): Int = {
    setExpiration(response)
    Filter.CONTINUE
  }

  override def afterHandle(request: Request, response: Response): Unit = {
    setExpiration(response)
  }
}
