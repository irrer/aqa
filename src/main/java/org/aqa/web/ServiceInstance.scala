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

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import play.api.libs.json._
import org.restlet.data.Status
import org.aqa.AQA
import org.restlet.data.MediaType
import org.aqa.web.WebUtil._

object ServiceInstance {
  private val path = new String((new ServiceInfo).pathOf)
}

class ServiceInstance extends Restlet with SubUrlAdmin {

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    try {
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(AQA.serviceStartTime.toString, MediaType.TEXT_PLAIN)
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
