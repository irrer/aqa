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
