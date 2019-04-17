package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import org.aqa.db.EPID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.AQA
import org.aqa.Config
import edu.umro.util.OpSys

/*
object Doc {
  private val path = new String((new Doc).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
  
  
  
}

class Doc extends Restlet with SubUrlAdmin with Logging {




  private def showServiceInfo(response: Response): Unit = {
    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            <h2>{ pageTitle }</h2>
          </div>
        </div>
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            { basicInfo }
          </div>
          <div class="col-md-4 col-md-offset-1">
            { showLogFileList }
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <p></p><br> </br>
            { configInfo }
          </div>
        </div>
      </div>
    }
    setResponse(wrapBody(content, pageTitle), response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      0 match {
        case _ if valueMap.contains(requestRestartLabel) => doConfirm(response)
        case _ if valueMap.contains(confirmRestartLabel) => restartService(response)
        case _ if valueMap.contains(waitForRestartLabel) => waitForRestart(response)
        case _ if showFileContents(valueMap, response) => {}
        case _ => showServiceInfo(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
*/