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
import scala.concurrent.duration.DurationInt
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

object ServiceInfo {
  private val path = new String((new ServiceInfo).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class ServiceInfo extends Restlet with SubUrlAdmin with Logging {
  private val logFileTag = "LogFileVersion"
  private val pageTitle = "Service Information"

  private lazy val logDir: File = {
    val logFileName: String = {
      Util.buildProperties.getProperty("wrapper.logfile") match {
        case name: String if (name != null) => name
        case _ => """C:\Program Files\AQA\logging""" // Assume a reasonable default
      }
    }
    new File(logFileName).getParentFile
  }

  private def allFiles: Seq[File] = {
    try {
      Util.listDirFiles(logDir).filter(f => !f.getName.contains("lck")).sortBy(_.lastModified).toSeq
    } catch {
      case t: Throwable => Seq[File]()
    }
  }

  private def fileRef(file: File): Elem = {
    val href = pathOf + "/?" + logFileTag + "=" + file.getName
    val ago = WebUtil.timeAgo(new Date(file.lastModified))
    <a href={ href }>{ file.getName + ":" } { ago }</a>
  }

  private def showList = {
    if (allFiles.isEmpty) {
      <div class="row"><div class="col-sm-11 col-sm-offset-1" style="margin-top:10px">No log files</div></div>
    } else
      allFiles.map(f => { <div class="row"><div class="col-sm-11 col-sm-offset-1" style="margin-top:10px">  { fileRef(f) }</div></div> })
  }

  private def showFileContents(valueMap: ValueMapT, response: Response): Boolean = {
    try {
      val file = new File(logDir, valueMap(logFileTag))
      val data = Util.readBinaryFile(file).right.get
      response.setEntity(new String(data), MediaType.TEXT_PLAIN)
      logger.info("Showing contents of log file " + file.getAbsolutePath)
      true
    } catch {
      case t: Throwable => false
    }
  }

  val confirmRestartLabel = "confirmRestart"
  private def confirmRestart: Elem = {
    val href = pathOf + "?" + confirmRestartLabel + "=" + confirmRestartLabel
    <a class="btn btn-danger" href={ href } role="button" title="Shut down and restart service.
Jobs in progress will be aborted.">Confirm Restart</a>
  }

  val cancelRestartLabel = "cancelRestart"
  private def cancelRestart: Elem = {
    val href = pathOf + "?" + cancelRestartLabel + "=" + cancelRestartLabel
    <a class="btn btn-default" href={ href } role="button">Cancel</a>
  }

  val requestRestartLabel = "requestRestart"
  private def requestRestart: Elem = {
    // TODO require user to confirm
    val href = pathOf + "?" + requestRestartLabel + "=" + requestRestartLabel
    <a class="btn btn-default" href={ href } role="button" title="Shut down and restart. Jobs in progress will be aborted.">Restart Service</a>
  }

  private def basicInfo: Elem = {

    val mac = {
      val text = OpSys.getMACAddress.formatted("%012x")
      (0 until 12 by 2).map(octet => text.substring(octet, octet + 2)).mkString("-")
    }

    <div class="row">
      <h5>Service Version: { System.getProperty("Build.Service_version") }</h5>
      <h5>Build Date: { System.getProperty("Build.ServiceBuildDate") }</h5>
      <h5>Built By: { System.getProperty("Build.Builder") }</h5>
      <h5>Local Server IP: { OpSys.getHostIPAddress }</h5>
      <h5>Server Name: { OpSys.getHostName }</h5>
      <h5>Service started:{ timeAgo("", new Date(AQA.serviceStartTime)) }</h5>
      <p></p>
      { requestRestart }
    </div>
  }

  private def configInfo: Elem = {
    <div class="row">
      <h4>Configuration Parameters</h4>
      { Config.toHtml }
    </div>
  }

  private def showLogFileList: Elem = {
    <div class="row">
      <h4 style="margin-top:40px;">Log Files</h4>
      { showList }
    </div>
  }

  /**
   * Show a page that lets the user do a confirmation to really really restart the service.
   */
  private def doConfirm(response: Response) = {
    val content = {
      <div class="row">
        <div class="col-md-8 col-sm-offset-1">
          <h3>Service Restart</h3>
          <table style="margin:40px">
            <tr>
              <td>
                { cancelRestart }
              </td>
              <td><div style="margin:40px"> </div></td>
              <td>
                { confirmRestart }
              </td>
            </tr>
          </table>
        </div>
      </div>
    }
    setResponse(wrapBody(content, pageTitle), response, Status.SUCCESS_OK)
  }

  private def restartService(response: Response): Unit = {
    AQA.initiateServiceRestart
    response.redirectSeeOther(pathOf + "/?" + waitForRestartLabel + "=" + waitForRestartLabel)
  }

  val waitForRestartLabel = "waitForRestart"

  /**
   * Show a page that waits until the service has restarted and then redirects them to the home page.
   */
  private def waitForRestart(response: Response) = {
    val content = {
      <div class="row">
        <div class="col-md-8 col-sm-offset-1">
          <h3>Service Restart</h3>
          <h3>Waiting for Service Restart</h3>
          When the service is ready you will automatically be redirected to the main page.
        </div>
      </div>
    }

    val javascript = """
<script language="javascript">
// Reload the main page when the server is ready.

var status = '""" + AQA.serviceStartTime + """';
var instanceUrl = '/admin/ServiceInstance';
var WebRefreshTime = 1000;

function watchStatus() {
	$.ajax({
		url : instanceUrl,
		success : function(result) {
			if (status == result) {
			//if (status > 1) {
				setTimeout(watchStatus, WebRefreshTime);
			} else {
				window.location.assign("/");
			}
		},
		error : function(result) {
			setTimeout(watchStatus, WebRefreshTime);
		}
	});
}

setTimeout(watchStatus, WebRefreshTime);
</script>
"""

    setResponse(wrapBody(content, pageTitle, None, true, Some(javascript)), response, Status.SUCCESS_OK)
  }

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
