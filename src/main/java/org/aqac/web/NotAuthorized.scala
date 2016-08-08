package org.aqac.web

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
import org.aqac.db.MachineType
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
import org.aqac.Logging._


class NotAuthorized extends Restlet with SubUrlRoot {

    private val pageTitle = "Not Authorized"

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = pathOf + "?" + name + "=" + name
        new FormButton(name, 1, 0, subUrl, action, buttonType)
    }

    private val homeButton = makeButton("Home", true, ButtonType.BtnPrimary)

    private val content = "Your user id and password are correct, but are not authorized to access this content."

    private val message = new WebPlainText("message", false, 6, 0, ((Null)) => <div>{ content }</div>)

    private val form = new WebForm(pathOf, List(List(message), List(homeButton)))

    private def emptyForm(response: Response) = {
        form.setFormResponse(Map[String, String](), styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def buttonIs(valueMap: Map[String, String], button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, homeButton) => response.redirectSeeOther("/")
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }
}
