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
import org.aqac.db.Machine
import org.restlet.representation.Representation
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.restlet.ext.fileupload.RestletFileUpload
import edu.umro.ScalaUtil.Trace._
import java.io.File
import org.restlet.data.CharacterSet

object RunWinstonLutz {
    val path = "/RunWinstonLutz"

    val RunWinstonLutzPKTag = "RunWinstonLutzPK"
}

class RunWinstonLutz extends Restlet {

    /** Maximum tongue and groove offset in mm.  Exceeding this value probably indicates a user error. */
    private val maxTongueAndGrooveOffset = 10.0

    private val pageTitle = "Winston Lutz"

    def machineList() = Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

    private val machine = new WebInputSelect("Machine", 6, 0, machineList)

    private val tongueAndGrooveX = new WebInputText("Tongue and Groove correction X", 3, 0, "In mm. Use 0 if not known")

    private val tongueAndGrooveY = new WebInputText("Tongue and Groove correction Y", 3, 0, "In mm. Use 0 if not known")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = RunWinstonLutz.path + "?" + name + "=" + name
        new FormButton(name, 1, 0, action, buttonType)
    }

    private val runButton = makeButton("Run", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val RunWinstonLutzPK = new WebInputHidden(RunWinstonLutz.RunWinstonLutzPKTag)

    private val form = new WebForm(RunWinstonLutz.path,
        List(List(machine), List(tongueAndGrooveX, tongueAndGrooveY), List(runButton, cancelButton)),
        6)

    private val defaultValueMap =
        Map((tongueAndGrooveX.label, "0.0"),
            (tongueAndGrooveY.label, "0.0"))

    private def emptyForm(response: Response) = {
        form.setFormResponse(defaultValueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def validateTongueAndGroove(valueMap: Map[String, String], input: IsInput): Map[String, Style] = {
        val err = Error.make(input, "Must be a valid floating point number from -" + maxTongueAndGrooveOffset + " to " + maxTongueAndGrooveOffset)
        try {
            val tg = input.getValOrEmpty(valueMap).toFloat
            if ((tg >= (-maxTongueAndGrooveOffset)) && (tg <= (maxTongueAndGrooveOffset))) styleNone
            else err
        }
        catch {
            case e: Exception => err
        }
    }

    private def validateFiles(valueMap: Map[String, String]): Map[String, Style] = {
        val dir = sessionDir(valueMap)
        0 match {
            case _ if (!dir.isDirectory) => Error.make(???, "No files have been uploaded (no directory)")
            case _ if (dir.list.size < 2) => Error.make(???, "At least two files are required.")
            case _ => styleNone
        }
    }

    private def validate(valueMap: Map[String, String]): Map[String, Style] = {
        validateFiles(valueMap) ++
            validateTongueAndGroove(valueMap, tongueAndGrooveX) ++
            validateTongueAndGroove(valueMap, tongueAndGrooveY)
    }

    /**
     * Run the procedure.
     */
    private def run(valueMap: Map[String, String], response: Response) = {
        // TODO actually run the test and redirect to test results
        val errMap = validate(valueMap)
        if (errMap.isEmpty) {
            //moveFiles(
        //runWinstonLutz()
        }
        else 
                    form.setFormResponse(valueMap, errMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }

    private def buttonIs(valueMap: Map[String, String], button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = defaultValueMap ++ getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
                case _ if buttonIs(valueMap, runButton) => run(valueMap, response)
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }
}
