package org.aqa.webrun.dailyQA

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.WebUtil._
import org.restlet.data.MediaType
import org.aqa.Logging
import org.restlet.data.Status
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import java.util.Date
import scala.xml.Elem
import org.aqa.db.User
import org.aqa.db.Institution
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.Output
import org.aqa.db.Machine

/**
 * Support for generating JS scripts on request.
 */
object DailyQARestlet {

  private val pageTitle = "Daily QA Summary"
  private val path = new String((new DailyQARestlet).pathOf)
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?date=" + outputPK + "'></script>"
  }
}

class DailyQARestlet extends Restlet with SubUrlRoot with Logging {

  private val userPkTag = "userPkTag"

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private val prevButton = makeButton("Previous", false, ButtonType.BtnDefault)
  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)
  private val nextButton = makeButton("Next", false, ButtonType.BtnDefault)
  private val allInstitutions = new WebInputCheckbox("Show All Institutions", true, Some("Check to see data from all institions."), 3, 0)

  val buttonList: WebRow = List(prevButton, refreshButton, nextButton)

  private val dateField = new WebInputDatePicker("Date", 5, 1)

  private val report = new WebPlainText("report", false, 10, 1, makeReport _)

  private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, List(dateField ++ List(buttonList, allInstitutions), List(report)))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private def makeReport(valueMap: ValueMapT): Elem = {

    val user: Option[User] = {
      valueMap.get(userPkTag) match {
        case Some(userPkText) =>
          User.get(userPkText.toLong)
          None
      }
    }

    val isWhiteListed = {
      user match {
        case Some(u) => userIsWhitelisted(u.id)
        case _ => false
      }
    }

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    val showAllInst: Boolean = valueMap.get(allInstitutions.label) match {
      case Some(value) => value.equalsIgnoreCase(true.toString)
      case _ => false
    }

    case class Col(name: String, title: String, toElem: (Pair) => Elem) {
      def toHeader = <th title={ title }>{ name }</th>
    }

    case class Pair(cbct: Option[BBbyCBCT], epid: Option[BBbyEPID]);

    def getMachine(pair: Pair): Elem = {
      def getMachId(outputPK: Long): String = {
        val output = Output.get(outputPK).get
        val mach = Machine.get(output.machinePK.get).get
        mach.id
      }

      def machHtml(outputPK: Long): Elem = {
        wrapAlias(<div title="Machine Name">{ getMachId(outputPK) }</div>)
      }

      val elem = (pair.cbct, pair.epid) match {
        case (Some(c), _) => machHtml(c.outputPK)
        case (_, Some(e)) => machHtml(e.outputPK)
        case _ => <div title="Could not determine source machine">Unknown</div>
      }
      elem
    }

    val colList: List[Col] = List(
      new Col("Machine", "Name of treatment machine", getMachine _)
    // TODO NEXT: make more columns.  RE: Justin's email (flagged) from Sep 9.
    )

    def pairToRow(pair: Pair) = {
      colList.map(col => col.toElem(pair))
    }

    def getPairList: List[Pair] = {
      ??? // TODO
    }

    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          <table class="table table-responsive">
            <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
            { getPairList.map(pair => pairToRow(pair)) }
          </table>
        </div>
      </div>
    }

    content
  }

  /**
   * Get midnight of selected date.  If there is a formatting or other problem, return the
   *  current date.  Midnight facilitates searching the entire day.
   */
  private def getSelectedDate(valueMap: ValueMapT): Date = {

    /** Midnight of current date.  Facilitates searching the entire day. */
    def now: Date = {
      val text = Util.standardDateFormat.format((new Date).getTime)
      Util.standardDateFormat.parse(text.replaceAll("T.*", "T:00:00:00"))
    }

    try {
      dateField.dateFormat.parse(valueMap(dateField.label))
    } catch {
      case t: Throwable => now
    }
  }

  private def show(response: Response, valueMap: ValueMapT) = {
    val userPkValue = {
      getUser(response.getRequest) match {
        case Some(user) => Map((userPkTag, user.userPK.get.toString))
        case _ => Map[String, String]()
      }
    }
    formCreate(valueMap).setFormResponse(valueMap ++ userPkValue, styleNone, DailyQARestlet.pageTitle, response, Status.SUCCESS_OK)
    val selectedDate = getSelectedDate(valueMap)
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      show(response, valueMap)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}
