package org.aqa.approval

import edu.umro.ScalaUtil.Trace
import org.aqa.db.OutputApproval
import org.aqa.db.User
import org.aqa.Util

import java.text.SimpleDateFormat
import scala.xml.Elem

case class ApprovalHtml(outputPK: Long) {

  def elem: Elem = {
    val approvalStatus = OutputApproval.getByOutput(outputPK).lastOption

    val id = s"Approval_$outputPK"

    def choiceList(status: String) = {

      def toElem(statusChoice: OutputApproval.Status): Elem = statusChoice.toOption(status.equals(statusChoice.name))

      Trace.trace("UNAPPROVED: " + toElem(OutputApproval.UNAPPROVED))
      Trace.trace("APPROVED  : " + toElem(OutputApproval.APPROVED))
      Trace.trace("REJECTED  : " + toElem(OutputApproval.REJECTED))

      Seq(
        toElem(OutputApproval.UNAPPROVED),
        toElem(OutputApproval.APPROVED),
        toElem(OutputApproval.REJECTED)
      )
    }

    def makeSelector(status: String): Elem = {
      val onchange = s"ApprovalChange($outputPK)"
      val sel = {
        <select value={status} prevvalue={status} name={id} id={id} class="form-control" onchange={onchange}>
          {choiceList(status)}
        </select>
      }
      sel
    }

    def userElem(userId: String): Elem = {
      <span aqaalias="" id={s"ApprovalUser_$outputPK"}>{userId}</span>
    }

    def dateElem(dateText: String): Elem = {
      <span id={s"ApprovalDate_$outputPK"}>{dateText}</span>
    }

    if (approvalStatus.isDefined) {

      val status = approvalStatus.get.status
      val userId = User.get(approvalStatus.get.userPK).get.id
      val dateText = Util.formatDate(ApprovalHtml.dateFormat, approvalStatus.get.creationDateTime)

      val content = {
        <span>
          {makeSelector(status)}
          {userElem(userId)}
          <br>{dateElem(dateText)}</br>
        </span>
      }
      content
    } else {

      <span>
        {makeSelector(OutputApproval.defaultStatus.name)}
          {userElem("")}
        <br/>
        {dateElem("")}
      </span>
    }
  }

}

object ApprovalHtml {

  // val dateFormat = new SimpleDateFormat("EEE MMM d yyyy H:mm") // TODO put back
  val dateFormat = new SimpleDateFormat("EEE MMM d yyyy H:mm:ss") // TODO take out

}
