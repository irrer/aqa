package org.aqa.approval

import org.aqa.db.OutputApproval
import org.aqa.db.User
import org.aqa.Util
import org.aqa.web.WebUtil

import java.text.SimpleDateFormat
import scala.xml.Elem

case class ApprovalHtml(outputPK: Long, user: Option[User]) {

  def elem: Elem = {
    val approvalStatus = OutputApproval.getByOutput(outputPK).lastOption

    val id = s"Approval_$outputPK"

    def choiceList(status: String) = {

      def toElem(statusChoice: OutputApproval.Status): Elem = statusChoice.toOption(status.equals(statusChoice.name))

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

      if (user.isDefined && user.get.isApprover)
        sel
      else
        WebUtil.addAttr(sel, "disabled", "disabled")
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
  val dateFormat = new SimpleDateFormat("EEE MMM d yyyy H:mm")
}
