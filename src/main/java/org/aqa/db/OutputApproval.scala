/*
 * Copyright 2025 Regents of the University of Michigan
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

package org.aqa.db

import org.aqa.db.Db.driver.api._
import org.aqa.Logging

import java.sql.Timestamp
import scala.xml.Elem
import scala.xml.Text

/**
  * Support adding a user defined approval to an output.
  */
case class OutputApproval(
    outputApprovalPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    creationDateTime: Timestamp, // When the user made the status change
    userPK: Long, // User that made the status change
    status: String
) extends Logging {

  def insert: OutputApproval = {
    val insertQuery = OutputApproval.query returning OutputApproval.query.map(_.outputApprovalPK) into
      ((outputApproval, outputApprovalPK) => outputApproval.copy(outputApprovalPK = Some(outputApprovalPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(OutputApproval.query.insertOrUpdate(this))

  /** Percent of DR-GS over OPEN. */

  override def toString: String = {
    "outputApprovalPK: " + outputApprovalPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    creationDateTime: " + creationDateTime + "\n" +
      "    userPK: " + userPK + "\n" +
      "    status: " + status
  }
}

object OutputApproval extends Logging {
  class OutputApprovalTable(tag: Tag) extends Table[OutputApproval](tag, "outputApproval") {

    def outputApprovalPK = column[Long]("outputApprovalPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def creationDateTime = column[Timestamp]("creationDateTime")
    def userPK = column[Long]("userPK")
    def status = column[String]("status")

    def * =
      (
        outputApprovalPK.?,
        outputPK,
        creationDateTime,
        userPK,
        status
      ) <> (OutputApproval.apply _ tupled, OutputApproval.unapply)

    def outputFK = //
      foreignKey("OutputApproval_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputApprovalTable]

  case class Status(name: String, htmlPrefix: String) {
    def toOption(selected: Boolean): Elem = {

      val text = s"$htmlPrefix $name"

      val elem1: Elem = new Elem(null, "option", scala.xml.Null, scala.xml.TopScope, minimizeEmpty = false, Text(text))

      val elem2 = elem1 % new scala.xml.UnprefixedAttribute("value", Text(name), scala.xml.Null)

      val elem3 =
        if (selected)
          elem2 % new scala.xml.UnprefixedAttribute("selected", Text("true"), scala.xml.Null)
        else
          elem2

      elem3
    }
  }

  val UNAPPROVED: Status = Status("UNAPPROVED", "\u25EF") // '&' + "#9711;")
  val APPROVED: Status = Status("APPROVED", "\u2705") // '&' + "#x2705;")
  val REJECTED: Status = Status("REJECTED", "\u274C") // '&' + "#10060;")

  /** List of all possible statuses */
  val statusList: Seq[Status] = Seq(UNAPPROVED, APPROVED, REJECTED)

  /**
    * Convert a string to a Status
    * @param text Name of status
    * @return Corresponding status, or None on failure.
    */
  def stringToStatus(text: String): Option[Status] = {
    statusList.find(s => s.name.equals(text))
  }

  val defaultStatus: Status = UNAPPROVED

  /**
    * Get the list of all approvals for this output.
    * @param outputApprovalPK For this output.
    * @return List of approvals, may be empty.
    */
  def get(outputApprovalPK: Long): Option[OutputApproval] = {
    val action = for {
      approval <- OutputApproval.query if approval.outputApprovalPK === outputApprovalPK
    } yield approval
    Db.run(action.result).headOption
  }

  /**
    * Get all the approvals
    * @return All the approvals.
    */
  def list(): Seq[OutputApproval] = {
    val action = for {
      approval <- OutputApproval.query
    } yield approval
    Db.run(action.result)
  }

  /**
    * Get the approval (if there is one) for the given output.
    * @param outputPK For this output.
    * @return the OutputApproval, if it exists.
    */
  def getByOutput(outputPK: Long): Seq[OutputApproval] = {
    val action = for {
      approval <- OutputApproval.query if approval.outputPK === outputPK
    } yield approval
    Db.run(action.result).sortBy(_.creationDateTime.getTime)
  }

  def delete(outputApprovalPK: Long): Int = {
    val q = query.filter(_.outputApprovalPK === outputApprovalPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

}
