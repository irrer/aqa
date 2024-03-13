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

package org.aqa.db

import org.aqa.db.Db.driver.api._
import org.aqa.Logging

/**
  * Support adding a user defined note to an output.
  */
case class OutputNote(
    outputNotePK: Option[Long], // primary key
    outputPK: Long, // output primary key
    mediaFormat: String, // format that the note is in, e.g. text, pdf, etc.  This field is not used at this time (only text supported), but may be useful in the future.
    content: Array[Byte] // content of note
) extends Logging {

  def insert: OutputNote = {
    val insertQuery = OutputNote.query returning OutputNote.query.map(_.outputNotePK) into
      ((outputNote, outputNotePK) => outputNote.copy(outputNotePK = Some(outputNotePK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(OutputNote.query.insertOrUpdate(this))

  /**
    * Get the content in text form.
    * @return
    */
  def contentAsText: String = new String(content)

  /** Percent of DR-GS over OPEN. */

  override def toString: String = {
    "outputNotePK: " + outputNotePK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      " :: " + contentAsText
  }
}

object OutputNote extends Logging {
  class OutputNoteTable(tag: Tag) extends Table[OutputNote](tag, "outputNote") {

    def outputNotePK = column[Long]("outputNotePK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    private def mediaFormat = column[String]("mediaFormat")
    def content = column[Array[Byte]]("content")

    def * =
      (
        outputNotePK.?,
        outputPK,
        mediaFormat,
        content
      ) <> (OutputNote.apply _ tupled, OutputNote.unapply)

    def outputFK = foreignKey("OutputNote_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputNoteTable]

  def get(outputNotePK: Long): Option[OutputNote] = {
    val action = for {
      inst <- OutputNote.query if inst.outputNotePK === outputNotePK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
   * Get all the notes
   * @return All the notes.
   */
  def list(): Seq[OutputNote] = {
    val action = for {
      inst <- OutputNote.query
    } yield inst
    Db.run(action.result)
  }

  /**
    * Get the note (if there is one) for the given output.
    * @param outputPK For this output.
    * @return the OutputNote, if it exists.
    */
  def getByOutput(outputPK: Long): Option[OutputNote] = {
    val action = for {
      inst <- OutputNote.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result).headOption
  }

  def delete(outputNotePK: Long): Int = {
    val q = query.filter(_.outputNotePK === outputNotePK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  /*
  def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("OutputNote.insert not implemented for Elem parameter.")
  }
   */

}
