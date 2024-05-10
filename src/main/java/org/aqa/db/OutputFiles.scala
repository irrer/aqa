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
  * Store the zipped file contents of an Output.
  *
  * The convention for the primary key, which is enforced by the code, is to use the same pk for this
  * table as the corresponding output entry.  This is to help ensure that there is at most one OutputFiles
  * for each Output.
  *
  * Note that this tables does NOT use an auto-incrementing counter for the primary key,
  */
case class OutputFiles(
    outputFilesPK: Long, // primary key
    outputPK: Long, // referenced output
    zippedContent: Array[Byte]
) // The files in zip form created by the process
{

  def insert(): Unit = Db.run(OutputFiles.query += this)

}

object OutputFiles extends Logging {
  class OutputFilesTable(tag: Tag) extends Table[OutputFiles](tag, "outputFiles") {

    def outputFilesPK = column[Long]("outputFilesPK", O.PrimaryKey)
    def outputPK = column[Long]("outputPK")
    def zippedContent = column[Array[Byte]]("zippedContent")

    def * = (outputFilesPK, outputPK, zippedContent) <> (OutputFiles.apply _ tupled, OutputFiles.unapply)

    def outputFK = foreignKey("OutputFiles_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputFilesTable]

  def get(outputFilesPK: Long): Option[OutputFiles] = {
    val action = for {
      outputFiles <- query if outputFiles.outputFilesPK === outputFilesPK
    } yield outputFiles
    val list = Db.run(action.result)
    list.headOption
  }

  def getByOutput(outputPK: Long): Option[OutputFiles] = {
    val action = for {
      outputFiles <- query if outputFiles.outputPK === outputPK
    } yield outputFiles
    val list = Db.run(action.result)
    list.headOption
  }

  def delete(outputFilesPK: Long): Int = {
    val q = query.filter(_.outputFilesPK === outputFilesPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

}
