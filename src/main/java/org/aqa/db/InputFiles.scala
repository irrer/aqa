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
import org.aqa.Crypto
import org.aqa.Logging

/**
  * Store the zipped file contents of an Input.
  *
  * The convention for the primary key, which is enforced by the code, is to use the same pk for this
  * table as the corresponding input entry.  This is to help ensure that there is at most one InputFiles
  * for each Input.
  *
  * Note that this tables does NOT use an auto-incrementing counter for the primary key,
  */
case class InputFiles(
    inputFilesPK: Long, // primary key
    inputPK: Long, // referenced input
    zippedContent: Array[Byte]
) // The files in zip form created by the process
{

  def insert(): Unit = Db.run(InputFiles.query += this)

  override def toString: String = {
    s"inputFilesPK: $inputFilesPK    inputPK: $inputPK     zippedContent size ${zippedContent.length} : ${Crypto.byteArrayToHex(zippedContent.take(50))}"
  }

}

object InputFiles extends Logging {
  class InputFilesTable(tag: Tag) extends Table[InputFiles](tag, "inputFiles") {

    def inputFilesPK = column[Long]("inputFilesPK", O.PrimaryKey)
    def inputPK = column[Long]("inputPK")
    def zippedContent = column[Array[Byte]]("zippedContent")

    def * = (inputFilesPK, inputPK, zippedContent) <> (InputFiles.apply _ tupled, InputFiles.unapply)

    def inputFK = foreignKey("InputFiles_inputPKConstraint", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[InputFilesTable]

  def get(inputFilesPK: Long): Option[InputFiles] = {
    val action = for {
      inputFiles <- query if inputFiles.inputFilesPK === inputFilesPK
    } yield inputFiles
    val list = Db.run(action.result)
    list.headOption
  }

  def getByInputPK(inputPK: Long): Seq[InputFiles] = {
    val action = for {
      inputFiles <- query if inputFiles.inputPK === inputPK
    } yield inputFiles
    val list = Db.run(action.result)
    list
  }

  def delete(inputFilesPK: Long): Int = {
    val q = query.filter(_.inputFilesPK === inputFilesPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByInputPK(inputPK: Long): Int = {
    val q = query.filter(_.inputPK === inputPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    DbSetup.init
    val ts = new java.sql.Timestamp(System.currentTimeMillis)
    val inputOld = new Input(None, None, ts, None, None, None, None)
    val inputNew = inputOld.insert
    val pk = inputNew.inputPK.get
    println("pk: " + pk)

    val data: Array[Byte] = Array(1, 2, 3, 4)
    val inputFiles = new InputFiles(pk, pk, data)
    inputFiles.insert()
    println("inserted inputFiles")

    Input.delete(pk)
    println("done")
  }

}
