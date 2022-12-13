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

package org.aqa.procedures

import org.aqa.db.DiffBaselineOpen
import org.aqa.db.DiffBaselineTrans
import org.aqa.db.EPIDCenterCorrection
import org.aqa.db.LeafOffsetCorrection
import org.aqa.db.LeafTransmission
import org.aqa.db.LOCRSquared
import org.aqa.Logging

import java.io.File
import scala.xml.Elem

object ProcedureOutputUtil extends Logging {

  val outputFileName = "output.xml"

  private val procedureList: List[ProcedureOutput] = List(LeafOffsetCorrection, LeafTransmission, EPIDCenterCorrection, LOCRSquared, DiffBaselineOpen, DiffBaselineTrans)

  private def insert(elem: Elem, outputPK: Long) = procedureList.map(po => po.insert(elem, outputPK))

  private val labelList = procedureList.map(p => p.topXmlLabel)

  private def outputPkFromElem(elem: Elem): Option[Long] = {
    try {
      Some((elem \ "@outputPK").head.toString.toLong)
    } catch {
      case t: Throwable => {
        val msg = "Unable to get outputPK from XML file: " + fmtEx(t)
        logger.warn(msg)
        throw new RuntimeException(msg)
        None
      }
    }

  }

  private def getOutputFile(args: Array[String]): File = {
    if ((args == null) || (args.isEmpty)) new File(outputFileName)
    else new File(args(0))
  }

  /**
    * Give each of the ProcedureOutput classes an opportunity to extract data from the XML and insert it into the database.
    */
  def insertIntoDatabase(elem: Elem, outputPK: Option[Long]): Unit = {
    val outPK: Long = outputPK match {
      case Some(opk) => opk
      case _         => outputPkFromElem(elem).get
    }
    procedureList.map(p => p.insert(elem, outPK))
  }

  /*
    def main(args: Array[String]): Unit = {
        try {
            val valid = Config.validate
            DbSetup.init
            println("Inserting data into database")
            val outputPK = System.getenv("outputPK").toLong
            println("Using outputPK: " + outputPK)
            val outputFile = getOutputFile(args)
            println("Using outputFile: " + outputFile.getAbsolutePath)
            val elem = XML.loadFile(outputFile)
            println("Read file " + outputFile.getAbsolutePath)
            insertIntoDatabase(elem, outputPK)
            println("Done inserting data into database")
            System.exit(0)
        }
        catch {
            case t: Throwable => {
                ProcedureStatus.terminate("Unexpected problem while inserting data into the database: " + Logging.fmtEx(t), ProcedureStatus.crash)
                System.exit(1)
            }
        }
    }
   */

}
