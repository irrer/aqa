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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.phase2.Phase2Util

import java.io.File
import scala.xml.Elem

case class MetadataCheck(
    metadataCheckPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    beamName: String, // name of beam in plan
    SOPInstanceUID: Option[String], // DICOM SOPInstanceUID 0008,0018
    gantryAnglePlan_deg: Double, // planned gantry angle in degrees
    gantryAnglePlanMinusImage_deg: Double, // difference from planned gantry angle in degrees
    collimatorAnglePlan_deg: Double, // planned collimator angle in degrees
    collimatorAnglePlanMinusImage_deg: Double, // difference from planned collimator angle in degrees
    x1JawPlan_mm: Double, // planned jaw position in mm
    x1JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
    x2JawPlan_mm: Double, // planned jaw position in mm
    x2JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
    y1JawPlan_mm: Double, // planned jaw position in mm
    y1JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
    y2JawPlan_mm: Double, // planned jaw position in mm
    y2JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
    energyPlan_kev: Double, // planned energy in kilo electron volts
    energyPlanMinusImage_kev: Double, // difference from planned energy in kilo electron volts
    flatteningFilter: Boolean, // true if a flattening filter was present
    pass: Boolean // true if all values were within tolerances
) {

  def insert: MetadataCheck = {
    val insertQuery = MetadataCheck.query returning MetadataCheck.query.map(_.metadataCheckPK) into
      ((metadataCheck, metadataCheckPK) => metadataCheck.copy(metadataCheckPK = Some(metadataCheckPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(MetadataCheck.query.insertOrUpdate(this))

  override def toString: String = {
    "    metadataCheckPK: " + metadataCheckPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    beamName: " + beamName + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    gantryAnglePlan_deg: " + gantryAnglePlan_deg + "\n" +
      "    gantryAnglePlanMinusImage_deg: " + gantryAnglePlanMinusImage_deg + "\n" +
      "    collimatorAnglePlan_deg: " + collimatorAnglePlan_deg + "\n" +
      "    collimatorAnglePlanMinusImage_deg: " + collimatorAnglePlanMinusImage_deg + "\n" +
      "    x1JawPlan_mm: " + x1JawPlan_mm + "\n" +
      "    x1JawPlanMinusImage_mm: " + x1JawPlanMinusImage_mm + "\n" +
      "    x2JawPlan_mm: " + x2JawPlan_mm + "\n" +
      "    x2JawPlanMinusImage_mm: " + x2JawPlanMinusImage_mm + "\n" +
      "    y1JawPlan_mm: " + y1JawPlan_mm + "\n" +
      "    y1JawPlanMinusImage_mm: " + y1JawPlanMinusImage_mm + "\n" +
      "    y2JawPlan_mm: " + y2JawPlan_mm + "\n" +
      "    y2JawPlanMinusImage_mm: " + y2JawPlanMinusImage_mm + "\n" +
      "    energyPlan_kev: " + energyPlan_kev + "\n" +
      "    energyPlanMinusImage_kev: " + energyPlanMinusImage_kev + "\n" +
      "    flatteningFilter: " + flatteningFilter + "\n" +
      "    pass: " + pass + "\n"
  }
}

object MetadataCheck extends ProcedureOutput with Logging {
  class MetadataCheckTable(tag: Tag) extends Table[MetadataCheck](tag, "metadataCheck") {

    def metadataCheckPK = column[Long]("metadataCheckPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def beamName = column[String]("beamName")
    def SOPInstanceUID = column[Option[String]]("SOPInstanceUID")
    def gantryAnglePlan_deg = column[Double]("gantryAnglePlan_deg")
    def gantryAnglePlanMinusImage_deg = column[Double]("gantryAnglePlanMinusImage_deg")
    def collimatorAnglePlan_deg = column[Double]("collimatorAnglePlan_deg")
    def collimatorAnglePlanMinusImage_deg = column[Double]("collimatorAnglePlanMinusImage_deg")
    def x1JawPlan_mm = column[Double]("x1JawPlan_mm")
    def x1JawPlanMinusImage_mm = column[Double]("x1JawPlanMinusImage_mm")
    def x2JawPlan_mm = column[Double]("x2JawPlan_mm")
    def x2JawPlanMinusImage_mm = column[Double]("x2JawPlanMinusImage_mm")
    def y1JawPlan_mm = column[Double]("y1JawPlan_mm")
    def y1JawPlanMinusImage_mm = column[Double]("y1JawPlanMinusImage_mm")
    def y2JawPlan_mm = column[Double]("y2JawPlan_mm")
    def y2JawPlanMinusImage_mm = column[Double]("y2JawPlanMinusImage_mm")
    def energyPlan_kev = column[Double]("energyPlan_kev")
    def energyPlanMinusImage_kev = column[Double]("energyPlanMinusImage_kev")
    def flatteningFilter = column[Boolean]("flatteningFilter")
    def pass = column[Boolean]("pass")

    def * =
      (
        metadataCheckPK.?,
        outputPK,
        beamName,
        SOPInstanceUID,
        gantryAnglePlan_deg,
        gantryAnglePlanMinusImage_deg,
        collimatorAnglePlan_deg,
        collimatorAnglePlanMinusImage_deg,
        x1JawPlan_mm,
        x1JawPlanMinusImage_mm,
        x2JawPlan_mm,
        x2JawPlanMinusImage_mm,
        y1JawPlan_mm,
        y1JawPlanMinusImage_mm,
        y2JawPlan_mm,
        y2JawPlanMinusImage_mm,
        energyPlan_kev,
        energyPlanMinusImage_kev,
        flatteningFilter,
        pass
      ) <> (MetadataCheck.apply _ tupled, MetadataCheck.unapply)

    def outputFK = foreignKey("MetadataCheck_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MetadataCheckTable]

  override val topXmlLabel = "MetadataCheck"

  def get(metadataCheckPK: Long): Option[MetadataCheck] = {
    val action = for {
      inst <- MetadataCheck.query if inst.metadataCheckPK === metadataCheckPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[MetadataCheck] = {
    val action = for {
      inst <- MetadataCheck.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(metadataCheckPK: Long): Int = {
    val q = query.filter(_.metadataCheckPK === metadataCheckPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[MetadataCheck]): Seq[Int] = {
    val ops = list.map { imgId => MetadataCheck.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Insert of MetadataCheck using Elem is not supported.")
  }

  def insertSeq(list: Seq[MetadataCheck]): Unit = {
    val ops = list.map { loc => MetadataCheck.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class MetadataCheckHistory(output: Output, metadataCheck: MetadataCheck) {
    override def toString: String = {
      "date: " + output.dataDate.get + "    " + metadataCheck
    }

    val ordering: String = output.dataDate.get.getTime + "  " + metadataCheck.beamName
  }

  /**
    * Get the CenterBeam results.
    *
    * @param machinePK: For this machine
    */
  def history(machinePK: Long, procedurePK: Long): Seq[MetadataCheckHistory] = {

    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      metadataCheck <- MetadataCheck.query.filter(c => c.outputPK === output.outputPK)
    } yield (output, metadataCheck)

    val result = Db.run(search.result).map(h => MetadataCheckHistory(h._1, h._2)).sortBy(_.ordering)
    result
  }

  // ----------------------------------------------------------------------------------------------

  /**
    * Put SOPInstanceUIDs in rows that need it.
    *
    * As of 16-Apr-2021 this is done automatically as part of Phase 2 code.
    *
    * This was only needed as a fix and should never be needed again.
    *
    * Also, this code is bit hacky, but it did the job.
    *
    * @param args DoIt [MAX_COUNT]   'DoIt' as the first parameter is required to make changes to the
    * database.  If MAX_COUNT is not given then all rows are fixed.
    */
  def main(args: Array[String]): Unit = {

    DbSetup.init

    var count = 0
    var doneCount = 0

    val DoIt = args.nonEmpty && args.head.equals("DoIt")

    val maxCount = {
      if (args.length > 1)
        args(1).toInt
      else
        Int.MaxValue
    }
    println("DoIt: " + DoIt + "    maxCount: " + maxCount)

    val outputPKList = {
      val phase2PK = Procedure.ProcOfPhase2.get.procedurePK.get
      val search = for {
        output <- Output.query.filter(o => o.procedurePK === phase2PK).map(_.outputPK)
      } yield output
      val seq = Db.run(search.result)
      seq
    }

    Trace.trace("\n\n\n\nNumber of outputs to process: " + outputPKList.size)

    def getDicomSeriesByOutput(outputPK: Long): Seq[AttributeList] = {
      val output = Output.get(outputPK).get
      val inputDir = output.dir.getParentFile

      if (!inputDir.isDirectory) {
        println("Input directory does not exist.  Restoring from database...")
        Input.getFilesFromDatabase(output.inputPK, inputDir.getParentFile)
      }

      if (inputDir.isDirectory) {
        val dicomFileList = Util.listDirFiles(inputDir).filter(f => f.isFile && DicomFileUtilities.isDicomOrAcrNemaFile(f))
        val alList = dicomFileList.flatMap(f => DicomFile(f).attributeList).filter(Util.isRtimage)
        alList
      } else {
        println("Input directory does not exist.")
        Seq()
      }
    }

    def getPlanFromShared(rtplanUID: String): AttributeList = {
      // try getting from (old) common directory
      val sharedFile = new File(Config.sharedDir, rtplanUID + ".dcm")
      if (sharedFile.canRead) {
        Trace.trace("Attempting to read file " + sharedFile.getAbsolutePath)
        val al = new AttributeList
        al.read(sharedFile)
        al
      } else
        throw new RuntimeException("can not get rtplan from shared: " + rtplanUID)
    }

    def getPlan(rtplanUID: String): AttributeList = {
      val rtplanDicomSeries = DicomSeries.getBySopInstanceUID(rtplanUID).headOption
      if (rtplanDicomSeries.isEmpty)
        getPlanFromShared(rtplanUID)
      else {
        val al = rtplanDicomSeries.get.attributeListList.filter(a => Util.sopOfAl(a).equals(rtplanUID)).head
        al
      }
    }

    def makeBeamMap(plan: AttributeList, imageList: Seq[AttributeList]): Map[String, String] = {
      val map = imageList.map(img => (Phase2Util.getBeamNameOfRtimage(plan, img).get, Util.sopOfAl(img))).toMap
      map
    }

    def fixSop(meta: MetadataCheck, beamMap: Map[String, String]): Unit = {
      count = count + 1
      if (count <= maxCount) {
        val j = beamMap.get(meta.beamName)
        if (j.isEmpty) {
          println("hey")
        }
        val sop = beamMap(meta.beamName)
        if (DoIt) {
          println("Doing it: meta: " + meta.metadataCheckPK.get + "   beam " + meta.beamName + " --> " + sop) // TODO should modify meta in DB
          val meta2 = meta.copy(SOPInstanceUID = Some(sop))
          meta2.insertOrUpdate()
          doneCount = doneCount + 1
        } else {
          println("NOT Doing it: meta: " + meta.metadataCheckPK.get + "   beam " + meta.beamName + " --> " + sop) // TODO should modify meta in DB
        }
      }
    }

    def fixMetadata(outputPK: Long): Unit = {
      println("Doing output: " + outputPK)
      try {
        val metaList = MetadataCheck.getByOutput(outputPK)
        if (metaList.isEmpty)
          Trace.trace("No MetadataCheck rows for outputPK " + outputPK)
        else {
          val needToDoList = metaList.filter(_.SOPInstanceUID.isEmpty)
          if (true) {
            val size = metaList.size
            val need = needToDoList.size
            0 match {
              case _ if size == need => Trace.trace("need to do all " + size + " MetadataCheck outputPK " + outputPK)
              case _ if need == 0    => Trace.trace("all MetadataCheck " + size + " already done for outputPK " + outputPK)
              case _                 => Trace.trace("Need to do " + need + " of " + size + " for outputPK " + outputPK)
            }
          }
          if (needToDoList.nonEmpty) {
            val imageAl = getDicomSeriesByOutput(outputPK)
            val rtplanUID = Phase2Util.referencedPlanUID(imageAl.head)
            val rtplanAl = getPlan(rtplanUID)
            //val rtplanText = DicomUtil.attributeListToString(rtplanAl)
            //if (false) println(rtplanText)
            if (false) {
              val imgMap = imageAl
                .map(al =>
                  Util.sopOfAl(al) + " :: " + {
                    val atList = DicomUtil.findAllSingle(al, TagByName.ReferencedBeamNumber)
                    atList.flatMap(_.getIntegerValues).mkString("  ")
                  }
                )
                .mkString("\n")
              println("imgMap:\n" + imgMap)
            }
            val beamMap = makeBeamMap(rtplanAl, imageAl)

            metaList.map(meta => fixSop(meta, beamMap))
          }
        }
      } catch {
        case t: Throwable =>
          println("outputPK " + outputPK + " failed: " + fmtEx(t))
      }
    }

    println("========= Starting ===============")
    val start = System.currentTimeMillis()
    outputPKList.foreach(fixMetadata)
    println("count: " + count)
    println("doneCount: " + doneCount)
    println("========= Done =============== elapsed: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }
}
