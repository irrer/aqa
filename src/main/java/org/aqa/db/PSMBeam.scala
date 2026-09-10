/*
 * Copyright 2024 Regents of the University of Michigan
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
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.db.Db.driver.api._
import org.aqa.Logging
import org.aqa.Util

import java.sql.Timestamp
import javax.vecmath.Point2d

case class PSMBeam(
                    psmBeamPK: Option[Long], // primary key
                    outputPK: Long, // output primary key
                    xCenter_mm: Double, // X coordinate of beam's center in mm
                    yCenter_mm: Double, // Y coordinate of beam's center in mm
                    SOPInstanceUID: String, // SOPInstanceUID if it is in the DICOM
                    beamName: String, // name of beam
                    Rows: Int, // Number of rows in the image.  DICOM metadata 0028,0010
                    Columns: Int, // Number of columns in the image.  DICOM metadata 0028,0011
                    ImagePlanePixelSpacingX: Double, // Physical distance (in mm) between the center of each image pixel in the X axis.  DICOM metadata 3002,0011 first value
                    ImagePlanePixelSpacingY: Double, // Physical distance (in mm) between the center of each image pixel in the Y axis.  DICOM metadata 3002,0011 second value
                    mean_cu: Double, // average value of pixels in CU
                    stdDev_cu: Double, // standard deviation of pixels in CU
                    top_mm: Option[Double], // top of field edge measurement.  Possibly None if the point was too close to one of the edges making the measurement of all four edges impossible.
                    bottom_mm: Option[Double], // bottom of field edge measurement.  Possibly None if the point was too close to one of the edges making the measurement of all four edges impossible.
                    left_mm: Option[Double], // left field edge measurement.  Possibly None if the point was too close to one of the edges making the measurement of all four edges impossible.
                    right_mm: Option[Double], // right field edge measurement.  Possibly None if the point was too close to one of the edges making the measurement of all four edges impossible.m
                    floodField_cu: Option[Double], // value of flood field pixels that correspond to this point
                    wholeDetector_cu: Option[Double], // value of whole detector pixels that correspond to this point
                    beamResponseNormalized: Option[Double] // normalized value mean_cu:  mean_cu / (mean_cu of center beam)
                  ) {

  def insert: PSMBeam = {
    val insertQuery = PSMBeam.query returning PSMBeam.query.map(_.psmBeamPK) into
      ((psmBeam, psmBeamPK) => psmBeam.copy(psmBeamPK = Some(psmBeamPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(PSMBeam.query.insertOrUpdate(this))

  val center: Point2d = new Point2d(xCenter_mm, yCenter_mm)

  override def toString: String = {
    "    psmBeamPK: " + psmBeamPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    xCenter_mm: " + Util.fmtDbl(xCenter_mm) + "\n" +
      "    yCenter_mm: " + Util.fmtDbl(yCenter_mm) + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    Rows: " + Rows + "\n" +
      "    Columns: " + Columns + "\n" +
      "    ImagePlanePixelSpacingX: " + ImagePlanePixelSpacingX + "\n" +
      "    ImagePlanePixelSpacingY: " + ImagePlanePixelSpacingY + "\n" +
      "    mean_cu: " + Util.fmtDbl(mean_cu) + "\n" +
      "    stdDev_cu: " + Util.fmtDbl(stdDev_cu) + "\n" +
      "    top_mm: " + Util.fmtDbl(top_mm) + "\n" +
      "    bottom_mm: " + Util.fmtDbl(bottom_mm) + "\n" +
      "    left_mm: " + Util.fmtDbl(left_mm) + "\n" +
      "    right_mm: " + Util.fmtDbl(right_mm) + "\n" +
      "    floodField_cu: " + Util.fmtDbl(floodField_cu) + "\n" +
      "    wholeDetector_cu: " + Util.fmtDbl(wholeDetector_cu) + "\n" +
      "    beamResponseNormalized: " + Util.fmtDbl(beamResponseNormalized)
  }

  val beamResponse: Option[Double] = {
    if (floodField_cu.isDefined && beamResponseNormalized.isDefined)
      Some(floodField_cu.get * beamResponseNormalized.get)
    else
      None
  }

  def rawImage: Double = wholeDetector_cu.get * floodField_cu.get // multiply

  def psm: Double = rawImage / beamResponseNormalized.get

}

object PSMBeam extends Logging {
  class PSMBeamTable(tag: Tag) extends Table[PSMBeam](tag, "psmBeam") {

    def psmBeamPK = column[Long]("psmBeamPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def xCenter_mm = column[Double]("xCenter_mm")

    def yCenter_mm = column[Double]("yCenter_mm")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def beamName = column[String]("beamName")

    def Rows = column[Int]("Rows")

    def Columns = column[Int]("Columns")

    def ImagePlanePixelSpacingX = column[Double]("ImagePlanePixelSpacingX")

    def ImagePlanePixelSpacingY = column[Double]("ImagePlanePixelSpacingY")

    def mean_cu = column[Double]("mean_cu")

    def stdDev_cu = column[Double]("stdDev_cu")

    def top_mm = column[Option[Double]]("top_mm")

    def bottom_mm = column[Option[Double]]("bottom_mm")

    def left_mm = column[Option[Double]]("left_mm")

    def right_mm = column[Option[Double]]("right_mm")

    def floodField_cu = column[Option[Double]]("floodField_cu")

    def wholeDetector_cu = column[Option[Double]]("wholeDetector_cu")

    def beamResponseNormalized = column[Option[Double]]("beamResponseNormalized")

    def * =
      (
        psmBeamPK.?,
        outputPK,
        xCenter_mm,
        yCenter_mm,
        SOPInstanceUID,
        beamName,
        Rows,
        Columns,
        ImagePlanePixelSpacingX,
        ImagePlanePixelSpacingY,
        mean_cu,
        stdDev_cu,
        top_mm,
        bottom_mm,
        left_mm,
        right_mm,
        floodField_cu,
        wholeDetector_cu,
        beamResponseNormalized
      ) <> (PSMBeam.apply _ tupled, PSMBeam.unapply)

    def outputFK = foreignKey("PSMBeam_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMBeamTable]

  def get(psmBeamPK: Long): Option[PSMBeam] = {
    val action = for {
      inst <- PSMBeam.query if inst.psmBeamPK === psmBeamPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
   * Get PSM beams by machine and data date time.
   *
   * @param machinePK Machine.
   * @param dataDate  Data data from PSM output.
   * @return List of beams for that machine and data date.
   */
  def getByMachineAndTime(machinePK: Long, dataDate: Timestamp): Seq[PSMBeam] = {
    val action = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.dataDate === dataDate))
      inst <- PSMBeam.query.filter(p => p.outputPK === output.outputPK)
    } yield inst
    val list = Db.run(action.result)
    list
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[PSMBeam] = {
    val action = for {
      inst <- PSMBeam.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(psmBeamPK: Long): Int = {
    val q = query.filter(_.psmBeamPK === psmBeamPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[PSMBeam]): Seq[Int] = {
    list.map(_.insertOrUpdate())
  }

  case class PSMBeamHistory(output: Output, psmBeamList: Seq[PSMBeam]) {
    def matchesResolution(rtimage: AttributeList): Boolean = {
      val trans = new IsoImagePlaneTranslator(rtimage)

      val p = psmBeamList.head

      // @formatter:off
      val ok =
        (trans.width      == p.Columns                ) &&
        (trans.height     == p.Rows                   ) &&
        (trans.pixelSizeX == p.ImagePlanePixelSpacingX) &&
        (trans.pixelSizeY == p.ImagePlanePixelSpacingY)
      // @formatter:on
      ok
    }
  }

  /**
   * Get the history of PSMBeam results.
   *
   * @param machinePK : For this machine
   * @return Complete history sorted by date.
   *
   */
  def historyByMachine(machinePK: Long): Seq[PSMBeamHistory] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      beam <- PSMBeam.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, beam)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val both = Db.run(search.result)

    // list of all distinct outputs (outputPK, output)
    val outputMap = both.map(b => b._1).groupBy(_.outputPK.get).values.map(oList => (oList.head.outputPK.get, oList.head)).toMap

    // make PSMBeamHistory list
    val history = both.map(_._2).groupBy(_.outputPK).values.map(psmList => PSMBeamHistory(outputMap(psmList.head.outputPK), psmList))

    // sort by data date
    history.toSeq.sortBy(_.output.dataDate.get.getTime)
  }

}
