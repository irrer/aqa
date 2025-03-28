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
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil.ToZipOutputStream
import org.aqa.db.Db.driver.api._
import org.aqa.Logging
import org.aqa.Util

/**  */
case class PSM(
    psmPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    floodFieldPK: Long, // Flood field from which this was derived
    xMax_mm: Double, // X coordinate of maximum point determined by bicubic interpolation in mm
    yMax_mm: Double, // Y coordinate of maximum point determined by bicubic interpolation in mm
    SOPInstanceUID: String, // SOPInstanceUID if it is in the DICOM
    Rows: Int, // Number of rows in the image.  DICOM metadata 0028,0010
    Columns: Int, // Number of columns in the image.  DICOM metadata 0028,0011
    ImagePlanePixelSpacingX: Double, // Physical distance (in mm) between the center of each image pixel in the X axis.  DICOM metadata 3002,0011 first value
    ImagePlanePixelSpacingY: Double, // Physical distance (in mm) between the center of each image pixel in the Y axis.  DICOM metadata 3002,0011 second value
    dicom_zip: Array[Byte] // Single DICOM image in zip form.
) extends Logging {

  def insert: PSM = {
    val insertQuery = PSM.query returning PSM.query.map(_.psmPK) into
      ((psm, psmPK) => psm.copy(psmPK = Some(psmPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(PSM.query.insertOrUpdate(this))

  override def toString: String = {
    "    psmPK: " + psmPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    floodFieldPK: " + floodFieldPK + "\n" +
      "    xMax_mm: " + Util.fmtDbl(xMax_mm) + "\n" +
      "    yMax_mm: " + Util.fmtDbl(yMax_mm) + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    Rows: " + Rows + "\n" +
      "    Columns: " + Columns + "\n" +
      "    ImagePlanePixelSpacingX: " + ImagePlanePixelSpacingX + "\n" +
      "    ImagePlanePixelSpacingY: " + ImagePlanePixelSpacingY + "\n"
  }

  /** Binary content as DICOM. */
  lazy val dicom: AttributeList = DicomUtil.zippedByteArrayToDicom(dicom_zip).head
}

object PSM extends Logging {
  class PSMTable(tag: Tag) extends Table[PSM](tag, "psm") {

    def psmPK = column[Long]("psmPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def floodFieldPK = column[Long]("floodFieldPK")

    def xMax_mm = column[Double]("xMax_mm")

    def yMax_mm = column[Double]("yMax_mm")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def Rows = column[Int]("Rows")

    def Columns = column[Int]("Columns")

    def ImagePlanePixelSpacingX = column[Double]("ImagePlanePixelSpacingX")

    def ImagePlanePixelSpacingY = column[Double]("ImagePlanePixelSpacingY")

    def dicom_zip = column[Array[Byte]]("dicom_zip")

    def * =
      (
        psmPK.?,
        outputPK,
        floodFieldPK,
        xMax_mm,
        yMax_mm,
        SOPInstanceUID,
        Rows,
        Columns,
        ImagePlanePixelSpacingX,
        ImagePlanePixelSpacingY,
        dicom_zip
      ) <> (PSM.apply _ tupled, PSM.unapply)

    def outputFK = foreignKey("PSM_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMTable]

  def get(psmPK: Long): Option[PSM] = {
    val action = for {
      inst <- PSM.query if inst.psmPK === psmPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[PSM] = {
    val action = for {
      inst <- PSM.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(psmPK: Long): Int = {
    val q = query.filter(_.psmPK === psmPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[PSM]): Seq[Int] = {
    list.map(_.insertOrUpdate())
  }

  /**
    * Make a PSM object from the given parameters.  It is up to the caller to insert it into the database.
    * @param outputPK Attach to this output.
    * @param al Contains image and metadata.
    * @param xMax_mm X coordinate at max value.
    * @param yMax_mm y coordinate at max value.
    * @return A shiny new PSM object.
    */
  def makePSM(outputPK: Long, floodFieldPK: Long, al: AttributeList, xMax_mm: Double, yMax_mm: Double): PSM = {
    val dicom_zip = {
      val zos = new ToZipOutputStream()
      zos.writeDicom(al, "FloodField.dcm", "AQA")
      zos.finish()
    }

    val newPSM = PSM(
      psmPK = None,
      outputPK = outputPK,
      floodFieldPK = floodFieldPK,
      xMax_mm = xMax_mm,
      yMax_mm = yMax_mm,
      SOPInstanceUID = Util.sopOfAl(al),
      Rows = al.get(TagByName.Rows).getIntegerValues.head,
      Columns = al.get(TagByName.Columns).getIntegerValues.head,
      ImagePlanePixelSpacingX = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.head,
      ImagePlanePixelSpacingY = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.toSeq(1),
      dicom_zip = dicom_zip
    )

    newPSM
  }

  case class PSMHistory(output: Output, psmList: PSM) {}

  /**
    * Get the history of PSM results.
    *
    * @param machinePK : For this machine
    * @return Complete history sorted by date.
    *
    */
  def historyByMachine(machinePK: Long): Seq[PSMHistory] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      psm <- PSM.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, psm)
    }

    // Fetch entire history from the database.
    val pairList = Db.run(search.result)

    // make PSMHistory list
    val history = pairList.map(pair => new PSMHistory(pair._1, pair._2))

    // sort by data date
    history.sortBy(_.output.dataDate.get.getTime)
  }

}
