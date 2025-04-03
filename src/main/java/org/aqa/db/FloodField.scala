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

import java.sql.Timestamp

/**
  * Describe a flood field used for PSM processing.  Note that this is different from the flood field use in Phase2 and Phase3.
  */

case class FloodField(
    floodFieldPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    Rows: Int, // Number of rows in the image.  DICOM metadata 0028,0010
    Columns: Int, // Number of columns in the image.  DICOM metadata 0028,0011
    ImagePlanePixelSpacingX: Double, // Physical distance (in mm) between the center of each image pixel in the X axis.  DICOM metadata 3002,0011 first value
    ImagePlanePixelSpacingY: Double, // Physical distance (in mm) between the center of each image pixel in the Y axis.  DICOM metadata 3002,0011 second value
    SOPInstanceUID: Option[String], // SOPInstanceUID if it is in the DICOM
    StationName: String, // StationName from DICOM.  This infers the identity of the treatment machine.
    RTImageDescription: String, // RTImageDescription from DICOM.
    StudyID: String, // StudyID from DICOM.
    imageHash_md5: String, // MD5 hash of image bytes
    dicom_zip: Array[Byte] // zipped DICOM content
) extends Logging {

  def insert: FloodField = {
    val insertQuery = FloodField.query returning FloodField.query.map(_.floodFieldPK) into
      ((floodField, floodFieldPK) => floodField.copy(floodFieldPK = Some(floodFieldPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(FloodField.query.insertOrUpdate(this))

  override def toString: String = {
    "    floodFieldPK: " + floodFieldPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    Rows: " + Rows + "\n" +
      "    Columns: " + Columns + "\n" +
      "    ImagePlanePixelSpacingX: " + ImagePlanePixelSpacingX + "\n" +
      "    ImagePlanePixelSpacingY: " + ImagePlanePixelSpacingY + "\n" +
      "    SOPInstanceUID: " + {
      if (SOPInstanceUID.isDefined) SOPInstanceUID.get else "NA"
    } + "\n" +
      "    StationName: " + StationName + "\n" +
      "    RTImageDescription: " + RTImageDescription + "\n" +
      "    StudyID: " + StudyID + "\n" +
      "    imageHash_md5: " + imageHash_md5 + "\n"
  }

  /** Binary content as DICOM. */
  lazy val dicom: AttributeList = DicomUtil.zippedByteArrayToDicom(dicom_zip).head

}

object FloodField extends Logging {
  class FloodFieldTable(tag: Tag) extends Table[FloodField](tag, "floodField") {

    def floodFieldPK = column[Long]("floodFieldPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def Rows = column[Int]("Rows")

    def Columns = column[Int]("Columns")

    def ImagePlanePixelSpacingX = column[Double]("ImagePlanePixelSpacingX")

    def ImagePlanePixelSpacingY = column[Double]("ImagePlanePixelSpacingY")

    def SOPInstanceUID = column[Option[String]]("SOPInstanceUID")

    def StationName = column[String]("StationName")

    def RTImageDescription = column[String]("RTImageDescription")

    def StudyID = column[String]("StudyID")

    def imageHash_md5 = column[String]("imageHash_md5")

    def dicom_zip = column[Array[Byte]]("dicom_zip")

    def * =
      (
        floodFieldPK.?,
        outputPK,
        Rows,
        Columns,
        ImagePlanePixelSpacingX,
        ImagePlanePixelSpacingY,
        SOPInstanceUID,
        StationName,
        RTImageDescription,
        StudyID,
        imageHash_md5,
        dicom_zip
      ) <> (FloodField.apply _ tupled, FloodField.unapply)

    def outputFK = foreignKey("FloodField_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[FloodFieldTable]

  def get(floodFieldPK: Long): Option[FloodField] = {
    val action = for {
      inst <- FloodField.query if inst.floodFieldPK === floodFieldPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[FloodField] = {
    val action = for {
      inst <- FloodField.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  /**
    * Get a list of all rows for the given hash.  There should be either zero or one.
    */
  def getByImageHash(imageHash: String): Seq[FloodField] = {
    val action = for {
      inst <- FloodField.query if inst.imageHash_md5 === imageHash
    } yield inst
    Db.run(action.result)
  }

  /**
    * Get all flood field entries that match passed parameters.
    *
    * @param machinePK               For this machine
    * @param Rows                    Number of rows of pixels
    * @param Columns                 Number of columns of pixels
    * @param ImagePlanePixelSpacingX Horizontal spacing of pixels in mm
    * @param ImagePlanePixelSpacingY Vertical spacing of pixels in mm
    * @param minDate                 On or before this
    * @param maxDate                 On or after this
    * @return List of all matching flood fields, sorted by delivery date.
    */
  def getMatching(
      machinePK: Long,
      Rows: Int,
      Columns: Int,
      ImagePlanePixelSpacingX: Double,
      ImagePlanePixelSpacingY: Double,
      minDate: Timestamp,
      maxDate: Timestamp //
  ): Seq[FloodField] = {
    val action = for {
      output <- Output.query if output.machinePK === machinePK
      ff <- FloodField.query
      if (ff.Rows === Rows) &&
        (ff.Columns === Columns) &&
        (ff.ImagePlanePixelSpacingX === ImagePlanePixelSpacingX) &&
        (ff.ImagePlanePixelSpacingY === ImagePlanePixelSpacingY) &&
        (ff.outputPK === output.outputPK) &&
        (output.dataDate >= minDate) &&
        (output.dataDate <= maxDate)
    } yield (output, ff)
    val outputFloodList = Db.run(action.result)
    val floodList: Seq[FloodField] = outputFloodList.sortBy(_._1.dataDate.get.getTime).map(_._2)

    floodList
  }

  def delete(floodFieldPK: Long): Int = {
    val q = query.filter(_.floodFieldPK === floodFieldPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[FloodField]): Seq[Int] = {
    list.map(_.insertOrUpdate())
  }

  /**
    * Create a FloodField object by extracting information from the given DICOM.
    *
    * @param outputPK For this output.
    * @param al       From this DICOM.
    * @return a new FloodField.
    */
  def makeFloodField(outputPK: Long, al: AttributeList): FloodField = {

    val Rows = al.get(TagByName.Rows).getIntegerValues.head
    val Columns = al.get(TagByName.Columns).getIntegerValues.head

    val ImagePlanePixelSpacingX = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.head
    val ImagePlanePixelSpacingY = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.toSeq(1)

    val StationName = al.get(TagByName.StationName).getSingleStringValueOrEmptyString
    val RTImageDescription = al.get(TagByName.RTImageDescription).getSingleStringValueOrEmptyString
    val SOPInstanceUID: Option[String] = {
      val attr = al.get(TagByName.SOPInstanceUID)
      if (attr == null)
        None
      else
        Some(attr.getSingleStringValueOrEmptyString)
    }
    val StudyID: String = al.get(TagByName.StudyID).getSingleStringValueOrEmptyString
    val imageHash_md5 = Util.imagePixelMD5Hash(al)
    val dicom_zip = {
      val zos = new ToZipOutputStream()
      zos.writeDicom(al, "FloodField.dcm", "AQA")
      zos.finish()
    }

    // @formatter:off
    val floodField = new FloodField(
      floodFieldPK            = None,
      outputPK                = outputPK,
      Rows                    = Rows ,
      Columns                 = Columns,
      ImagePlanePixelSpacingX = ImagePlanePixelSpacingX,
      ImagePlanePixelSpacingY = ImagePlanePixelSpacingY,
      SOPInstanceUID          = SOPInstanceUID,
      StationName             = StationName,
      RTImageDescription      = RTImageDescription,
      StudyID                 = StudyID,
      imageHash_md5           = imageHash_md5,
      dicom_zip               = dicom_zip
    )
    // @formatter:on
    floodField
  }

}
