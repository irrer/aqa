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
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil.ToZipOutputStream
import org.aqa.db.Db.driver.api._
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.sql.Timestamp

/**  */
case class PSM(
    psmPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    imageHash_md5: String, // MD5 hash of image bytes
    floodFieldImageHash_md5: String, // Image hash of flood field from which this was derived
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
      "    imageHash_md5: " + imageHash_md5.take(16) + "...\n" +
      "    floodFieldImageHash_md5: " + floodFieldImageHash_md5.take(16) + "...\n" +
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

  private var floodFieldScaled: Option[DicomImage] = None

  def getFloodFieldScaled: DicomImage = {
    if (floodFieldScaled.isDefined)
      floodFieldScaled.get
    else {
      // get the flood field as a scaled DICOM image.  Do this is separate steps so that if there is an exception it will point to the problem
      val machinePK = Output.get(outputPK).get.machinePK.get
      val ffSeq = FloodField.getByImageHash(machinePK, floodFieldImageHash_md5)
      val ff = ffSeq.head
      val ffDicom = ff.dicom
      val image = new DicomImage(ffDicom)
      val scaled = image.scalePixels(ffDicom)
      floodFieldScaled = Some(scaled) // save for next time
      scaled
    }
  }

}

object PSM extends Logging {
  class PSMTable(tag: Tag) extends Table[PSM](tag, "psm") {

    def psmPK = column[Long]("psmPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def imageHash_md5 = column[String]("imageHash_md5")

    def floodFieldImageHash_md5 = column[String]("floodFieldImageHash_md5")

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
        imageHash_md5,
        floodFieldImageHash_md5,
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
  def makePSM(outputPK: Long, floodFieldImageHash_md5: String, al: AttributeList, xMax_mm: Double, yMax_mm: Double): PSM = {
    val dicom_zip = {
      val zos = new ToZipOutputStream()
      zos.writeDicom(al, "FloodField.dcm", "AQA")
      zos.finish()
    }

    val imageHash_md5 = Util.imagePixelMD5Hash(al)

    val newPSM = PSM(
      psmPK = None,
      outputPK = outputPK,
      imageHash_md5 = imageHash_md5,
      floodFieldImageHash_md5 = floodFieldImageHash_md5,
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

  case class PSMHistory(output: Output, psm: PSM) {}

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
    val history = pairList.map(pair => PSMHistory(pair._1, pair._2))

    // sort by data date
    history.sortBy(_.output.dataDate.get.getTime)
  }

  /**
    * Get the most recent PSM that satisfies all the following conditions:
    *    matches the machine                     AND
    *    is earlier than the dataDate parameter  AND
    *    references a valid flood field          AND
    *    is not too old as dictated by Config.PSMMaxFloodFieldAge_ms/Config.PSMMaxFloodFieldAge_day
    *
    * @param machinePK Match this machine.
    * @param dataDate Older than this date.
    * @param Rows This many rows of pixels in image
    * @param Columns This many columns of pixels in image
    * @param ImagePlanePixelSpacingX_mm This X pixel spacing in mm.
    * @param ImagePlanePixelSpacingY_mm This Y pixel spacing in mm.
    * @return
    */
  //noinspection ScalaWeakerAccess
  def getUsablePsm(
      machinePK: Long,
      dataDate: Timestamp,
      Rows: Int,
      Columns: Int,
      ImagePlanePixelSpacingX_mm: Double,
      ImagePlanePixelSpacingY_mm: Double //
  ): Option[PSM] = {

    val newerThan: Timestamp = new Timestamp(dataDate.getTime - Config.PSMMaxFloodFieldAge_ms)

    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.dataDate < dataDate) && (o.dataDate > newerThan))
      psm <- PSM.query.filter(p => (p.outputPK === output.outputPK) && (p.Rows === Rows) && (p.Columns === Columns))
    } yield {
      (output, psm)
    }

    def isSamePixelSpacing(psm: PSM): Boolean = {
      def isClose(a: Double, b: Double): Boolean = ((a - b) / b).abs < 0.00000001

      isClose(psm.ImagePlanePixelSpacingX, ImagePlanePixelSpacingX_mm) && isClose(psm.ImagePlanePixelSpacingY, ImagePlanePixelSpacingY_mm)
    }

    val opf1 = Db.run(search.result)
    val opf2 = opf1.toList.filter(r => isSamePixelSpacing(r._2)).sortBy(_._1.dataDate.get.getTime).lastOption

    opf2.map(_._2)
  }

  /**
    * Get the most recent PSM that satisfies all the following conditions:
    *    matches the machine                     AND
    *    is earlier than the dataDate parameter  AND
    *    references a valid flood field          AND
    *    is not too old as dictated by Config.PSMMaxFloodFieldAge_ms/Config.PSMMaxFloodFieldAge_day
    *
    * @param machinePK For this machine
    * @param rtimage Extract date, rows, columns and pixel spacing from this image.
    * @return
    */
  def getUsablePsm(machinePK: Long, rtimage: AttributeList): Option[PSM] = {

    val dataDate = new Timestamp(Util.extractDateTimeAndPatientIdFromDicomAl(rtimage)._1.head.getTime)
    val Rows: Int = rtimage.get(TagByName.Rows).getIntegerValues.head
    val Columns: Int = rtimage.get(TagByName.Columns).getIntegerValues.head
    val ImagePlanePixelSpacing = rtimage.get(TagByName.ImagePlanePixelSpacing).getDoubleValues

    getUsablePsm(
      machinePK = machinePK,
      dataDate = dataDate,
      Rows = Rows,
      Columns = Columns,
      ImagePlanePixelSpacingX_mm = ImagePlanePixelSpacing.head,
      ImagePlanePixelSpacingY_mm = ImagePlanePixelSpacing(1)
    )
  }

}
