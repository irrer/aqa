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
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
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
                       KVP: Double, // energy level
                       FlatteningFilterFree: Boolean, // true if this is an FFF (flattening filter free)
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
      "    KVP: " + KVP + "\n" +
      "    FlatteningFilterFree: " + FlatteningFilterFree + "\n" +
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

  private def doubleOf(tag: AttributeTag): Double = dicom.get(tag).getDoubleValues.head

  /** For converting raw pixel values into scaled pixel values. */
  lazy val RescaleSlope: Double = doubleOf(TagByName.RescaleSlope)

  /** For converting raw pixel values into scaled pixel values. */
  lazy val RescaleIntercept: Double = doubleOf(TagByName.RescaleIntercept)

  /** Mean value of scaled pixels. */
  lazy val meanScaledPixelValue: Double = FloodField.calculateMean(dicom)

  lazy val dicomImageRaw: DicomImage = new DicomImage(dicom)

  /** Image with pixel values scaled by dividing each pixel by the mean of the entire image. */
  lazy val dicomImageNormalized: DicomImage = FloodField.makeNormalizedFF(dicom)
}

object FloodField extends Logging {
  class FloodFieldTable(tag: Tag) extends Table[FloodField](tag, "floodField") {

    def floodFieldPK = column[Long]("floodFieldPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def Rows = column[Int]("Rows")

    def Columns = column[Int]("Columns")

    def ImagePlanePixelSpacingX = column[Double]("ImagePlanePixelSpacingX")

    def ImagePlanePixelSpacingY = column[Double]("ImagePlanePixelSpacingY")

    def KVP = column[Double]("KVP")

    def FlatteningFilterFree = column[Boolean]("FlatteningFilterFree")

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
        KVP,
        FlatteningFilterFree,
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
   * Also require it to specify the machine as an extra precaution against using a flood field from the wrong machine.
   *
   * @param machinePK Specify machine.
   * @param imageHash For this hash
   */
  def getByImageHash(machinePK: Long, imageHash: String): Seq[FloodField] = {
    val action = for {
      output <- Output.query if output.machinePK === machinePK
      inst <- FloodField.query if (inst.imageHash_md5 === imageHash) && inst.outputPK === output.outputPK
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
   * @param kvp                     Energy level
   * @param fff                     True if Flattening Filter Free
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
                   kvp: Double,
                   fff: Boolean,
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
        (ff.KVP === kvp) &&
        (ff.FlatteningFilterFree === fff) &&
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
   * Find the percentage of rise in the profile via: (hi - lo) / hi
   *
   * @param profile either X or Y image profile
   * @return Percent rise.
   */
  private def profilePercentRise(profile: Seq[Float]): Double = {
    val borderPercent = 5.0 // drop this percent of pixels off each end from the profiles to ignore edge effects
    val samplePercent = 10.0 // take this percent of the profile to get a statistically sufficient number of points to be reliable

    val borderCount: Int = ((borderPercent / 100) * profile.size).round.toInt // drop this number of pixels off each end from the profiles to ignore edge effects
    val sampleCount: Int = ((samplePercent / 100) * profile.size).round.toInt // take this number of the profile to get a statistically sufficient number of points to be reliable

    val lo: Float = profile.slice(borderCount, borderCount + sampleCount).sum / sampleCount
    val hi: Float = profile.dropRight(borderCount).takeRight(sampleCount).sum / sampleCount

    val edgeMean: Float = (lo + hi) / 2

    val center: Float = {
      val dropCount = (profile.size / 2) - (sampleCount / 2)
      profile.slice(dropCount, dropCount + sampleCount).sum / sampleCount
    }

    val percentRise = ((center - edgeMean) / center) * 100
    percentRise
  }

  def xProfilePercentRise(al: AttributeList): Double = {
    val di = new DicomImage(al)
    profilePercentRise(di.columnSums)
  }

  def yProfilePercentRise(al: AttributeList): Double = {
    val di = new DicomImage(al)
    profilePercentRise(di.rowSums)
  }

  /** If the center of the image is raised by this percent or more, then assume it is an FFF image. */
  val fffPercentChange: Double = 15.0 // value in center must be at least this percent higher than ends to qualify as an FFF

  /**
   * Look at the profile of the image to determine whether it is FFF.  There is no way to determine
   * this directly from the DICOM, so this is the best we can do.  FFF fields have a notable rise in
   * the center, whereas non-FFF are nearly flat.
   *
   * @param al DICOM image.
   * @return True if it is FFF.
   */
  def isFFF(al: AttributeList): Boolean = {

    val is = //
      (xProfilePercentRise(al) > fffPercentChange) &&
        (yProfilePercentRise(al) > fffPercentChange)

    is
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

    val KVP = DicomUtil.findAllTag(al, TagByName.KVP).head.getDoubleValues.head

    // @formatter:off
    val floodField = new FloodField(
      floodFieldPK            = None,
      outputPK                = outputPK,
      Rows                    = Rows ,
      Columns                 = Columns,
      ImagePlanePixelSpacingX = ImagePlanePixelSpacingX,
      ImagePlanePixelSpacingY = ImagePlanePixelSpacingY,
      KVP                     = KVP,
      FlatteningFilterFree    = isFFF(al),
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

  /**
   * Calculate the scaled mean value of the pixels.
   *
   * Do everything in Double precision to match the math of the PSM generator written in Matlab.
   *
   * @param al For this rtimage.
   * @return Mean of scaled pixel values.
   */
  private def calculateMean(al: AttributeList): Double = {

    def doubleOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head

    val slope = doubleOf(TagByName.RescaleSlope)
    val intercept = doubleOf(TagByName.RescaleIntercept)

    val img = new DicomImage(al)

    val dbl = img.pixelData.map(row => row.map(p => (p * slope) + intercept))

    val mean = dbl.flatten.sum / (img.width * img.height)

    mean
  }

  /**
   * Make a normalized version of the flood field.  This is done by dividing each pixel by the mean of all the FF pixels.
   *
   * Some care is done to do this in Double precision, because when summing the pixels with Float, some precision is lost.
   *
   * @param al: raw DICOM image
   * @return New image.
   */
  private def makeNormalizedFF(al: AttributeList): DicomImage = {

    def doubleOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head

    val slope = doubleOf(TagByName.RescaleSlope)
    val intercept = doubleOf(TagByName.RescaleIntercept)

    val img = new DicomImage(al)

    val dbl = img.pixelData.map(row => row.map(p => (p * slope) + intercept))

    val mean = dbl.flatten.sum / (img.width * img.height)

    val flt = dbl.map(row => row.map(p => (p / mean).toFloat))

    new DicomImage(flt)
  }

}
