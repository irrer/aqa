package org.aqa.db

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

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import javax.vecmath.Point2i
import scala.xml.Elem

case class PixelSensitivityMap(
    pixelSensitivityMapPK: Option[Long], // primary key
    outputPK: Long, // metadata
    psmType: String, // type of PSM
    width_pix: Int, // number of pixels wide (columns)
    height_pix: Int, // number of pixels tall (rows)
    pixelSensitivityMap_array: Array[Byte], // Pixel sensitivity map.  One value for each pixel.
    wholeDetector_array: Array[Byte] // Whole detector, taken directly from the EPID.  One value for each pixel.

    /*
        Naming:
            WD = whole detector. This is the image directly from the EPID
            FF =  flood field. This is the calibration image automatically applied by the LinAC to create the WD image.
            WD*FF =Raw image. This is the EPID image with the flood field image removed.
            PSM = the alternate calibration image to the flood field that we need for QA applications.
            Raw image/PSM = BR (Beam Response). The BR image is the image that we require to perform our symmetry analysis on.

     */

) {

  def this(
      pixelSensitivityMapPK: Option[Long],
      outputPK: Long,
      psmType: String,
      width_pix: Int,
      height_pix: Int,
      pixelSensitivityMapDouble_array: Seq[Seq[Double]],
      wholeDetectorDouble_array: Seq[Seq[Double]]
  ) =
    this(
      pixelSensitivityMapPK,
      outputPK,
      psmType,
      width_pix,
      height_pix,
      PixelSensitivityMap.double2dToByte1d(width_pix, height_pix, pixelSensitivityMapDouble_array),
      PixelSensitivityMap.double2dToByte1d(width_pix, height_pix, wholeDetectorDouble_array)
    )

  def insert: PixelSensitivityMap = {
    val insertQuery = PixelSensitivityMap.query returning PixelSensitivityMap.query.map(_.pixelSensitivityMapPK) into
      ((PSM, pixelSensitivityMapPK) => PSM.copy(pixelSensitivityMapPK = Some(pixelSensitivityMapPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  private def byte1dToDouble2d(byteArray: Array[Byte]): Seq[Seq[Double]] = {
    val in = new ObjectInputStream(new ByteArrayInputStream(byteArray))
    def readRow: Seq[Double] = (0 until width_pix).map(_ => in.readDouble())
    val matrix = (0 until height_pix).map(_ => readRow)
    matrix
  }

  var pixelSensitivityMapAsDouble: Seq[Seq[Double]] = byte1dToDouble2d(pixelSensitivityMap_array)

  var wholeDetectorAsDouble: Seq[Seq[Double]] = byte1dToDouble2d(wholeDetector_array)

  def insertOrUpdate(): Int = Db.run(PixelSensitivityMap.query.insertOrUpdate(this))

  override def toString: String = {
    s"pixelSensitivityMapPK: $pixelSensitivityMapPK    outputPK: $outputPK    psmType: $psmType"
  }

  /** List of pixels that have a value of zero (invalid). */
  private val zeroPixelSet: Set[Point2i] = {
    def isZero(x: Int, y: Int) = { (pixelSensitivityMapAsDouble(y)(x) == 0) || (wholeDetectorAsDouble(y)(x) == 0) }
    val list = for (x <- 0 until width_pix; y <- 0 until height_pix; if isZero(x, y)) yield new Point2i(x, y)
    list.toSet
  }

  private def multiplyByWholeDetector(dicomImage: DicomImage): Seq[Seq[Double]] = {


    def doRow(y: Int) = {
      wholeDetectorAsDouble(y).zip(dicomImage.pixelData(y)).map(wi => wi._1 * wi._2)
    }

    val wdXimg = (0 until height_pix).map(doRow)

    wdXimg
  }

  /**
   * Divide each pixel in the given image by the corresponding pixel in the PSM.
   * @param wdXimg For this image.
   * @return New image map.
   */
  private def divByPsm(wdXimg: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    val zeroSet = {
      val list = for (x <- 0 until width_pix; y <- 0 until height_pix; if wdXimg(y)(x) == 0) yield new Point2i(x, y)
      list.toSet ++ zeroPixelSet
    }

    def divRow(y: Int): Seq[Double] = {
      val wdRow = wdXimg(y)
      val psmRow = pixelSensitivityMapAsDouble(y)

      def divPixel(x: Int): Double = {
        if (zeroSet.contains(new Point2i(x, y))) // avoid division by zero exception
          0
        else
          wdRow(x) / psmRow(x)
      }

      (0 until width_pix).map(divPixel)
    }

    (0 until height_pix).map(divRow)
  }

  private def normalize(divByPsmImage: Seq[Seq[Double]], dicomImage: DicomImage): AttributeList = {

    val sampleSize = height_pix * 2

    val divByPsmOrdered = divByPsmImage.flatten.sorted
    val divByPsmLoMean = divByPsmOrdered.slice(sampleSize, sampleSize + sampleSize).sum / sampleSize
    val divByPsmHiMean = divByPsmOrdered.dropRight(sampleSize).takeRight(sampleSize).sum / sampleSize

    val imageSorted = dicomImage.pixelData.flatten.sorted
    val imageLoMean = imageSorted.slice(sampleSize, sampleSize + sampleSize).sum / sampleSize
    val imageHiMean = imageSorted.dropRight(sampleSize).takeRight(sampleSize).sum / sampleSize


    ???
  }

  /**
    * Apply pixel sensitivity compensation to the given image.  The image is modified, but the metadata is not changed.
    * @param rtimage Apply to this RTIMAGE.
    * @return A new version of the image.
    */
  def applyPsm(rtimage: AttributeList): AttributeList = {
    val dicomImage = new DicomImage(rtimage)

    val wdXimg = multiplyByWholeDetector(dicomImage)

    val beamResponseImage = divByPsm(wdXimg)

    val normalized = normalize(beamResponseImage, dicomImage)
    ???
  }

}

//noinspection ScalaWeakerAccess
object PixelSensitivityMap extends ProcedureOutput {
  class PSMTable(tag: Tag) extends Table[PixelSensitivityMap](tag, "pixelSensitivityMap") {

    def pixelSensitivityMapPK = column[Long]("pixelSensitivityMapPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def psmType = column[String]("psmType")
    def width_pix = column[Int]("width_pix")
    def height_pix = column[Int]("height_pix")
    def pixelSensitivityMap_array = column[Array[Byte]]("pixelSensitivityMap_array")
    def wholeDetector_array = column[Array[Byte]]("wholeDetector_array")

    def * =
      (pixelSensitivityMapPK.?, outputPK, psmType, width_pix, height_pix, pixelSensitivityMap_array, wholeDetector_array) <> (PixelSensitivityMap.apply _ tupled, PixelSensitivityMap.unapply)

    def outputFK =
      foreignKey("PSM_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMTable]

  override val topXmlLabel = "pixelSensitivityMap"

  def get(pixelSensitivityMapPK: Long): Option[PixelSensitivityMap] = {
    val action = for {
      inst <- PixelSensitivityMap.query if inst.pixelSensitivityMapPK === pixelSensitivityMapPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[PixelSensitivityMap] = {
    val action = for {
      inst <- PixelSensitivityMap.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(pixelSensitivityMapPK: Long): Int = {
    val q = query.filter(_.pixelSensitivityMapPK === pixelSensitivityMapPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[PixelSensitivityMap]): Seq[Int] = {
    val ops = list.map { imgId => PixelSensitivityMap.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Insert by elem not implemented.")
  }

  def insertSeq(list: Seq[PixelSensitivityMap]): Unit = {
    val ops = list.map { loc => PixelSensitivityMap.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  private def double2dToByte1d(width: Int, height: Int, doubleMatrix: Seq[Seq[Double]]): Array[Byte] = {
    val bytesOut = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytesOut)
    val doubleArray = doubleMatrix.flatten
    // extra check for array size
    if ((doubleArray.size != (width * height)) || (doubleMatrix.head.size != width) || (doubleMatrix.size != height))
      throw new RuntimeException(
        s"Wrong size for double matrix.  Is of size ${doubleMatrix.head.size} * ${doubleMatrix.size} = ${doubleMatrix.size} but should be $width * $height = ${width * height}"
      )
    doubleArray.foreach(out.writeDouble)
    bytesOut.toByteArray
  }

}
