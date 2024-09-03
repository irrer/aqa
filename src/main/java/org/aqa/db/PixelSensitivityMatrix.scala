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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import javax.vecmath.Point2i

case class PixelSensitivityMatrix(
    pixelSensitivityMatrixPK: Option[Long], // primary key
    outputPK: Long, // metadata
    psmType: String, // type of PSM
    width: Int, // number of pixels wide (columns)
    height: Int, // number of pixels tall (rows)
    pixelSensitivityMatrix_array: Array[Byte], // Pixel sensitivity matrix.  One value for each pixel.
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
      pixelSensitivityMatrixPK: Option[Long],
      outputPK: Long,
      psmType: String,
      width_pix: Int,
      height_pix: Int,
      pixelSensitivityMatrixDouble_array: Seq[Seq[Double]],
      wholeDetectorDouble_array: Seq[Seq[Double]]
  ) =
    this(
      pixelSensitivityMatrixPK,
      outputPK,
      psmType,
      width_pix,
      height_pix,
      PixelSensitivityMatrix.double2dToByte1d(width_pix, height_pix, pixelSensitivityMatrixDouble_array),
      PixelSensitivityMatrix.double2dToByte1d(width_pix, height_pix, wholeDetectorDouble_array)
    )

  def insert: PixelSensitivityMatrix = {
    val insertQuery = PixelSensitivityMatrix.query returning PixelSensitivityMatrix.query.map(_.pixelSensitivityMatrixPK) into
      ((PSM, pixelSensitivityMatrixPK) => PSM.copy(pixelSensitivityMatrixPK = Some(pixelSensitivityMatrixPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  private def byte1dToDouble2d(byteArray: Array[Byte]): Seq[Seq[Double]] = {
    val in = new ObjectInputStream(new ByteArrayInputStream(byteArray))
    def readRow: Seq[Double] = (0 until width).map(_ => in.readDouble())
    val matrix = (0 until height).map(_ => readRow)
    matrix
  }

  private var pixelSensitivityMatrixAsDouble: Seq[Seq[Double]] = byte1dToDouble2d(pixelSensitivityMatrix_array)

  private var wholeDetectorAsDouble: Seq[Seq[Double]] = byte1dToDouble2d(wholeDetector_array)

  def insertOrUpdate(): Int = Db.run(PixelSensitivityMatrix.query.insertOrUpdate(this))

  override def toString: String = {
    s"pixelSensitivityMatrixPK: $pixelSensitivityMatrixPK    outputPK: $outputPK    psmType: $psmType"
  }

  /** List of pixels that have a value of zero (invalid). */
  private val zeroPixelSet: Set[Point2i] = {
    def isZero(x: Int, y: Int) = { (pixelSensitivityMatrixAsDouble(y)(x) == 0) || (wholeDetectorAsDouble(y)(x) == 0) }
    val list = for (x <- 0 until width; y <- 0 until height; if isZero(x, y)) yield new Point2i(x, y)
    list.toSet
  }

  private def multiplyByWholeDetector(dicomImage: DicomImage): Seq[Seq[Double]] = {

    def doRow(y: Int) = {
      wholeDetectorAsDouble(y).zip(dicomImage.pixelData(y)).map(wi => wi._1 * wi._2)
    }

    val wdXimg = (0 until height).map(doRow)

    wdXimg
  }

  /**
    * Divide each pixel in the given image by the corresponding pixel in the PSM.
    * @param wdXimg For this image.
    * @return New image map.
    */
  private def divByPsm(wdXimg: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    val zeroSet = {
      val list = for (x <- 0 until width; y <- 0 until height; if wdXimg(y)(x) == 0) yield new Point2i(x, y)
      list.toSet ++ zeroPixelSet
    }

    def divRow(y: Int): Seq[Double] = {
      val wdRow = wdXimg(y)
      val psmRow = pixelSensitivityMatrixAsDouble(y)

      def divPixel(x: Int): Double = {
        if (zeroSet.contains(new Point2i(x, y))) // avoid division by zero exception
          0
        else
          wdRow(x) / psmRow(x)
      }

      (0 until width).map(divPixel)
    }

    (0 until height).map(divRow)
  }

  private def normalize(divByPsmImage: Seq[Seq[Double]], dicomImage: DicomImage): AttributeList = {

    val sampleSize = height * 2

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
object PixelSensitivityMatrix {
  class PSMTable(tag: Tag) extends Table[PixelSensitivityMatrix](tag, "pixelSensitivityMatrix") {

    def pixelSensitivityMatrixPK = column[Long]("pixelSensitivityMatrixPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def psmType = column[String]("psmType")
    def width_pix = column[Int]("width_pix")
    def height_pix = column[Int]("height_pix")
    def pixelSensitivityMatrix_array = column[Array[Byte]]("pixelSensitivityMatrix_array")
    def wholeDetector_array = column[Array[Byte]]("wholeDetector_array")

    def * =
      (
        pixelSensitivityMatrixPK.?,
        outputPK,
        psmType,
        width_pix,
        height_pix,
        pixelSensitivityMatrix_array,
        wholeDetector_array
      ) <> (PixelSensitivityMatrix.apply _ tupled, PixelSensitivityMatrix.unapply)

    def outputFK =
      foreignKey("PSM_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMTable]

  def get(pixelSensitivityMatrixPK: Long): Option[PixelSensitivityMatrix] = {
    val action = for {
      inst <- PixelSensitivityMatrix.query if inst.pixelSensitivityMatrixPK === pixelSensitivityMatrixPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[PixelSensitivityMatrix] = {
    val action = for {
      inst <- PixelSensitivityMatrix.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(pixelSensitivityMatrixPK: Long): Int = {
    val q = query.filter(_.pixelSensitivityMatrixPK === pixelSensitivityMatrixPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
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
