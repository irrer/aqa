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

package org.aqa.webrun.bbByCBCT

import edu.umro.ImageUtil.DicomVolume
import javax.vecmath.Point3d
import java.awt.image.BufferedImage
import javax.vecmath.Point3i
import edu.umro.ImageUtil.DicomImage
import java.awt.geom.Point2D
import java.awt.Rectangle
import org.aqa.Config
import edu.umro.ImageUtil.ImageUtil

/**
 * Create images in the three axis.  Each image is the sum of multiple planes nearest the BB.
 */
object BBbyCBCTMakeImageXYZ {

  /**
   * Get image from the perspective of the X-axis.
   */
  private def xImage(entireVolume: DicomVolume, start: Point3i, size: Point3i): DicomImage = {
    val vol = entireVolume.getSubVolume(new Point3i(start.getX, 0, 0), new Point3i(size.getX, entireVolume.ySize, entireVolume.zSize))
    def sumOf(y: Int, z: Int): Float = {
      val xx = for (x <- 0 until vol.xSize) yield (vol.getXYZ(x, y, z))
      (xx.sum) / vol.xSize
    }

    def rowOf(y: Int): IndexedSeq[Float] = {
      for (z <- 0 until vol.zSize) yield (sumOf(y, z))
    }

    val grid = for (y <- 0 until vol.ySize) yield (rowOf(y)).toIndexedSeq
    val image = new DicomImage(grid)
    image
  }

  /**
   * Get image from the perspective of the Y-axis.
   */
  private def yImage(entireVolume: DicomVolume, start: Point3i, size: Point3i): DicomImage = {
    val vol = entireVolume.getSubVolume(new Point3i(0, start.getY, 0), new Point3i(entireVolume.xSize, size.getY, entireVolume.zSize))

    def sumOf(x: Int, z: Int): Float = {
      val yy = for (y <- 0 until vol.ySize) yield (vol.getXYZ(x, y, z))
      (yy.sum) / vol.xSize
    }

    def rowOf(x: Int): IndexedSeq[Float] = {
      for (z <- 0 until vol.zSize) yield (sumOf(x, z))
    }

    val grid = for (x <- 0 until vol.xSize) yield (rowOf(x)).toIndexedSeq
    val image = new DicomImage(grid)
    image
  }

  /**
   * Get image from the perspective of the Z-axis.
   */
  private def zImage(entireVolume: DicomVolume, start: Point3i, size: Point3i): DicomImage = {
    val vol = entireVolume.getSubVolume(new Point3i(0, 0, start.getZ), new Point3i(entireVolume.xSize, entireVolume.ySize, size.getZ))
    def sumOf(x: Int, y: Int): Float = {
      val zz = for (z <- 0 until vol.zSize) yield (vol.getXYZ(x, y, z))
      (zz.sum) / vol.xSize
    }

    def rowOf(y: Int): IndexedSeq[Float] = {
      for (x <- 0 until vol.xSize) yield (sumOf(x, y))
    }

    val grid = for (y <- 0 until vol.xSize) yield (rowOf(y)).toIndexedSeq
    val image = new DicomImage(grid)

    image
  }

  /**
   * Establish the min and max values by removing the outliers.  This is used for choosing the
   * color range of the images.
   *
   * Note that the images will be presented together, so the aggregate range of the pixels should be
   * considered so that the images will be colored similarly.
   */
  private def getMinMax(imageList: Seq[DicomImage]): (Float, Float) = {
    val allHist = imageList.map(img => img.histogram).flatten.groupBy(hp => hp.value)
    val histogram = allHist.map(vhp => new DicomImage.HistPoint(vhp._1, vhp._2.map(hp => hp.count).sum)).toSeq.sortBy(_.value)
    val totalSize = histogram.map(_.count).sum
    val numDrop = {
      if (histogram.size < 1000)
        0
      else
        (totalSize * 0.005).round.toInt
    }

    /**
     * Keep removing members until the requisite number has been dropped.
     */
    def trim(total: Int, hist: Seq[DicomImage.HistPoint]): Float = {
      if (hist.size == 1) hist.head.value
      else {
        if (total < numDrop)
          trim(total + hist.head.count, hist.tail)
        else hist.head.value
      }
    }

    val minPix = trim(0, histogram)
    val maxPix = trim(0, histogram.reverse)

    (minPix, maxPix)
  }

  /**
   * Make an annotated image.
   *
   * @param dicomImage Raw image, not corrected for aspect ratio.
   *
   * @param location Position of BB.
   *
   * @param pixelSize Dimensions of pixels in mm.
   *
   * @param minMax Minimum and maximum values to be used for rendering deep color.
   */
  private def makeImage(dicomImage: DicomImage, location: Point2D.Double, pixelSize: Point2D.Double, minMax: (Float, Float)): BufferedImage = {
    val aspectCorrected = dicomImage.renderPixelsToSquare(pixelSize.getX, pixelSize.getY)
    val bufImg =
      {
        if (Config.CBCTImageColor.getRGB == 0)
          aspectCorrected.toDeepColorBufferedImage(0.0)
        else {
          aspectCorrected.toBufferedImage(Config.CBCTImageColor)
          val maxPixelValue = dicomImage.getSubArray(new Rectangle((location.getX - 5).toInt, (location.getY - 5).toInt, 10, 10)).flatten.max
          aspectCorrected.toBufferedImage(ImageUtil.rgbColorMap(Config.CBCTImageColor), 0.0.toFloat, maxPixelValue.toFloat)
        }
      }
    bufImg
  }

  /**
   * Make images that show the BB from the X, Y and Z axis by taking a slice a 4 times thicker than
   * the BB in the given direction that encompass the BB.  The point is to be able to generate
   * images that show the BB but minimizing noise by eliminating the volume in front of and behind
   * the BB (from the 3 orthogonal axis).   The voxels are averaged in along the relevant axis to
   * produce a 2-D image.
   */
  def makeImagesXYZ(entireVolume: DicomVolume, fineLocation_vox: Point3d, voxSize_mm: Point3d): Seq[BufferedImage] = {
    val mm = (Config.DailyQAPhantomCubeSize_mm * 1.1) / 2

    // point closest to origin for each sub-volume.  Make sure this is within the boundaries of the volume.
    val start = new Point3i(
      Math.max(0, (fineLocation_vox.getX - (mm / voxSize_mm.getX)).ceil.toInt),
      Math.max(0, (fineLocation_vox.getY - (mm / voxSize_mm.getY)).ceil.toInt),
      Math.max(0, (fineLocation_vox.getZ - (mm / voxSize_mm.getZ)).ceil.toInt))

    // number of planes in each sub-volume
    val maxSize = new Point3i(
      ((mm * 2) / voxSize_mm.getX).ceil.toInt,
      ((mm * 2) / voxSize_mm.getY).ceil.toInt,
      ((mm * 2) / voxSize_mm.getZ).ceil.toInt)

    // number of planes in each sub-volume  Make sure this is within the boundaries of the volume.
    val size = new Point3i(
      Math.min(entireVolume.xSize - start.getX, maxSize.getX),
      Math.min(entireVolume.ySize - start.getY, maxSize.getY),
      Math.min(entireVolume.zSize - start.getZ, maxSize.getZ))

    // these are CPU intensive, so process in parallel for speed
    val imageList = Seq(xImage _, yImage _, zImage _).par.map(im => im(entireVolume, start, size)).toList

    val minMaxPixels = getMinMax(imageList)
    val bufImgList = Seq(
      makeImage(imageList(0), new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getY), new Point2D.Double(voxSize_mm.getZ, voxSize_mm.getY), minMaxPixels),
      makeImage(imageList(1), new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getX), new Point2D.Double(voxSize_mm.getZ, voxSize_mm.getX), minMaxPixels),
      makeImage(imageList(2), new Point2D.Double(fineLocation_vox.getX, fineLocation_vox.getY), new Point2D.Double(voxSize_mm.getX, voxSize_mm.getY), minMaxPixels))

    bufImgList
  }

}