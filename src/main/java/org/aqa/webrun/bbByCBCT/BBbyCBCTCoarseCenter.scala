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

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.DicomVolume
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.text.SimpleDateFormat
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Point3i
import scala.annotation.tailrec

/**
  * Find the coarse (approximate) center of the BB to within two voxels.
  *
  * This involves doing image processing to remove noise and recognize where the cube is.  Besides
  * the usual problem of image artifacts, it must deal with the possibility that the cube may be
  * appear in various parts of the CBCT volume.
  */
class BBbyCBCTCoarseCenter(entireVolume: DicomVolume, voxSize_mm: Point3d) extends Logging {

  /** Size of cube in pixels. */
  private val cubeSize_pix = {
    new Point3d(Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX, Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY, Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)
  }

  private def d2i(d: Double): Int = d.round.toInt

  /**
    * Get the voxels in the plane parallel to the table at the given slice index.
    *
    * This also sets pixels to 'zero' by the following rules:
    *
    *    'zero' is the value of the lowest valued pixel in the image.  It is usually 0.
    *
    *    Zero out pixels that are near zero.  In a dark image this will zero out all of them.
    *    This is useful to avoid attempts at processing noise.
    *
    *    Starting with the brightest/largest, determine the list of pixel values (levels) it takes
    *    to cover the cube, and remove others.
    */
  private def makeHorizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {

    /** Number of pixels to make up the area of the cube in a horizontal slice. */
    val numberOfCubePixels = cubeSize_pix.getX * cubeSize_pix.getZ * (1 + (Config.DailyQACBCTCubeCrossSectionalAreaPercentTolerance / 100))

    // grab a horizontal slice (perpendicular to the Y axis)
    val s = (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))

    @tailrec
    def getCubeLevel(count: Int, hst: Seq[DicomImage.HistPoint]): Float = {
      if ((count > numberOfCubePixels) || (hst.size == 1)) hst.head.value
      else {
        getCubeLevel(count + hst.head.count, hst.tail)
      }
    }

    val di = new DicomImage(s)
    val cubeLevel = getCubeLevel(0, di.histogram.reverse)

    // Remove low level noise by setting the N lowest values to the zero value.
    val histogram = s.flatten.distinct.sorted
    val zero = histogram.head // use this as the lowest pixel value.  Usually this is zero.
    // getting the cutoff level this way guards against there not being enough levels to accommodate CBCTHistogramNoiseLevel.
    val cutoff = Math.max(histogram.take(Config.DailyQACBCTDarkPixelValueLevels).last, cubeLevel)
    val sii = s.map(row => row.map(pixel => if (cutoff >= pixel) zero else pixel))

    sii
  }

  /** Keep a cache of previously calculated results for efficiency. */
  private val sliceCache = scala.collection.mutable.Map[Int, IndexedSeq[IndexedSeq[Float]]]()

  /**
    * Wrap <code>makeHorizontalSlice</code> with caching.
    */
  private def horizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
    sliceCache.get(sliceIndex) match {
      case Some(slice) => slice
      case _ =>
        sliceCache.put(sliceIndex, makeHorizontalSlice(sliceIndex))
        sliceCache(sliceIndex)
    }
  }

  /*
   * Determine if the given band of pixels contain a pair of edges the proper
   * distance apart that correspond to the size of the cube.  If so, return the
   * center position of the cube.  If not, return None.
   *
   * An edge is defined as changing from zero to non-zero (rising), or the
   * reverse (falling).
   *
   * Bands are a one dimensional array of .
   *
   * This will find all rising-falling edge pairs.  If one or more are the
   * proper distance apart, then the first found will be returned.  Note that
   * if multiple pairs of the proper distance are found then this is probably
   * not the cube anyway, and checks in the perpendicular direction will show
   * this.
   *
   * @param band : Pixels that have originated from either a row or column of pixels in a horizontal slice.
   *
   * @param cubeSize_pix : The cube size in pixels along the length of the band.
   */
  private def bandContainsCube(band: Seq[Float], cubeLen_pix: Double): Option[Double] = {

    val zero = band.min // zero as defined by this band.
    val hasNonZero = band.find(_ != zero) // true if it has pixels that might be interesting.  Totally 'black' bands are quickly ignored.

    val result = {
      if (hasNonZero.isEmpty) // quick check to reject all black bands.
        None
      else {
        // Get list of rising and falling edge indices.
        val risingList = band.indices.dropRight(1).filter(index => (band(index) == zero) && (band(index + 1) != zero))
        val fallingList = band.indices.dropRight(1).filter(index => (band(index) != zero) && (band(index + 1) == zero))

        val cubeDiff_pix = cubeLen_pix * (Config.DailyQACBCTCubeSizePercentTolerance / 100.0) // allowed number of pixels off (error limit)
        val min = cubeLen_pix - cubeDiff_pix
        val max = cubeLen_pix + cubeDiff_pix

        /**
          * Return true if the given segment of the band is approximately the right length and is mostly
          * populated with non-zero voxels.
          */
        def inRangeAndPopulated(r: Int, f: Int) = {
          val length = f - r
          (length >= min) && (max >= length) && band.slice(r, r + length).count(_ != 0) > (length * 0.75)
        }

        val centerList = for (r <- risingList; f <- fallingList; if inRangeAndPopulated(r, f)) yield (r + 1 + f) / 2.0
        centerList.headOption
      }
    }
    result
  }

  /*
   * Get the average of a list of centers of the cube. There must be a sufficient number of them with a similar center to qualify.
   *
   * @param centerSeq: List of centers found by finding pair of leading and falling edges separated by the size of the cube.
   *
   * @param bandSpacing_mm: The width of voxels in mm, determines how many centers are required.
   *
   * @param centerSpacing_mm: The size of voxels in mm that determines how close centers must be to be considered near to each other.
   *
   * @param sliceIndex: Slice index for debug only
   */
  private def getProximalGroup(centerSeq: Seq[Double], bandSpacing_mm: Double, centerSpacing_mm: Double, sliceIndex: Int): Option[Double] = {

    val cubeSizeY_vox = Config.DailyQAPhantomCubeSize_mm / bandSpacing_mm
    val voxTolerance = (Config.DailyQACBCTCubeSizePercentTolerance / 100.0) * cubeSizeY_vox

    // Minimum and maximum number of centers required for a group to describe the cube.
    val minCount = cubeSizeY_vox - voxTolerance
    val maxCount = cubeSizeY_vox + voxTolerance

    // For two centers to be considered close to each other, they must be no farther apart than this number of voxels.
    val proximity_vox = 5

    case class Proximal(count: Int, center: Double) {}

    // for each center, determine how many other centers it is near to.  Also get average of their centers.
    def getProximalSeq = {
      val ps = centerSeq.map(center => Proximal(1, center))

      def proxTo(index: Int) = {
        val center = centerSeq(index)
        val cntrLst = centerSeq.filter(c => (c - center).abs <= proximity_vox)
        Proximal(cntrLst.size, cntrLst.sum / cntrLst.size)
      }

      val proxSeq = centerSeq.indices.map(i => proxTo(i))

      proxSeq
    }

    if (centerSeq.size < minCount)
      None // there are not enough centers to constitute the cube
    else {
      val proximalSeq = getProximalSeq
      def seqMax = proximalSeq.maxBy(_.count)

      // Require there to be the proper number of edges near to each other (aligned).  There should be enough to
      // make up most of the cube, but not much more than the cube.  If there are more than
      // the cube then it is pointing to some long parallel lines which are probably the table.
      val correctCount = (seqMax.count >= minCount) && (maxCount >= seqMax.count)

      if (correctCount)
        Some(seqMax.center)
      else
        None
    }
  }

  /**
    * Determine if the given slice contains the cube by dividing into horizontal bands and
    * looking at the profile of each.  If enough qualify, then return true.
    *
    * -------------------------------
    * |                             |
    * |                             |
    * -------------------------------
    * |                             |
    * |            ...........      |
    * -------------.---------.-------
    * |            .         .      |
    * |            .         .      |
    * -------------.---------.-------
    * |            .         .      |
    * |            ...........      |
    * -------------------------------
    * |                             |
    * |                             |
    * -------------------------------
    *
    *              |<------->|
    *
    *    Look for rising and falling edges of
    *    cube separated by the size of the cube.
    */

  private def containsCubeHorz(sliceIndex: Int): Option[Double] = {
    val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

    // make a list of all of the centers of edge pairs that are close to the cube size
    val centerSeq = dicomImage.pixelData.flatMap(band => bandContainsCube(band, cubeSize_pix.getZ))

    val pg = getProximalGroup(centerSeq, voxSize_mm.getX, voxSize_mm.getZ, sliceIndex)
    pg
  }

  /**
    * Determine if the given slice contains the cube by dividing into vertical bands and
    * looking at the profile of each.  If any qualify, then return true.
    *
    * -------------------------------
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * |    |    |  ..........  |    |  -----
    * |    |    |  . |    | .  |    |    ^
    * |    |    |  . |    | .  |    |    |  Look for rising and falling edges of cube
    * |    |    |  . |    | .  |    |    |  separated by the size of the cube.
    * |    |    |  . |    | .  |    |    v
    * |    |    |  ..........  |    |  -----
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * |    |    |    |    |    |    |
    * -------------------------------
    */
  //noinspection ScalaDocUnclosedTagWithoutParser,ScalaDocParserErrorInspection
  private def containsCubeVert(sliceIndex: Int): Option[Double] = {
    val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

    def toVerticalColumn(offset: Int): IndexedSeq[Float] = (0 until entireVolume.xSize).map(y => dicomImage.get(offset, y))

    // make a list of all of the centers of edge pairs that are close to the cube size
    val vertBandList = (0 until entireVolume.zSize).map(toVerticalColumn)
    val centerSeq = vertBandList.flatMap(band => bandContainsCube(band, cubeSize_pix.getX))

    val pg = getProximalGroup(centerSeq, voxSize_mm.getZ, voxSize_mm.getX, sliceIndex)
    pg
  }

  /**
    * Require the image to contain bands oriented in both the vertical and horizontal
    * directions that contain pairs of rising and falling edges separated by the size
    * of the cube to consider the slice to contain the cube.
    *
    * If found, return the center coordinates.
    */
  private def containsCubeRaw(sliceIndex: Int): Option[Point2d] = {
    containsCubeHorz(sliceIndex) match {
      case Some(h) =>
        containsCubeVert(sliceIndex) match {
          case Some(v) =>
            Some(new Point2d(h, v))
          case _ => None
        }
      case _ => None
    }
  }

  /** Keep a cache of previously calculated results for efficiency. */
  private val containsCubeCache = scala.collection.mutable.Map[Int, Option[Point2d]]()

  private def containsCube(sliceIndex: Int): Option[Point2d] = {
    containsCubeCache.get(sliceIndex) match {
      case Some(cc)            => cc
      case _ if sliceIndex < 0 => None
      case _ =>
        containsCubeCache.put(sliceIndex, containsCubeRaw(sliceIndex))
        containsCubeCache(sliceIndex)
    }
  }

  /**
    * From the given 'found' slice, incrementally ascend the cube examining each slice position until
    * there are several consecutive non-cube slices above, and a minimum number below.
    */
  @tailrec
  private def findVeryFirst(sliceIndex: Int): Option[Int] = {
    val isTop = {
      val searchRange = 6 // Consider this many slices above and below the point of examination.
      val minRequired = 3 // Of the slices below the point of examination, require this many to contain the cube.
      val prev = (sliceIndex - searchRange until sliceIndex).flatMap(si => containsCube(si))
      val next = (0 until (sliceIndex + searchRange)).flatMap(si => containsCube(si))
      val ok = prev.isEmpty && (next.size >= minRequired)
      ok
    }

    if (isTop)
      Some(sliceIndex)
    else if (sliceIndex == 0) None
    else findVeryFirst(sliceIndex - 1)
  }

  /**
    * Descend vertically through the volume looking for a horizontal slice containing the cube.  For
    * speed, skip down a fraction of the cube height at at time as opposed to examining every slice.
    * The skip size should not be too large in case one of the slices in the cube is not recognized
    * as such.  After one of the topmost is found, another function will find the very topmost.
    */
  @tailrec
  private def findOneOfFirst(sliceIndex: Int): Option[Int] = {
    val cubeSkipFraction = 0.25
    if (containsCube(sliceIndex).isDefined)
      Some(sliceIndex)
    else {
      val next = d2i(sliceIndex + d2i(cubeSize_pix.getY * cubeSkipFraction))
      if (next < entireVolume.ySize)
        findOneOfFirst(next)
      else
        None
    }
  }

  /**
    * For debug only.  dump the horizontal images as png and their pixel values as text.
    */
  private def dumpHorizontalSliceImagesAndTextToDisk(entireVolume: DicomVolume): Unit = {
    import java.awt.Color
    import java.io.File
    val date = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss").format(new java.util.Date)
    val dir = new File("""D:\tmp\aqa\tmp\cbctSlices\""" + date)
    dir.mkdirs
    logger.info("Created slice image directory: " + dir.getAbsolutePath)
    def writeImg(sliceIndex: Int): Unit = {
      val di = new DicomImage(horizontalSlice(sliceIndex))
      val bi = di.toBufferedImage(Color.white)

      val graphics = ImageUtil.getGraphics(bi)
      graphics.setColor(Color.orange)
      val vert = containsCubeVert(sliceIndex)
      if (vert.isDefined) graphics.drawLine(0, d2i(vert.get), di.width, d2i(vert.get))
      val horz = containsCubeHorz(sliceIndex)
      if (horz.isDefined) graphics.drawLine(d2i(horz.get), 0, d2i(horz.get), di.height)

      val pngFile = new File(dir, sliceIndex.formatted("%03d") + ".png")
      Util.writePng(bi, pngFile)
      val txtFile = new File(dir, sliceIndex.formatted("%03d") + ".txt")
      Util.writeFile(txtFile, di.pixelsToText)
    }
    (0 until entireVolume.ySize).par.foreach(writeImg)
    logger.info("Done creating slice image directory: " + dir.getAbsolutePath)
  }

  /**
    * For debug only. For each slice, show whether it contains the cube or not.
    */
  private def showAllSlices(entireVolume: DicomVolume): Unit = {
    logger.info("all: begin")
    var count = 0
    var first = -1
    var last = -1
    (0 until entireVolume.ySize).foreach(sliceIndex => {
      val h = containsCubeHorz(sliceIndex)
      val v = containsCubeVert(sliceIndex)
      logger.info("sliceIndex: " + sliceIndex + "    h: " + h + "    v: " + v)
      if (h.isDefined && v.isDefined) {
        count = count + 1
        if (first == -1) first = sliceIndex
        last = sliceIndex
      }
    })

    logger.info("all: done.  expected count of cube slices: " + Util.fmtDbl(cubeSize_pix.getY) + "    first: " + first + "    last: " + last + "    actual count of cube slices found: " + count)
  }

  /**
    * Require most of the slices near the given one to be valid slices.
    */
  private def nearSlicesContainCube(sliceIndex: Int): Option[Point2d] = {
    val nearList = (sliceIndex - 2 until sliceIndex + 2).map(si => containsCube(si))
    val ok = (nearList.size - nearList.flatten.size).abs <= 1
    if (ok) {
      nearList.flatten.headOption
    } else
      None
  }

  /**
    * Make sure that the middle slice is in the cube and return XYZ coordinates.
    */
  private def getMiddle(top: Int): Option[Point3i] = {
    val middle = d2i(top + (cubeSize_pix.getY / 2))
    nearSlicesContainCube(middle) match {
      case Some(p2d) =>
        val point = new Point3i(d2i(p2d.getY), middle, d2i(p2d.getX))
        Some(point)
      case _ => None
    }
  }

  /**
    * Top level processing.  Get the center coordinates of the cube (coincides with BB).  If not found,
    * then return None.
    */
  def getCoarseCenter_vox: Option[Point3i] = {

    logger.info(
      "Finding CBCT BB coarse center.  " +
        "Volume size XYZ: " + entireVolume.xSize + entireVolume.ySize + entireVolume.zSize +
        "voxel size mm XYZ: " + voxSize_mm
    )

    if (false) dumpHorizontalSliceImagesAndTextToDisk(entireVolume)
    if (false) showAllSlices(entireVolume)

    // Find vertical top of the cube.  If found, then get the vertical center by jumping down 1/2 cube.
    findOneOfFirst(0) match {
      case Some(sliceIndex) =>
        findVeryFirst(sliceIndex) match {
          case Some(top) => getMiddle(top)
          case _         => None
        }
      case _ => None
    }
  }

  //  def getCoarseCenter_vox(entVol: DicomVolumeX: Point3d): Option[Point3i] = {
  //
  //    doit(entVolXX)
  //  }

}
