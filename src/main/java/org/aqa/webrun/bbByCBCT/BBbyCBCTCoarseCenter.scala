package org.aqa.webrun.bbByCBCT

import org.aqa.Logging
import edu.umro.ImageUtil.DicomVolume
import javax.vecmath.Point3d
import javax.vecmath.Point3i
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import javax.vecmath.Point2d
import edu.umro.ImageUtil.ImageUtil
import java.text.SimpleDateFormat

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
    new Point3d(
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX,
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY,
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)
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
    val numberOfCubePixels = cubeSize_pix.getX * cubeSize_pix.getZ

    // grab a horizontal slice (perpendicular to the Y axis)
    val s = (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))

    def getCubeLevel(count: Int, hst: Seq[DicomImage.HistPoint]): Float = {
      if ((count > numberOfCubePixels) || (hst.size == 1)) hst.head.value
      else {
        getCubeLevel(count + hst.head.count, hst.tail)
      }
    }

    val di = new DicomImage(s)
    val cubeLevel = getCubeLevel(0, di.histogram.reverse)

    // Remove low level noise by settin the N lowest values to the zero value.
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
      case _ => {
        sliceCache.put(sliceIndex, makeHorizontalSlice(sliceIndex))
        sliceCache(sliceIndex)
      }
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

    case class EdgePair(rising: Int, falling: Int) {
      /** Length is position of first non-zero pixel to last divided by 2. */
      val center = (rising + 1 + falling) / 2.0
      val length = falling - rising
    }

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

        def inRange(r: Int, f: Int) = {
          val length = f - r
          (length >= min) && (max >= length)
        }

        val centerList = for (r <- risingList; f <- fallingList; if inRange(r, f)) yield ((r + 1 + f) / 2.0)
        centerList.headOption

        //        // establish the limits  that determine whether or not and edge pair's length is the proper.
        //
        //        val edgePairList = risingList
        //          .map(r => (r, fallingList.find(f => f > r))). // For each rising edge, find the corresponding falling edge.  This will be the first in the falling edge list that has an index greater than the rising edge.
        //          filter(p => p._2.isDefined). // Discard any rising edges that do not have a corresponding falling edge. This can happen if the last pixel in the band is non-zero.
        //          map(p => new EdgePair(p._1, p._2.get)). // Convert to convenience class.
        //          filter(edgePair => (edgePair.length >= min) && (max >= edgePair.length)) // Filter out pairs that are not the proper length.
        //
        //        val r = if (edgePairList.isEmpty)
        //          None // no edge pairs found, so nothing.
        //        else
        //          Some(edgePairList.head.center) // Found at least one good pair.  Return the center of the first one.
        //        r
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

    case class Proximal(count: Int, center: Double);

    // for each center, determine how many other centers it is near to.  Also get average of their centers.
    def getProximalSeq = {
      val ps = centerSeq.map(center => new Proximal(1, center))

      def proxTo(index: Int) = {
        val center = centerSeq(index)
        val cntrLst = centerSeq.filter(c => (c - center).abs <= proximity_vox)
        new Proximal(cntrLst.size, cntrLst.sum / cntrLst.size)
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
    val centerSeq = dicomImage.pixelData.map(band => bandContainsCube(band, cubeSize_pix.getZ)).flatten

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
  private def containsCubeVert(sliceIndex: Int): Option[Double] = {
    val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

    def toVerticalColumn(offset: Int): IndexedSeq[Float] = (0 until entireVolume.xSize).map(y => dicomImage.get(offset, y))

    // make a list of all of the centers of edge pairs that are close to the cube size
    val vertBandList = (0 until entireVolume.zSize).map(toVerticalColumn)
    val centerSeq = vertBandList.map(band => bandContainsCube(band, cubeSize_pix.getX)).flatten

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
      case Some(h) => {
        containsCubeVert(sliceIndex) match {
          case Some(v) => {
            Some(new Point2d(h, v))
          }
          case _ => None
        }
      }
      case _ => None
    }
  }

  /** Keep a cache of previously calculated results for efficiency. */
  private val containsCubeCache = scala.collection.mutable.Map[Int, Option[Point2d]]()

  private def containsCube(sliceIndex: Int): Option[Point2d] = {
    containsCubeCache.get(sliceIndex) match {
      case Some(cc) => cc
      case _ => {
        containsCubeCache.put(sliceIndex, containsCubeRaw(sliceIndex))
        containsCubeCache(sliceIndex)
      }
    }
  }

  private def findVeryFirst(sliceIndex: Int): Option[Int] = {
    val isTop = {
      val searchRange = 5
      val prev = (-searchRange to 0).map(si => containsCube(si)).flatten
      val ok = (prev.size == 0) && containsCube(sliceIndex).isDefined && containsCube(sliceIndex + 1).isDefined
      ok
    }

    if (isTop)
      Some(sliceIndex)
    else if (sliceIndex == 0) None
    else findVeryFirst(sliceIndex - 1)
  }

  /**
   * Descend vertically through the volume looking for a horizontal slice containing the cube.  For
   * speed, skip down 1/3 of the cube height at at time.  After one of the topmost is found, another
   * function will find the very topmost.
   */
  private def findOneOfFirst(sliceIndex: Int): Option[Int] = {
    if (containsCube(sliceIndex).isDefined)
      Some(sliceIndex)
    else {
      val next = sliceIndex + d2i(cubeSize_pix.getY / 4)
      if (next < entireVolume.ySize)
        findOneOfFirst(next)
      else
        None
    }
  }

  /**
   * For debug only.  dump the horizontal images as png and their pixel values as text.
   */
  private def dumpHorizontalSliceImagesAndTextToDisk(entireVolume: DicomVolume) = {
    import java.io.File
    import java.awt.Color
    val date = (new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")).format(new java.util.Date)
    val dir = new File("""D:\tmp\aqa\tmp\cbctSlices\""" + date)
    dir.mkdirs
    logger.info("Created slice image directory: " + dir.getAbsolutePath)
    def writeImg(sliceIndex: Int) = {
      val di = new DicomImage(horizontalSlice(sliceIndex))
      val bi = di.toBufferedImage(Color.white)
      val pngFile = new File(dir, sliceIndex.formatted("%03d") + ".png")
      Util.writePng(bi, pngFile)
      val txtFile = new File(dir, sliceIndex.formatted("%03d") + ".txt")
      Util.writeFile(txtFile, di.pixelsToText)
    }
    (0 until entireVolume.ySize).par.map(writeImg)
    logger.info("Done creating slice image directory: " + dir.getAbsolutePath)
  }

  /**
   * For debug only. For each slice, show whether it contains the cube or not.
   */
  private def showAllSlices(entireVolume: DicomVolume) = {
    logger.info("all: begin")
    (0 until entireVolume.ySize).map(sliceIndex => {
      val h = containsCubeHorz(sliceIndex)
      val v = containsCubeVert(sliceIndex)
      logger.info("sliceIndex: " + sliceIndex + "    h: " + h + "    v: " + v)
    })
    logger.info("all: done")
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
      case Some(p2d) => {
        val point = new Point3i(d2i(p2d.getY), middle, d2i(p2d.getX))
        Some(point)
      }
      case _ => None
    }
  }

  /**
   * Top level processing.  Get the center coordinates of the cube (coincides with BB).  If not found,
   * then return None.
   */
  def getCoarseCenter_vox: Option[Point3i] = {

    logger.info("Finding CBCT BB coarse center.  " +
      "Volume size XYZ: " + entireVolume.xSize + entireVolume.ySize + entireVolume.zSize +
      "voxel size mm XYZ: " + voxSize_mm)

    if (false) dumpHorizontalSliceImagesAndTextToDisk(entireVolume)
    if (false) showAllSlices(entireVolume)

    // Find vertical top of the cube.  If qfound, then get the vertical center by jumping down 1/2 cube.
    findOneOfFirst(0) match {
      case Some(sliceIndex) => {
        findVeryFirst(sliceIndex) match {
          case Some(top) => getMiddle(top)
          case _ => None
        }
      }
      case _ => None
    }
  }

  //  def getCoarseCenter_vox(entVol: DicomVolumeX: Point3d): Option[Point3i] = {
  //
  //    doit(entVolXX)
  //  }

}