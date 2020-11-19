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

object BBbyCBCTCoarseCenter extends Logging {

  /**
   * After performing a histogram on a CBCT image, discard the lowest values as noise.  This number
   * determines the list of pixel values that should be discarded.  Slices that contain the cube
   * have been observed to have ~600 different values.
   */
  // private def CBCTHistogramNoiseLevel = 50

  /**
   * Tuning parameter.  Determines how close two centers must be to consider them to
   * both be the center of the same thing (hopefully the cube).
   */
  val Config_CBCTProximityGrouping_mm = 4.0

  /**
   * Tuning parameter.  Determines how close two centers must be to consider them to
   * both be the center of the same thing (hopefully the cube).
   */
  val Config_CBCTPercentToleranceToCubeSize = 20.0

  private def d2i(d: Double): Int = d.round.toInt

  def getCoarseVerticalCenter_vox(entireVolume: DicomVolume, voxSize_mm: Point3d, toHu: Float => Float): Option[Int] = {

    logger.info("Finding CBCT BB coarse center.  " +
      "Volume size XYZ: " + entireVolume.xSize + entireVolume.ySize + entireVolume.zSize +
      "voxel size mm XYZ: " + voxSize_mm)

    /** Size of cube in pixels. */
    val cubeSize_pix = new Point3d(
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX,
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY,
      Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)

    /** Keep a cache of previously calculated results for efficiency. */
    val sliceCache = scala.collection.mutable.Map[Int, IndexedSeq[IndexedSeq[Float]]]()

    /**
     *  Get the voxels in the plane parallel to the table at the given slice index.
     *
     *  This also sets pixels to 'zero' by the following rules:
     *
     *     'zero' is the value of the lowest valued pixel in the image.  It is usually 0.
     *
     *     Zero out
     */
    def horizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {

      /** Number of pixels to make up the area of the cube in a horizontal slice. */
      val numberOfCubePixels = cubeSize_pix.getX * cubeSize_pix.getZ * (1.0 + (Config.DailyQACBCTCubeSizePercentTolerance / 100.0))

      def makeSlice = {
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

      sliceCache.get(sliceIndex) match {
        case Some(slice) => slice
        case _ => {
          sliceCache.put(sliceIndex, makeSlice)
          sliceCache(sliceIndex)
        }
      }
    }

    if (false) { // For debug only.  Dump the horizontal images as png and their pixel values as text.
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

    def edgeSize(sumList: Seq[Float], index: Int, plateauSize_pix: Int): Float = {
      val left = sumList.take(index).takeRight(plateauSize_pix)
      val right = sumList.drop(index).take(plateauSize_pix)
      right.sum - left.sum
    }

    /**
     * Determine if the given band of pixels contain a pair of edges the proper
     * distance apart that correspond to the size of the cube.  If so, return the
     * center position of the cube.  If not, return None.
     */
    def bandContainsCube(band: Seq[Float], cubeSize_pix: Int): Option[Double] = {
      //      val plateauSize_pix = d2i(cubeSize_pix * cubePlateauFraction)
      //
      //      val edgeList = (plateauSize_pix until (band.size - plateauSize_pix)).map(index => (index, edgeSize(band, index, plateauSize_pix)))
      //      val rising = edgeList.maxBy(_._2)._1 // index of rising edge
      //      val falling = edgeList.minBy(_._2)._1 // index of falling edge
      //      val length = falling - rising
      //      val cubeDiff = cubeSize_pix * (Config.DailyQACBCTCubeSizePercentTolerance / 100.0)
      //
      //      if (length != 0)
      //        Trace.trace("very interesting")
      //      val min = cubeSize_pix - cubeDiff
      //      val max = cubeSize_pix + cubeDiff
      //      val ok = (length > min) && (length < max)
      //      if (ok) {
      //        val center = (rising + falling) / 2.0
      //        Some(center)
      //      } else None

      val zero = band.min
      val hasNonZero = band.find(_ != zero)

      case class EdgePair(rising: Int, falling: Int) {
        val center = (rising + 1 + falling) / 2.0
        val length = falling - rising
      }

      val result = {
        if (hasNonZero.isEmpty)
          None
        else {

          val risingList = band.indices.dropRight(1).filter(index => (band(index) == zero) && (band(index + 1) != zero))
          val fallingList = band.indices.dropRight(1).filter(index => (band(index) != zero) && (band(index + 1) == zero))
          val cubeDiff = cubeSize_pix * (Config.DailyQACBCTCubeSizePercentTolerance / 100.0)

          val min = cubeSize_pix - cubeDiff
          val max = cubeSize_pix + cubeDiff

          val edgePairList = risingList
            .map(r => (r, fallingList.find(f => f > r))).
            filter(p => p._2.isDefined).
            map(p => new EdgePair(p._1, p._2.get)).
            filter(edgePair => (edgePair.length >= min) && (max >= edgePair.length))

          val r = if (edgePairList.isEmpty)
            None
          else
            Some(edgePairList.head.center)
          r
          // ----------------------------------------------------------
          // ----------------------------------------------------------

          //        val length = band.drop(rising).indexWhere(v => v == zero)
          //        if ((length >= min) && (max >= length))
          //          Some((length / 2.0) + rising)
          //        else
          //          None
        }
      }
      result
    }

    /**
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
    def getProximalGroup(centerSeq: Seq[Double], bandSpacing_mm: Double, centerSpacing_mm: Double, sliceIndex: Int): Option[Double] = {

      val cubeSizeY_vox = Config.DailyQAPhantomCubeSize_mm / bandSpacing_mm
      val pixTolerance = (Config_CBCTPercentToleranceToCubeSize / 100.0) * cubeSizeY_vox

      // Minimum and maximum number of centers required for a group to describe the cube.
      val minCount = cubeSizeY_vox - pixTolerance
      val maxCount = cubeSizeY_vox + pixTolerance

      // For two centers to be considered close to each other, they must be no farther apart than this.
      val proximity_vox = Config_CBCTProximityGrouping_mm / centerSpacing_mm

      case class Proximal(count: Int, center: Double);

      // for each center, determine how many other centers it is near to.  Also get average of their centers.
      def getProximalSeq = {
        val ps = centerSeq.map(center => new Proximal(1, center))

        def proxTo(index: Int) = {
          val center = centerSeq(index)
          val cntrLst = centerSeq.filter(c => (c - center).abs < proximity_vox)
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

    def containsCubeHorz(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))
      if (sliceIndex == 244)
        Trace.trace("very interesting")

      // make a list of all of the centers of edge pairs that are close to the cube size
      val centerSeq = dicomImage.pixelData.map(band => bandContainsCube(band, d2i(cubeSize_pix.getZ))).flatten

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
    def containsCubeVert(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

      def toVerticalColumn(offset: Int): IndexedSeq[Float] = (0 until entireVolume.xSize).map(y => dicomImage.get(offset, y))

      // make a list of all of the centers of edge pairs that are close to the cube size
      val vertBandList = (0 until entireVolume.zSize).map(toVerticalColumn)
      val centerSeq = vertBandList.map(band => bandContainsCube(band, d2i(cubeSize_pix.getX))).flatten

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
    def containsCubeRaw(sliceIndex: Int): Option[Point2d] = {
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

    if (false) { // TODO rm
      Trace.trace("all: begin")
      (0 until entireVolume.ySize).map(sliceIndex => {
        val h = containsCubeHorz(sliceIndex)
        val v = containsCubeVert(sliceIndex)
        Trace.trace("sliceIndex: " + sliceIndex + "    h: " + h + "    v: " + v)
      })
      Trace.trace("all: done")
    }

    /** Keep a cache of previously calculated results for efficiency. */
    val containsCubeCache = scala.collection.mutable.Map[Int, Option[Point2d]]()

    def containsCube(sliceIndex: Int): Option[Point2d] = {
      containsCubeCache.get(sliceIndex) match {
        case Some(cc) => cc
        case _ => {
          containsCubeCache.put(sliceIndex, containsCubeRaw(sliceIndex))
          containsCubeCache(sliceIndex)
        }
      }
    }

    def findVeryFirst(sliceIndex: Int): Int = {
      if (containsCube(sliceIndex - 1).isDefined)
        findVeryFirst(sliceIndex - 1)
      else
        sliceIndex
    }

    /**
     * Descend vertically through the volume looking for a horizontal slice containing the cube.  For
     * speed, skip down 1/3 of the cube height at at time.  After one of the topmost is found, another
     * function will find the very topmost.
     */
    def findOneOfFirst(sliceIndex: Int): Option[Int] = {
      if (containsCube(sliceIndex).isDefined)
        Some(sliceIndex)
      else {
        val next = sliceIndex + d2i(cubeSize_pix.getY / 2)
        if (next < entireVolume.ySize)
          findOneOfFirst(next)
        else
          None
      }
    }

    /**
     * Check multiple points near the center to make sure that they are all defined. If the cube is
     * actually there, then it should be findable in multiple slices near the vertical center.
     */
    def checkMultiplePoints(vertCenterSliceIndex: Int): Option[Int] = {
      val partialCube = d2i(cubeSize_pix.getY / 5)
      val centerList = (vertCenterSliceIndex - partialCube until vertCenterSliceIndex + partialCube).map(sliceIndex => (sliceIndex, containsCube(sliceIndex)))

      // if any of the slices near the center do not describe the cube, then fail
      val bad = centerList.find(c => c._2.isEmpty).isDefined
      if (bad)
        None
      else {
        val xList = centerList.map(c => c._2.get.getX)
        val yList = centerList.map(c => c._2.get.getY)

        def isConsistent(list: Seq[Double]) = {
          val requiredStdDev = 0.2 // require the centers to have this standard deviation or less in both coordinates
          val mean = list.sum / list.size
          val stdDev = ImageUtil.stdDev(list.map(_.toFloat))
          (stdDev / mean) < requiredStdDev
        }

        if (isConsistent(xList) && isConsistent(yList))
          Some(vertCenterSliceIndex)
        else {
          logger.info("Found something that resembled the center, but not consistently: " + centerList.map(_._2.get).mkString("    "))
          None
        }
      }
    }

    // Find vertical top of the cube.  If found, then get the vertical center by jumping down 1/2 cube.
    val centerSliceIndex = findOneOfFirst(0) match {
      case Some(sliceIndex) => {
        val top = findVeryFirst(sliceIndex)
        Some(d2i(top + (cubeSize_pix.getY / 2)))
      }
      case _ => None
    }

    centerSliceIndex match {
      case Some(sliceIndex) => checkMultiplePoints(sliceIndex)
      case _ => None
    }

    centerSliceIndex
  }

}