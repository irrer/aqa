package org.aqa.webrun.bbByCBCT

import scala.xml.Elem
import javax.vecmath.Point3d
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import com.pixelmed.dicom.AttributeList
import org.aqa.db.CollimatorCentering
import org.aqa.webrun.phase2.Phase2Util
import com.pixelmed.dicom.TagFromName
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import org.aqa.Util
import edu.umro.ImageUtil.LocateMax
import edu.umro.ImageUtil.DicomVolume
import javax.vecmath.Point3i
import edu.umro.ScalaUtil.Trace
import java.io.File
import edu.umro.ImageUtil.ImageUtil
import java.awt.image.BufferedImage
import java.awt.geom.Point2D
import org.aqa.VolumeTranslator
import java.awt.Color
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.DicomUtil
import java.text.SimpleDateFormat
import edu.umro.ImageUtil.Profile
import javax.vecmath.Point2d

/**
 * Find the BB in the CBCT volume.
 */

object BBbyCBCTAnalysis extends Logging {

  val minHu = -1000.0 // minimum value for Houndsfield units.  Values below this are ignored as noise. // TODO put in Config

  /**
   * Get the corner of the search volume closest to the origin in voxels.
   */
  private def startOfSearch(entireVolume: DicomVolume, voxSize_mm: Point3d): Point3i = {
    def toStart(dimension: Int, size_mm: Double): Int = {
      val center = dimension / 2.0
      val offset = (Config.DailyPhantomSearchDistance_mm / size_mm) / 2.0
      (center - offset).round.toInt
    }

    val vs = entireVolume.volSize
    new Point3i(
      toStart(vs.getX, voxSize_mm.getX),
      toStart(vs.getY, voxSize_mm.getY),
      toStart(vs.getZ, voxSize_mm.getZ))
  }

  private def multiplyPoints(a: Point3d, b: Point3d) = new Point3d(a.getX * b.getX, a.getY * b.getY, a.getZ * b.getZ)

  private def d2i(d: Double): Int = d.round.toInt

  private def d2i(d: Point3d): Point3i = new Point3i(d2i(d.getX), d2i(d.getY), d2i(d.getZ))

  private def i2d(i: Point3i): Point3d = new Point3d(i.getX.toDouble, i.getY.toDouble, i.getZ)

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

  private def getCoarseHorizontal(profile: Seq[Float], pixSize_mm: Double): Int = {
    val edgeWidth_pix = (Config.PenumbraThickness_mm / pixSize_mm).round.toInt
    val cubeWidth_pix = (Config.DailyQAPhantomCubeSize_mm / pixSize_mm).round.toInt

    /** Index of first pixel and standard deviation of the set.  */
    case class IndexStdDev(index: Int, stdDev: Double, rating: Double = -1);

    def standDev(i: Int) = {
      val pix = profile.drop(i).take(edgeWidth_pix)
      if (pix.size < edgeWidth_pix) new IndexStdDev(i, 0)
      else {
        val avg = pix.sum / pix.size
        IndexStdDev(i, ImageUtil.stdDev(pix) / avg)
      }
    }

    val isdList = profile.indices.map(i => standDev(i))

    /**
     * Get the sum of the standard deviation of points one cube-width apart.
     */
    def rate(isd: IndexStdDev) = {
      val otherIndex = isd.index + cubeWidth_pix
      isdList.find(_.index == otherIndex) match {
        case Some(other) => isd.copy(rating = isd.stdDev + other.stdDev)
        case _ => isd
      }
    }

    val best = isdList.map(isd => rate(isd)).maxBy(_.rating)

    val cntr = best.index + ((edgeWidth_pix + cubeWidth_pix) / 2)
    cntr
  }

  private def getCoarseVerticalCenter_vox(entireVolume: DicomVolume, voxSize_mm: Point3d, toHu: Float => Float): Option[Int] = {

    val cubePlateauFraction = 0.8

    /** Size of cube in pixels. */
    val cubeSize_pix = new Point3i(
      d2i(Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX),
      d2i(Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY),
      d2i(Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ))

    /** One third of height of cube in X axis in pixels. */
    val oneThirdCubeX_pix = d2i((Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX) / 3)

    /** One third of height of cube in Y axis in pixels. */
    val oneThirdCubeY_pix = d2i((Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY) / 3)

    /** One third of height of cube in Z axis in pixels. */
    val oneThirdCubeZ_pix = d2i((Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ) / 3)

    /** Keep a cache of previously calculated results for efficiency. */
    val sliceCache = scala.collection.mutable.Map[Int, IndexedSeq[IndexedSeq[Float]]]()

    /** Get the voxels in the plane parallel to the table at the given slice index. */
    def horizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
      sliceCache.get(sliceIndex) match {
        case Some(slice) => slice
        case _ => {
          val s = (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))
          val sii = s.map(i => i.toIndexedSeq).toIndexedSeq
          sliceCache.put(sliceIndex, sii)
          sii
        }
      }
    }

    def edgeSize(sumList: Seq[Float], index: Int, plateauSize_pix: Int): Float = {
      val left = sumList.take(index).takeRight(plateauSize_pix)
      val right = sumList.drop(index).take(plateauSize_pix)
      right.sum - left.sum
    }

    def bandContainsCube(band: Seq[Float], cubeSize_pix: Int): Option[Double] = {
      val plateauSize_pix = d2i(cubeSize_pix * cubePlateauFraction)

      val edgeList = (plateauSize_pix until (band.size - plateauSize_pix)).map(index => (index, edgeSize(band, index, plateauSize_pix)))
      //      if (edgeList.isEmpty)
      //        Trace.trace
      val rising = edgeList.maxBy(_._2)._1 // index of rising edge
      val falling = edgeList.minBy(_._2)._1 // index of falling edge
      val length = falling - rising
      val cubeDiff = cubeSize_pix * (Config.DailyQACBCTCubeSizePercentTolerance / 100.0)

      val min = cubeSize_pix - cubeDiff
      val max = cubeSize_pix + cubeDiff
      val ok = (length > min) && (length < max)
      if (ok) {
        val center = (rising + falling) / 2.0
        Some(center)
      } else None
    }

    /**
     * Determine if the given slice contains the cube by dividing into horizontal bands and
     * looking at the profile of each.  If any qualify, then return true.
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
    def containsCubeHorzX(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

      def toMeans(i: Int): IndexedSeq[Float] = {
        val rectangle = new Rectangle(0, i, entireVolume.zSize, Math.min(oneThirdCubeX_pix, entireVolume.xSize - i))
        val di = dicomImage.getSubimage(rectangle)
        if (sliceIndex == 242) { // TODO rm
          val j = di.columnSums.map(s => toHu(s / di.height))
          Trace.trace("unfiltered HU: " + j.map(v => Util.fmtDbl(v)).mkString("   "))
        }
        val meanList = di.columnSums.map(s => toHu(s / di.height)).map(m => if (m < minHu) 0.toFloat else m)
        meanList
      }

      val horzBandList = (0 until entireVolume.xSize by oneThirdCubeX_pix).map(i => toMeans(i))

      val hasCube = horzBandList.map(band => bandContainsCube(band, cubeSize_pix.getZ)).flatten
      if (true && (sliceIndex == 242)) { // TODO rm
        println("containsCubeHorz sliceIndex: " + sliceIndex + "    horzBandList.size: " + horzBandList.size)
        horzBandList.map(b => {
          if (b.max > 0) {
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getZ))
            println(b.mkString("  "))
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getZ))
          }
        })
      }

      val ok = hasCube.nonEmpty
      if (sliceIndex == 242) Trace.trace("containsCubeHorz sliceIndex " + sliceIndex.formatted("%3d") + " : " + ok)
      hasCube.headOption
    }

    def containsCubeHorz(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

      val proximity = 4.0 //  pass as parameter TODO

      def toMeans(i: Int): IndexedSeq[Float] = {
        val rectangle = new Rectangle(0, i, entireVolume.zSize, 1)
        val di = dicomImage.getSubimage(rectangle)
        if (sliceIndex == 155) { // TODO rm
          val j = di.columnSums.map(s => toHu(s / 1))
          Trace.trace("unfiltered HU: " + j.map(v => Util.fmtDbl(v)).mkString("   "))
          if (i == 158)
            Trace.trace("hey")
        }
        val meanList = di.columnSums.map(s => toHu(s / 1)).map(m => if (m < minHu) 0.toFloat else m)
        meanList
      }

      val horzBandList = (0 until entireVolume.xSize by 1).map(i => toMeans(i))

      val hasCube = horzBandList.map(band => bandContainsCube(band, cubeSize_pix.getZ)).flatten
      if (false && (sliceIndex == 155)) { // TODO rm
        println("containsCubeHorz sliceIndex: " + sliceIndex + "    horzBandList.size: " + horzBandList.size)
        horzBandList.map(b => {
          if (b.max > 0) {
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getZ))
            println(b.mkString("  "))
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getZ))
          }
        })
      }
      Trace.trace("sliceIndex: " + sliceIndex + "      hasCube.size: " + hasCube.size)

      val ok = {

        val quorum = hasCube.size >= oneThirdCubeX_pix
        def proximal = {
          val seq = scala.collection.mutable.ArrayBuffer[Int]()
          seq.appendAll(hasCube.map(_ => 1))

          val j = seq.size
          val j1 = hasCube.size
          val j2 = seq(0)

          for (a <- (0 until hasCube.size)) {
            for (b <- (a + 1 until hasCube.size)) {
              if ((hasCube(a) - hasCube(b)).abs < proximity) {
                seq(a) = seq(a) + 1
                seq(b) = seq(b) + 1
              }
            }
          }

          val enough = (seq.size > 0) && (seq.max >= oneThirdCubeX_pix)
          if (quorum && (!enough))
            Trace.trace("hey")

          enough
        }

        if (hasCube.isEmpty)
          None
        else {
          val size = hasCube.size // TODO rm

          //Trace.trace("containsCubeHorz " + sliceIndex.formatted("%3d") + "  quorum: " + quorum + "    proximal: " + proximal)
          quorum && proximal
        }
      }
      Trace.trace("containsCubeHorz sliceIndex " + sliceIndex.formatted("%3d") + " : " + ok)
      hasCube.headOption

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
    def containsCubeVertX(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

      def toMeans(offset: Int): IndexedSeq[Float] = {
        val rectangle = new Rectangle(offset, 0, Math.min(oneThirdCubeZ_pix, entireVolume.zSize - offset), entireVolume.xSize)
        val di = dicomImage.getSubimage(rectangle)

        if (sliceIndex == 242) { // TODO rm
          val j = di.rowSums.map(s => toHu(s / di.width))
          Trace.trace("unfiltered HU: " + j.map(v => Util.fmtDbl(v)).mkString("   "))
        }

        val j = di.rowSums
        val maxJ = j.max
        val meanList = di.rowSums.map(s => toHu(s / di.width)).map(m => if (m < minHu) 0.toFloat else m)
        meanList.toIndexedSeq
      }

      val vertBandList = (0 until entireVolume.zSize by oneThirdCubeZ_pix).map(toMeans)
      val hasCube = vertBandList.map(band => bandContainsCube(band, cubeSize_pix.getX)).flatten

      if (true && (sliceIndex == 242)) { // TODO rm
        println("containsCubeVert sliceIndex: " + sliceIndex + "    vertBandList.size: " + vertBandList.size)
        vertBandList.map(b => {
          if (b.max > 0) {
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getX))
            println(b.mkString("  "))
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getX))
          }
        })
      }

      val ok = hasCube.nonEmpty
      //logger.info("containsCubeVert sliceIndex " + sliceIndex.formatted("%3d") + " : " + ok)
      hasCube.headOption
    }

    def containsCubeVert(sliceIndex: Int): Option[Double] = {
      val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

      def toMeans(offset: Int): IndexedSeq[Float] = {
        val rectangle = new Rectangle(offset, 0, 1, entireVolume.xSize)
        val di = dicomImage.getSubimage(rectangle)

        if (sliceIndex == 242) { // TODO rm
          val j = di.rowSums.map(s => toHu(s / 1))
          Trace.trace("unfiltered HU: " + j.map(v => Util.fmtDbl(v)).mkString("   "))
        }

        val j = di.rowSums
        val maxJ = j.max
        val meanList = di.rowSums.map(s => toHu(s / 1)).map(m => if (m < minHu) 0.toFloat else m)
        meanList.toIndexedSeq
      }

      val vertBandList = (0 until entireVolume.zSize by 1).map(toMeans)
      val hasCube = vertBandList.map(band => bandContainsCube(band, cubeSize_pix.getX)).flatten

      if (true && (sliceIndex == 242)) { // TODO rm
        println("containsCubeVert sliceIndex: " + sliceIndex + "    vertBandList.size: " + vertBandList.size)
        vertBandList.map(b => {
          if (b.max > 0) {
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getX))
            println(b.mkString("  "))
            println("containsCubeHorz ---------------------------------------------------------------------------------------------------------------------- " + sliceIndex + "    bandContainsCube: " + bandContainsCube(b, cubeSize_pix.getX))
          }
        })
      }

      val ok = hasCube.size >= oneThirdCubeZ_pix
      //logger.info("containsCubeVert sliceIndex " + sliceIndex.formatted("%3d") + " : " + ok)
      if (ok) hasCube.headOption
      else None
    }

    /**
     * Require the image to contain bands oriented in both the vertical and horizontal
     * directions that contain pairs of rising and falling edges separated by the size
     * of the cube to consider the slice to contain the cube.
     *
     * If found, return the center coordinates.
     */
    def containsCube(sliceIndex: Int): Option[Point2d] = {
      if (sliceIndex == 155) // TODO rmf
        Trace.trace("hey")
      val h = containsCubeHorz(sliceIndex)
      val v = containsCubeVert(sliceIndex)
      Trace.trace("sliceIndex: " + sliceIndex + "    h: " + h + "    v: " + v)

      if (false) { // TODO put back in
        containsCubeHorz(sliceIndex) match {
          case Some(h) => {
            containsCubeVert(sliceIndex) match {
              case Some(v) => {
                Trace.trace("Found center: " + sliceIndex + " : " + h + ", " + v)
                Some(new Point2d(h, v))
              }
              case _ => None
            }
          }
          case _ => None
        }
      }
      if (h.isDefined && v.isDefined) Some(new Point2d(h.get, v.get)) else None
    }

    if (true) {
      Trace.trace
      val start = System.currentTimeMillis
      val all = (0 until entireVolume.ySize).map(sliceIndex => (sliceIndex, containsCube(sliceIndex)))
      Trace.trace("All:\n" + all.mkString("\n"))
      val elapsed = System.currentTimeMillis - start
      Trace.trace("elapsed ms: " + elapsed)
      Trace.trace
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
        val next = sliceIndex + oneThirdCubeY_pix
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
      val partialCube = cubeSize_pix.getY / 5
      val centerList = (vertCenterSliceIndex - partialCube until vertCenterSliceIndex + partialCube).map(sliceIndex => (sliceIndex, containsCube(sliceIndex)))
      Trace.trace("centerList:\n" + centerList.mkString("\n"))

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
        Some(top + (cubeSize_pix.getY / 2))
      }
      case _ => None
    }

    centerSliceIndex match {
      case Some(sliceIndex) => checkMultiplePoints(sliceIndex)
      case _ => None
    }

    centerSliceIndex
  }

  /**
   * Find the approximate center of the cube to within a few voxels.  To do this, each
   * horizontal slice is examined to determine if it contains approximately the number of non-zero
   * voxels that the cube (with known size) should contain.
   *
   * The algorithm checks by starting at the top of the image, examining each horizontally sliced
   * plane.  As an extra check, the cube is considered found when the first slice is found AND the
   * slice 1/4 the way down the cube also has the right number of non-zero voxels.
   *
   * After finding the top of the cube, the vertical center is found by offsetting by the number of
   * voxels.  The horizontal center is found by taking that horizontal slice's center of mass.
   *
   * @param entireVolume: Entire scanned volume.
   *
   * @voxSize_mm Size of a single voxel in mm.
   *
   * @return If found, the position in voxels.  If not found, return None.
   */
  private def getCoarseVox(entireVolume: DicomVolume, voxSize_mm: Point3d, toHu: Float => Float): Option[Point3i] = {

    val cubeHeight_vox = (Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY).round.toInt

    /** Keep a cache of previously calculated results for efficiency. */
    val sliceCache = scala.collection.mutable.Map[Int, IndexedSeq[IndexedSeq[Float]]]()

    /** Get the voxels in the plane parallel to the table at the given slice index. */
    def horizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
      sliceCache.get(sliceIndex) match {
        case Some(slice) => slice
        case _ => {
          val s = (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))
          sliceCache.put(sliceIndex, s)
          s
        }
      }
    }

    /** drop this percentage of the largest valued voxels to get rid of outliers. */

    /** Number of voxels expected to compose a cross section of cube. */
    val expectedVoxelCount: Int = ((Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX) * (Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)).round.toInt

    def getCoarseVerticalCenter_voxOld(entireVolume: DicomVolume, voxSize_mm: Point3d): Option[Int] = {

      val cubeHalf = cubeHeight_vox / 2

      /** Keep a cache of previously calculated results for efficiency. */
      val percentOfCubeVoxelsCache = scala.collection.mutable.Map[Int, Double]()

      def getRelaventVoxels(sliceIndex: Int): IndexedSeq[Float] = {
        val pctToDrop = 1.0
        val dropCount = (expectedVoxelCount * (pctToDrop / 100.0)).round.toInt
        val voxelList = horizontalSlice(sliceIndex).flatten.sorted.dropRight(dropCount)
        voxelList
      }

      def percentOfCubeVoxels(sliceIndex: Int): Double = {
        if (percentOfCubeVoxelsCache.contains(sliceIndex)) {
          percentOfCubeVoxelsCache(sliceIndex)
        } else {
          val voxelList = getRelaventVoxels(sliceIndex)
          val mid = (voxelList.max - voxelList.min) / 2
          val found = voxelList.filter(v => v > mid).size
          val percent = (found * 100.0) / expectedVoxelCount

          percentOfCubeVoxelsCache.put(sliceIndex, percent)

          percent
        }
      }

      def opposingEdgesValid(sliceIndex: Int): Boolean = {
        /**
         * Portion of cube to use when taking a plateau.  Dictates how long the
         *  compared profile segments should be. If this is too large, then a cube
         *  very close to the edge of the volume in X or Z will not be detected.
         */
        val cubeFraction = 0.2

        val dicomImage = new DicomImage(horizontalSlice(sliceIndex))

        val cubeSizeImageX_pix = Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ // size of cube in image X direction in pixels
        val cubeSizeImageY_pix = Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX // size of cube in image Y direction in pixels

        val plateauSizeImageX_pix = d2i(cubeSizeImageX_pix * cubeFraction) // size of plateau in image X direction in pixels
        val plateauSizeImageY_pix = d2i(cubeSizeImageY_pix * cubeFraction) // size of plateau in image Y direction in pixels

        val rowSum = dicomImage.rowSums
        val colSum = dicomImage.columnSums

        def edgeSize(sumList: Seq[Float], index: Int, plateauSize_pix: Int): Float = {
          val left = sumList.take(index).takeRight(plateauSize_pix)
          val right = sumList.drop(index).take(plateauSize_pix)
          right.sum - left.sum
        }

        val xSize_mmAll = {
          val xRange = (plateauSizeImageX_pix until dicomImage.width - plateauSizeImageX_pix).toSeq
          val xEdgeSeq = xRange.map(imgX => (imgX, edgeSize(colSum, imgX, plateauSizeImageX_pix)))
          val risingX = xEdgeSeq.maxBy(sv => sv._2)._1
          val fallingX = xEdgeSeq.minBy(sv => sv._2)._1

          val size = (fallingX - risingX) * voxSize_mm.getZ // size of cube in mm in image X direction
          (risingX, fallingX, size)
        }
        val xSize_mm = xSize_mmAll._3

        val ySize_mmAll = {
          val yRange = (plateauSizeImageY_pix until dicomImage.height - plateauSizeImageY_pix).toSeq
          val yEdgeSeq = yRange.map(imgY => (imgY, edgeSize(rowSum, imgY, plateauSizeImageY_pix)))
          val risingY = yEdgeSeq.maxBy(sv => sv._2)._1
          val fallingY = yEdgeSeq.minBy(sv => sv._2)._1
          val size = (fallingY - risingY) * voxSize_mm.getX // size of cube in mm in image Y direction
          (risingY, fallingY, size)
        }
        val ySize_mm = ySize_mmAll._3

        val cubeDiff = Config.DailyQAPhantomCubeSize_mm * (Config.DailyQACBCTCubeSizePercentTolerance / 100.0)
        val minCube_mm = Config.DailyQAPhantomCubeSize_mm - cubeDiff
        val maxCube_mm = Config.DailyQAPhantomCubeSize_mm + cubeDiff
        val ok =
          (xSize_mm > minCube_mm) && (xSize_mm < maxCube_mm) &&
            (ySize_mm > minCube_mm) && (ySize_mm < maxCube_mm)

        def fmt(d: Double) = d.formatted("%7.2f")
        logger.info("BBbyCBCTAnalysis.opposingEdgesValid: " + ok.toString.formatted("%5s") +
          "    Expected cube size mm: " + Config.DailyQAPhantomCubeSize_mm +
          "    slice: " + sliceIndex.formatted("%3d") +
          "   pct error allowed: " + Config.DailyQACBCTCubeSizePercentTolerance +
          "   X rise,fall,size: " + fmt(xSize_mmAll._1) + " : " + fmt(xSize_mmAll._2) + " : " + fmt(xSize_mm) +
          "   Y rise,fall,size: " + fmt(ySize_mmAll._1) + " : " + fmt(ySize_mmAll._2) + " : " + fmt(ySize_mm))

        ok
      }

      def hasCubeVoxels(sliceIndex: Int): Boolean = {
        val pct = percentOfCubeVoxels(sliceIndex)
        val diff = (100.0 - pct).abs
        val ok = diff <= Config.DailyQACBCTVoxPercentTolerance
        if (ok) logger.info("Cube cross section found at horizontal voxel slice " + sliceIndex + ".  Percent of voxels found: " + pct)
        ok
      }

      // Wrap CPU intensive function with caching to eliminate doing the same calculations multiple times.
      val cacheHasCubeVoxels = scala.collection.mutable.Map[Int, Boolean]()
      def hasCubeVoxelsCached(sliceIndex: Int): Boolean = {
        if (cacheHasCubeVoxels.contains(sliceIndex)) cacheHasCubeVoxels(sliceIndex)
        else {
          cacheHasCubeVoxels.put(sliceIndex, hasCubeVoxels(sliceIndex))
          cacheHasCubeVoxels(sliceIndex)
        }
      }

      // Wrap CPU intensive function with caching to eliminate doing the same calculations multiple times.
      val cacheOpposingEdgesValid = scala.collection.mutable.Map[Int, Boolean]()
      def opposingEdgesValidCached(sliceIndex: Int): Boolean = {
        if (cacheOpposingEdgesValid.contains(sliceIndex)) cacheOpposingEdgesValid(sliceIndex)
        else {
          cacheOpposingEdgesValid.put(sliceIndex, opposingEdgesValid(sliceIndex))
          cacheOpposingEdgesValid(sliceIndex)
        }
      }

      val top = {
        val start = System.currentTimeMillis
        val count = Math.max(5, cubeHeight_vox / 10) // get the number of slices to check.  Make sure there are at least a few.
        val minRequired = (count * 0.90).round.toInt // require that 90 percent of them have the right count

        def allHave(i: Int) = {
          val list = (i until (i + count)).map(sliceIndex => hasCubeVoxelsCached(sliceIndex) && opposingEdgesValidCached(sliceIndex))
          val all = list.filter(a => a).size >= minRequired
          all
        }

        val sliceIndex = (0 until entireVolume.ySize - cubeHalf).toSeq.find(i => allHave(i))
        logger.info("Elapsed time to find top of CBCT cube ms: " + (System.currentTimeMillis - start))
        sliceIndex
      }

      val center =
        if (top.isDefined) {
          val t = top.get
          val center = t + cubeHalf
          Some(center)
        } else {
          logger.warn("Could not find vertical center of cube")
          None
        }

      center
    }

    // Given the vertical center of the cube, find the center.
    val coarseVertOpt = BBbyCBCTCoarseCenter.getCoarseVerticalCenter_vox(entireVolume, voxSize_mm, toHu)

    if (false) { // TODO
      val coarseVertOptOld = getCoarseVerticalCenter_voxOld(entireVolume, voxSize_mm)
      Trace.trace("new alg: " + coarseVertOpt + " old alg: " + coarseVertOptOld)
    }

    coarseVertOpt match {
      case Some(bbY) => {
        val horzImage = new DicomImage(horizontalSlice(bbY))

        // for the tallness of the bb in the Y dimension, add the slices above and below this slice togther with this slice
        val sumDicomImage = {
          val bbHalfTallness = (Config.CBCTBBPenumbra_mm / voxSize_mm.getY).round.toInt
          val sliceList = (bbY - bbHalfTallness until bbY + bbHalfTallness).map(s => new DicomImage(horizontalSlice(s)))

          val sum = {
            for (y <- 0 until horzImage.height) yield {
              for (x <- 0 until horzImage.width) yield { sliceList.map(s => s.get(x, y)).sum }
            }
          }
          new DicomImage(sum)
        }
        val bbWidth_pix = ((Config.CBCTBBPenumbra_mm * 2) / voxSize_mm.getZ).round.toInt
        val bbHeight_pix = ((Config.CBCTBBPenumbra_mm * 2) / voxSize_mm.getX).round.toInt
        val coords = for (x <- 0 until (horzImage.width - bbWidth_pix); y <- 0 until (horzImage.height - bbHeight_pix)) yield (x, y)
        val best = coords.maxBy(xy => sumDicomImage.getSubimage(new Rectangle(xy._1, xy._2, bbWidth_pix, bbHeight_pix)).sum)
        val center1 = new Point3i(best._1 + (bbWidth_pix / 2), bbY, best._2 + (bbHeight_pix / 2))
        val center = new Point3i(center1.getZ, center1.getY, center1.getX)
        logger.info("coarse measurement of center: " + center)
        Some(center)
      }
      case _ => {
        logger.warn("could not find BB")
        None
      }
    }
  }

  /**
   * Make images that show the BB from the X, Y and Z axis by taking a slice a 4 times thicker than
   * the BB in the given direction that encompass the BB.  The point is to be able to generate
   * images that show the BB but minimizing noise by eliminating the volume in front of and behind
   * the BB (from the 3 orthogonal axis).   The voxels are averaged in along the relevant axis to
   * produce a 2-D image.
   */
  private def makeImagesXYZ(entireVolume: DicomVolume, fineLocation_vox: Point3d, cbctForLocation_mm: Point3d, voxSize_mm: Point3d): Seq[BufferedImage] = {
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

  /**
   * Container for intermediary results.
   *
   * @param coarseLoction_vox Initial measurement/location of bb in CBCT in voxel coordinate.
   *
   * @param fineLocation_vox Final measurement of location of bb in CBCT in voxel coordinates.
   *
   * @param volumeTranslator CBCT volume translator.  Translates between pixels and mm in CBCT frame of reference.  Provided as a convenience and because creating it consumes resources.
   *
   * @param cbctFrameOfRefLocation_mm Location of BB in CBCT frame of reference in mm.
   *
   * @param imageXYZ List of images from perspective of X, Y, and Z axis.
   */
  case class CBCTAnalysisResult(coarseLoction_vox: Point3i, fineLocation_vox: Point3d, volumeTranslator: VolumeTranslator, cbctFrameOfRefLocation_mm: Point3d, imageXYZ: Seq[BufferedImage])

  /**
   * Get the maximum (sub-voxel) point in the given volume.  It must
   * deviate sufficiently from the volume's mean to be considered a
   * valid maximum.
   */
  private def getMaxPoint(volume: DicomVolume): Option[Point3d] = {

    val profileList = Seq(volume.xPlaneProfile, volume.yPlaneProfile, volume.zPlaneProfile)

    /** Return the number of standard deviations that the maximum is. */
    def maxStdDev(profile: Profile) = (profile.cubicSpline.evaluate(profile.max) - profile.mean).abs / profile.standardDeviation

    def check(profile: Profile): Option[Double] = {
      if (maxStdDev(profile) >= Config.CBCTBBMinimumStandardDeviation)
        Some(profile.max)
      else
        None
    }

    val result = profileList.map(p => check(p))

    logger.info("CBCT standard dev required: " + Config.CBCTBBMinimumStandardDeviation + "    actual XYZ: " + profileList.map(maxStdDev).mkString("  "))

    if (result.flatten.size == 3)
      Some(new Point3d(result(0).get, result(1).get, result(2).get))
    else
      None
  }

  /**
   * Look in a center cubic volume of the CBCT set for the BB.
   *
   * The BB must be within a certain distance or the test fails.  Taking advantage of this
   * requirement greatly speeds the algorithm because it has fewer voxels to search.
   */
  def volumeAnalysis(cbctSeries: Seq[AttributeList], outputDir: File): Either[String, CBCTAnalysisResult] = {
    val sorted = Util.sortByZ(cbctSeries)
    outputDir.mkdirs

    val voxSize_mm = Util.getVoxSize_mm(sorted) // the size of a voxel in mm

    val entireVolume = DicomVolume.constructDicomVolume(sorted) // all CBCT voxels as a volume

    def getBbVolumeStart(offset_vox: Point3i, coarse_vox: Point3i): Point3i = {
      val ov = offset_vox
      ov.scale(-1)
      ov.add(coarse_vox)
      ov
    }

    /**
     * Get sub-volume of the entire volume that contains just the
     *  BB and some of surrounding cube, according to the coarse
     *  location.  Getting a larger volume ensures that the BB's
     *  location is actually in the volume, and also provides a
     *  larger sample of voxels inside the cube to differentiate
     *  from the brightness of the BB.
     */
    def getBbVolume(offset_vox: Point3i, bbVolumeStart: Point3i): DicomVolume = {
      val size_vox = offset_vox
      size_vox.scale(2)
      entireVolume.getSubVolume(bbVolumeStart, size_vox)
    }

    /**
     * Given the fine (precise) location, build results.
     */
    def processFineLocation(relOpt: Point3d, bbVolumeStart: Point3i, coarse_vox: Point3i): CBCTAnalysisResult = {
      val fineLocation_vox = relOpt
      fineLocation_vox.add(i2d(bbVolumeStart))
      val volumeTranslator = new VolumeTranslator(sorted)
      val cbctForLocation_mm = volumeTranslator.vox2mm(fineLocation_vox)
      val imageXYZ = makeImagesXYZ(entireVolume, fineLocation_vox, cbctForLocation_mm, voxSize_mm)
      val result = new CBCTAnalysisResult(coarse_vox, fineLocation_vox, volumeTranslator,
        cbctForLocation_mm: Point3d, imageXYZ)

      def fmt(d: Double) = d.formatted("%12.7f")
      def fmtPoint(point: Point3d): String = fmt(point.getX) + ",  " + fmt(point.getY) + ",  " + fmt(point.getZ)
      val contentDateTime = cbctSeries.map(al => DicomUtil.getTimeAndDate(al, TagFromName.ContentDate, TagFromName.ContentTime)).flatten.minBy(_.getTime)
      logger.info("BB found in CBCT" +
        "\n    ImagePositionPatient first slice: " + volumeTranslator.ImagePositionPatient +
        "\n    Content date and time: " + (new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss")).format(contentDateTime) +
        "    coordinates in voxels: " + fmtPoint(fineLocation_vox) +
        "\n    frame of ref coordinates in mm: " + fmtPoint(cbctForLocation_mm))

      result
    }

    val RescaleSlope = cbctSeries.head.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = cbctSeries.head.get(TagFromName.RescaleIntercept).getDoubleValues.head

    def toHu(pixValue: Float) = ((pixValue * RescaleSlope) + RescaleIntercept).toFloat
    val coarseOpt_vox = getCoarseVox(entireVolume, voxSize_mm, toHu)

    coarseOpt_vox match {
      case Some(coarse_vox) => {
        if (true) { // TODO rm
          val al = cbctSeries(coarse_vox.getZ)
          val bbSlice = new DicomImage(al)
          val sub = bbSlice.getSubimage(new Rectangle(coarse_vox.getX + 10, coarse_vox.getY + 10, 8, 8))
          val mean = sub.sum / (sub.width * sub.height)
          val RescaleSlope = al.get(TagFromName.RescaleSlope).getDoubleValues.head
          val RescaleIntercept = al.get(TagFromName.RescaleIntercept).getDoubleValues.head
          val StationName = al.get(TagFromName.StationName).getSingleStringValueOrEmptyString
          val meanHu = (mean * RescaleSlope) + RescaleIntercept
          Trace.trace(
            "    coarse_vox: " + coarse_vox +
              "    StationName: " + StationName +
              "    RescaleSlope: " + Util.fmtDbl(RescaleSlope) +
              "    RescaleIntercept: " + Util.fmtDbl(RescaleIntercept) +
              "    mean: " + Util.fmtDbl(mean) +
              "    meanHu: " + Util.fmtDbl(meanHu))
        }

        // found the coarse location of the BB.  Now try to find the fine (exact) location.
        val searchExtensionFactor = 4.0
        val offset_mm = Config.CBCTBBPenumbra_mm * searchExtensionFactor
        def offset_vox = d2i(new Point3d(offset_mm / voxSize_mm.getX, offset_mm / voxSize_mm.getY, offset_mm / voxSize_mm.getZ))

        val bbVolumeStart = getBbVolumeStart(offset_vox, coarse_vox)

        val bbVolume = getBbVolume(offset_vox, bbVolumeStart)

        getMaxPoint(bbVolume) match {
          case Some(maxPoint) => {
            // found the exact location
            val resultX = processFineLocation(maxPoint, bbVolumeStart, coarse_vox)
            Right(resultX)
          }
          case _ => Left("Could not find BB, possibly due to insufficient signal to noise ratio.")
        }
      }
      case _ => Left("No BB found.  Could not find cube containing BB.")
    }

    //    val volumeTranslator = new VolumeTranslator(sorted)
    //    // fine location in mm coordinates
    //    if (fineLocation_vox.isDefined) {
    //      val cbctForLocation_mm = volumeTranslator.vox2mm(fineLocation_vox.get)
    //      def fmt(d: Double) = d.formatted("%12.7f")
    //      def fmtPoint(point: Point3d): String = fmt(point.getX) + ",  " + fmt(point.getY) + ",  " + fmt(point.getZ)
    //      logger.info("BB found in CBCT" +
    //        "\n    ImagePositionPatient first slice: " + volumeTranslator.ImagePositionPatient +
    //        "\n    coordinates in voxels: " + fmtPoint(fineLocation_vox.get) +
    //        "\n    frame of ref coordinates in mm: " + fmtPoint(cbctForLocation_mm))
    //      val imageXYZ = makeImagesXYZ(entireVolume, fineLocation_vox.get, cbctForLocation_mm, voxSize_mm)
    //      //val imageXYZ = makeImagesXYZ(addVolumeMarker(entireVolume, fineLocation_vox.get), fineLocation_vox.get, cbctForLocation_mm, voxSize_mm)
    //      val result = new CBCTAnalysisResult(coarse_vox, fineLocation_vox.get, volumeTranslator, cbctForLocation_mm: Point3d, imageXYZ)
    //      //Right(cbctForLocation_mm, imageXYZ)
    //      Right(result)
    //    } else {
    //      Left("No BB found")
    //    }
  }
}
