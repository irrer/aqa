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
import scala.collection.mutable.ArrayBuffer
import edu.umro.ScalaUtil.DicomUtil
import java.text.SimpleDateFormat

/**
 * Find the BB in the CBCT volume.
 */

object BBbyCBCTAnalysis extends Logging {

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
  private def getCoarseVox(entireVolume: DicomVolume, voxSize_mm: Point3d): Option[Point3i] = {

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
    val pctToDrop = 1.0

    /** Number of voxels expected to compose a cross section of cube. */
    val expectedVoxelCount: Int = ((Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getX) * (Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)).round.toInt

    def getCoarseVerticalCenter_vox(entireVolume: DicomVolume, voxSize_mm: Point3d): Option[Int] = {

      val cubeHalf = cubeHeight_vox / 2

      /** Keep a cache of previously calculated results for efficiency. */
      val percentOfCubeVoxelsCache = scala.collection.mutable.Map[Int, Double]()

      def getRelaventVoxels(sliceIndex: Int): IndexedSeq[Float] = {
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

      def hasCubeVoxels(sliceIndex: Int): Boolean = {
        val pct = percentOfCubeVoxels(sliceIndex)
        val diff = (100.0 - pct).abs
        val ok = diff <= Config.DailyQACBCTVoxPercentTolerance
        if (ok) logger.info("Top of cube found at horizontal voxel slice " + sliceIndex + ".  Percent of voxels found: " + pct)
        ok
      }

      val top = {
        val count = Math.max(5, cubeHeight_vox / 10) // get the number of slices to check.  Make sure there are at least a few.
        val minRequired = (count * 0.90).round.toInt // require that 90 percent of them have the right count

        def allHave(i: Int) = {
          val list = (i until (i + count)).map(hasCubeVoxels)
          val all = list.filter(a => a).size >= minRequired
          all
        }

        (0 until entireVolume.ySize - cubeHalf).toSeq.find(i => allHave(i))
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

    //    /**
    //     * Set pixels to 0 if they are in range of a dark pixel.
    //     */
    //    def erode(orig: DicomImage, xSize: Double, ySize: Double): DicomImage = {
    //      val listOfPointCoordinatesWithinRadius = {
    //        val radius_mm = Config.DailyQAPhantomCubeSize_mm / 10
    //        val xMax = (radius_mm / xSize).round.toInt
    //        val yMax = (radius_mm / ySize).round.toInt
    //
    //        def withinRadius(x: Int, y: Int): Boolean = {
    //          val xx = x * xSize
    //          val yy = y * ySize
    //          val distance = Math.sqrt((xx * xx) + (yy * yy))
    //          val within = distance < radius_mm
    //          within
    //        }
    //
    //        val list = for (x <- (-xMax to xMax); y <- (-yMax to yMax); if (withinRadius(x, y))) yield (x, y)
    //        list
    //      }
    //
    //      val minPixelValue: Float = {
    //        val dropCount = (expectedVoxelCount * (pctToDrop / 100.0)).round.toInt
    //        val pixList = orig.pixelData.flatten.sorted.dropRight(dropCount).takeRight(expectedVoxelCount)
    //        val avg = pixList.sum / pixList.size
    //        val minPix = avg / 2
    //        minPix
    //      }
    //
    //      /**
    //       * Given a pixel in the original image, set it to dark if there is a dark pixel within the radius_mm.
    //       */
    //      def eval(x: Int, y: Int): Float = {
    //
    //        val pixValue = orig.get(x, y)
    //        if (pixValue < minPixelValue)
    //          pixValue // this pixel is dark, so leave the way it is
    //        else {
    //          def pixelIsDark(xx: Int, yy: Int): Boolean = {
    //            val j = (xx >= 0) && (yy >= 0) && (xx < orig.width) && (yy < orig.height) && (orig.get(xx, yy) < minPixelValue)
    //            j
    //          }
    //
    //          // look through the list of near pixels for a dark one.
    //          if (listOfPointCoordinatesWithinRadius.find(p => pixelIsDark(p._1 + x, p._2 + y)).isDefined) {
    //            0.toFloat // found a dark pixel nearby.  Set this one to dark.
    //          } else pixValue
    //        }
    //      }
    //
    //      val pixArray = {
    //        for (y <- 0 until orig.height) yield {
    //          for (x <- 0 until orig.width) yield { eval(x, y) }
    //        }
    //      }
    //
    //      val dicomImageEroded = orig //  new DicomImage(pixArray)
    //
    //      dicomImageEroded
    //    }

    // Given the vertical center of the cube, find the center.
    getCoarseVerticalCenter_vox(entireVolume, voxSize_mm) match {
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

    getCoarseVox(entireVolume, voxSize_mm) match {
      case Some(coarse_vox) => {
        val searchExtensionFactor = 4.0
        val offset_mm = Config.CBCTBBPenumbra_mm * searchExtensionFactor
        def offset_vox = d2i(new Point3d(offset_mm / voxSize_mm.getX, offset_mm / voxSize_mm.getY, offset_mm / voxSize_mm.getZ))

        val bbVolumeStart = {
          val ov = offset_vox
          ov.scale(-1)
          ov.add(coarse_vox)
          ov
        }

        // sub-volume of the entire volume that contains just the BB and a little bit of surrounding cube, according to the coarse location.
        val bbVolume = {
          val size_vox = offset_vox
          size_vox.scale(2)
          entireVolume.getSubVolume(bbVolumeStart, size_vox)
        }

        val relOpt = bbVolume.getMaxPoint(Config.CBCTBBMinimumStandardDeviation)
        if (relOpt.isDefined) {
          val fineLocation_vox = relOpt.get
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

          Right(result)
        } else
          Left("Could not find BB, possibly due to insufficient signal to noise ratio.")

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
