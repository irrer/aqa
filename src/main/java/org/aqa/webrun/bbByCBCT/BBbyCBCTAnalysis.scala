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

  //  /**
  //   * Get the size of the search volume in voxels.
  //   */
  //  private def sizeOfSearch(voxSize_mm: Seq[Double]): Point3i = {
  //    val seq = voxSize_mm.map(size_mm => (Config.DailyPhantomSearchDistance_mm / size_mm).ceil.toInt)
  //    new Point3i(seq.toArray)
  //  }

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
    val bufImg = if (Config.CBCTImageColor.getRGB == 0)
      aspectCorrected.toDeepColorBufferedImage(0.0)
    else
      aspectCorrected.toBufferedImage(Config.CBCTImageColor)
    bufImg
  }

  def getCoarseVox(entireVolume: DicomVolume, voxSize_mm: Point3d): Option[Point3i] = {

    def horizontalSlice(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
      (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))
    }

    def horizontalSliceToSum(sliceIndex: Int): Float = horizontalSlice(sliceIndex).flatten.sum

    val profile = (0 until entireVolume.ySize).map(sliceIndex => horizontalSliceToSum(sliceIndex))

    val cubeLen_vox = Config.DailyQAPhantomCubeSize_mm / voxSize_mm.getY // height (along Y axis) of cube in voxels
    val cubeSegment = (cubeLen_vox / 2).round.toInt

    /**
     * Determine if the profile at the given index defines the approximate top of the cube.
     */
    def isStartOfCube(index: Int): Boolean = {
      val list = profile.drop(index).take(cubeSegment)
      def flatness = {
        val stdDev = ImageUtil.stdDev(list)
        val avg = list.sum / list.size
        stdDev / avg
      }
      val isStart = ((list.min >= Config.DailyQACBCTPhantomMinHorizontalPlaneSum_cu) && (flatness < Config.DailyQACBCTFlatnessMinimum))
      isStart
    }

    //    val minSum = cubeSegment * Config.DailyQACBCTPhantomMinHorizontalPlaneSum_cu
    //
    //    val sumList = (0 until (profile.size - cubeSegment - 1)).map(i => profile.drop(i).take(cubeSegment).sum)
    //    val top = sumList.zipWithIndex.find(si => si._1 > minSum)

    val top = (0 until profile.size - cubeSegment).toSeq.find(index => isStartOfCube(index))
    if (top.isDefined) {
      // DICOM image that has just the cross section of the cube (no table, might contain BB).
      val midCubeIndex = top.get + cubeSegment
      val profileToMidCube = profile.take(midCubeIndex)
      val threshold = (profileToMidCube.head + profileToMidCube.last) / 2
      Trace.trace("profileToMidCube  min: " + profileToMidCube.min + "    max: " + profileToMidCube.max + "    head: " + profileToMidCube.head + "    last: " + profileToMidCube.last + "    threshold: " + threshold)
      val topPosition = LocateEdge.locateEdge(profileToMidCube, threshold)
      // the coarse Y position of the BB
      val bbY = (topPosition + (cubeLen_vox / 2)).round.toInt

      // find coarse X and Z
      val cubeSlice = new DicomImage(horizontalSlice(midCubeIndex))
      val bbX = ImageUtil.centerOfMass(cubeSlice.rowSums).round.toInt
      val bbZ = ImageUtil.centerOfMass(cubeSlice.columnSums).round.toInt

      val center = new Point3i(bbX, bbY, bbZ)
      def i2d(i: Point3i) = new Point3d(i.getX, i.getY, i.getZ)
      logger.info("coarse measurement of center: " + center)
      Some(center)
    } else {
      logger.warn("could not find BB")
      None
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

    val mm = Config.CBCTBBPenumbra_mm * 4

    // point closest to origin for each sub-volume
    val start = new Point3i(
      (fineLocation_vox.getX - (mm / voxSize_mm.getX)).ceil.toInt,
      (fineLocation_vox.getY - (mm / voxSize_mm.getY)).ceil.toInt,
      (fineLocation_vox.getZ - (mm / voxSize_mm.getZ)).ceil.toInt)

    // number of planes in each sub-volume
    val size = new Point3i(
      ((mm * 2) / voxSize_mm.getX).ceil.toInt,
      ((mm * 2) / voxSize_mm.getY).ceil.toInt,
      ((mm * 2) / voxSize_mm.getZ).ceil.toInt)

    val diX = xImage(entireVolume, start, size)
    val diY = yImage(entireVolume, start, size)
    val diZ = zImage(entireVolume, start, size)

    val minMaxPixels = getMinMax(Seq(diX, diY, diZ))

    val bufImgList = Seq(
      makeImage(diX, new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getY), new Point2D.Double(voxSize_mm.getZ, voxSize_mm.getY), minMaxPixels),
      makeImage(diY, new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getX), new Point2D.Double(voxSize_mm.getZ, voxSize_mm.getX), minMaxPixels),
      makeImage(diZ, new Point2D.Double(fineLocation_vox.getX, fineLocation_vox.getY), new Point2D.Double(voxSize_mm.getX, voxSize_mm.getY), minMaxPixels))

    bufImgList
  }

  //  private def addVolumeMarker(entireVolume: DicomVolume, fineLocation_vox: Point3d): DicomVolume = {
  //
  //    val min = 4
  //    val max = 20
  //
  //    def fineInt = new Point3i(fineLocation_vox.getX.round.toInt, fineLocation_vox.getY.round.toInt, fineLocation_vox.getZ.round.toInt)
  //
  //    def inX(x: Int) = (x >= min + fineInt.getX) && (x < max + fineInt.getX)
  //    def inY(x: Int) = (x >= min + fineInt.getY) && (x < max + fineInt.getY)
  //    def inZ(x: Int) = (x >= min + fineInt.getZ) && (x < max + fineInt.getZ)
  //
  //    val increase = (2.5).toFloat
  //    def mark(img: DicomImage, index: Int): DicomImage = {
  //      if (inZ(index)) {
  //        val pd = for (y <- 0 until img.height) yield {
  //          val row = for (x <- 0 until img.width) yield {
  //            val p = img.get(x, y)
  //            if (inX(x) && inY(y)) { p * increase } else { p }
  //          }
  //          row
  //        }
  //        new DicomImage(pd)
  //      } else
  //        img
  //    }
  //
  //    val imgList = entireVolume.volume.zipWithIndex.map(di => mark(di._1, di._2))
  //    new DicomVolume(imgList)
  //  }

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

    if (false) { // TODO rm
      val size = new Point3i(entireVolume.xSize, 1, entireVolume.zSize)
      def sliceTo2d(sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
        (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))
      }
      val imgList = (0 until entireVolume.ySize).map(sliceIndex => sliceTo2d(sliceIndex)).map(slice => (new DicomImage(slice)).toDeepColorBufferedImage(0.0))
      imgList.zipWithIndex.map(ii => Util.writePng(ii._1, new File(outputDir, ii._2.formatted("Slice%03d.png"))))

      println("sum list begin")
      (0 until entireVolume.ySize).map(sliceIndex => sliceTo2d(sliceIndex)).map(slice => println((new DicomImage(slice)).sum))
      println("sum list end")
    }

    val searchStart = startOfSearch(entireVolume, voxSize_mm) // point of search volume closest to origin
    // val searchSize = sizeOfSearch(voxSize_mm) // size of search volume

    /*
    val searchVolume = entireVolume.getSubVolume(searchStart, searchSize) // sub-volume of the CBCT volume to be searched for the BB.

    def coarse_vox = {
      val start = System.currentTimeMillis // TODO rm
      val size = new Point3i(4, 4, 2)
      val high = searchVolume.getHighest(size)
      high.add(searchStart)
      //high.add(new Point3i(-2, -2, -1))
      high.add(new Point3i(size.getX / 2, size.getY / 2, size.getZ / 2))
      high
    }

    logger.info("coarseLocation       in pixels: " + coarse_vox)
    val cvc = coarseVoxCube
    logger.info("%%%% coarseVoxCube in pixels: " + cvc)
    */

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

        // fine location in voxel coordinates

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
          logger.info("BB found in CBCT" +
            "\n    ImagePositionPatient first slice: " + volumeTranslator.ImagePositionPatient +
            "\n    coordinates in voxels: " + fmtPoint(fineLocation_vox) +
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
