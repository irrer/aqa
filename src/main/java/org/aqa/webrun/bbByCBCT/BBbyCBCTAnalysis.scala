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

/**
 * Find the BB in the CBCT volume.
 */

object BBbyCBCTAnalysis extends Logging {

  /**
   * Get the volume that needs to be searched for the BB.
   */
  //  private def getVolumeOfInterest(sorted: Seq[AttributeList], voxSize_mm: Point3d): DicomVolume = {
  //    val rows = sorted.head.get(TagFromName.Rows).getIntegerValues().head
  //    val columns = sorted.head.get(TagFromName.Columns).getIntegerValues().head
  //    // centers of volume in mm
  //    val xCenter_mm = ((rows - 1) * voxSize_mm.getX) / 2
  //    val yCenter_mm = ((columns - 1) * voxSize_mm.getY) / 2
  //    val zCenter_mm = (Util.slicePosition(sorted.last) + Util.slicePosition(sorted.head)) / 2.0
  //
  //    // consider only slices that are sufficiently close to the center of the volume.
  //    val zSlices = sorted.filter(al => (Util.slicePosition(al) - zCenter_mm).abs <= Config.DailyPhantomSearchDistance_mm)
  //
  //    // rectangle within a slice in voxels
  //    val xyRect = {
  //      val x = (xCenter_mm - Config.DailyPhantomSearchDistance_mm) / voxSize_mm.getX
  //      val y = (yCenter_mm - Config.DailyPhantomSearchDistance_mm) / voxSize_mm.getY
  //      val width = (Config.DailyPhantomSearchDistance_mm * 2) / voxSize_mm.getX
  //      val height = (Config.DailyPhantomSearchDistance_mm * 2) / voxSize_mm.getY
  //      val rect = new Rectangle(x.round.toInt, y.round.toInt, width.round.toInt, height.round.toInt)
  //      rect
  //    }
  //
  //    val imageList = zSlices.map(al => (new DicomImage(al)).getSubimage(xyRect))
  //
  //    def zOffset = {
  //      val firstSop = Util.sopOfAl(zSlices.head)
  //      sorted.map(al => Util.sopOfAl(al)).indexOf(firstSop)
  //    }
  //
  //    val offset = new Point3d(xyRect.getX, xyRect.getY, zOffset)
  //
  //    new DicomVolume(imageList)
  //  }

  /**
   * Get the corner of the search volume closest to the origin in voxels.
   */
  private def startOfSearch(entireVolume: DicomVolume, voxSize_mm: Seq[Double]): Point3i = {
    def toStart(dimension: Int, size_mm: Double): Int = {
      val center = dimension / 2.0
      val offset = (Config.DailyPhantomSearchDistance_mm / size_mm) / 2.0
      (center - offset).round.toInt
    }

    val seq = entireVolume.volSize.zip(voxSize_mm).map(dimScl => toStart(dimScl._1, dimScl._2))

    new Point3i(seq.toArray)
  }

  /**
   * Get the size of the search volume in voxels.
   */
  private def sizeOfSearch(voxSize_mm: Seq[Double]): Point3i = {
    val seq = voxSize_mm.map(size_mm => (Config.DailyPhantomSearchDistance_mm / size_mm).ceil.toInt)
    new Point3i(seq.toArray)
  }

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
        (totalSize * 0.05).round.toInt
    }

    /**
     * Keep removing members until the requisite number has been dropped.
     */
    def trim(total: Int, hist: Seq[DicomImage.HistPoint]): Float = {
      if (total < numDrop)
        trim(total + hist.head.count, hist.tail)
      else hist.head.value
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
    //val bufImg = aspectCorrected.toDeepColorBufferedImage(minMax._1, minMax._2)
    val bufImg = aspectCorrected.toBufferedImage(Color.blue)
    bufImg
  }

  /**
   * Make images that show the BB from the X, Y and Z axis by taking a slice a 4 times thicker than
   * the BB in the given direction that encompass the BB.  The point is to be able to generate
   * images that show the BB but minimizing noise by eliminating the volume in front of and behind
   * the BB (from the 3 orthogonal axis).   The voxels are averaged in along the relevant axis to
   * produce a 2-D image.
   */
  private def makeImagesXYZ(entireVolume: DicomVolume, fineLocation_vox: Point3d, fineLocation_mm: Point3d, voxSize: Seq[Double]): Seq[BufferedImage] = {

    val mm = Config.DailyPhantomBBPenumbra_mm

    // point closest to origin for each sub-volume
    val start = new Point3i(
      (fineLocation_vox.getX - (mm / voxSize(0))).ceil.toInt,
      (fineLocation_vox.getY - (mm / voxSize(1))).ceil.toInt,
      (fineLocation_vox.getZ - (mm / voxSize(2))).ceil.toInt)

    // number of planes in each sub-volume
    val size = new Point3i(
      ((mm * 2) / voxSize(0)).ceil.toInt,
      ((mm * 2) / voxSize(1)).ceil.toInt,
      ((mm * 2) / voxSize(2)).ceil.toInt)

    val diX = xImage(entireVolume, start, size)
    val diY = yImage(entireVolume, start, size)
    val diZ = zImage(entireVolume, start, size)

    val minMaxPixels = getMinMax(Seq(diX, diY, diZ))

    val bufImgList = Seq(
      makeImage(diX, new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getY), new Point2D.Double(voxSize(2), voxSize(1)), minMaxPixels),
      makeImage(diY, new Point2D.Double(fineLocation_vox.getZ, fineLocation_vox.getX), new Point2D.Double(voxSize(2), voxSize(0)), minMaxPixels),
      makeImage(diZ, new Point2D.Double(fineLocation_vox.getX, fineLocation_vox.getY), new Point2D.Double(voxSize(0), voxSize(1)), minMaxPixels))

    bufImgList
  }

  /**
   * Look in a center cubic volume of the CBCT set for the BB.
   *
   * The BB must be within a certain distance or the test fails.  Taking advantage of this
   * requirement greatly speeds the algorithm because it has fewer voxels to search.
   */
  def volumeAnalysis(cbctSeries: Seq[AttributeList]): Either[String, (Point3d, Seq[BufferedImage])] = {
    val sorted = Util.sortByZ(cbctSeries)

    val voxSize_mm = Util.getVoxSize_mm(sorted) // the size of a voxel in mm
    val entireVolume = DicomVolume.constructDicomVolume(sorted) // all CBCT voxels as a volume
    //Trace.trace("voxSize_mm: " + voxSize_mm + "    XYZ: " + entireVolume.xSize + " " + entireVolume.ySize + " " + entireVolume.zSize)

    val searchStart = startOfSearch(entireVolume, voxSize_mm) // point of search volume closest to origin
    val searchSize = sizeOfSearch(voxSize_mm) // size of search volume

    val searchVolume = entireVolume.getSubVolume(searchStart, searchSize) // sub-volume of the CBCT volume to be searched for the BB.

    val coarse2 = {
      val size = new Point3i(4, 4, 2)
      val high = searchVolume.getHighest(size)
      high.add(searchStart)
      high.add(new Point3i(-2, -2, -1))
      high
    }
    val coarseLocation = Seq(coarse2.getX.toDouble, coarse2.getY.toDouble, coarse2.getZ.toDouble)

    val bbVolumeStart = coarseLocation.zip(voxSize_mm).map(cs => (cs._1 - (Config.DailyPhantomBBPenumbra_mm / cs._2)).round.toInt)

    // sub-volume of the entire volume that contains just the BB according to the coarse location.
    val bbVolume = {
      val bbVolumeSize = voxSize_mm.map(s => ((Config.DailyPhantomBBPenumbra_mm * 2) / s).round.toInt)
      entireVolume.getSubVolume(new Point3i(bbVolumeStart.toArray), new Point3i(bbVolumeSize.toArray))
    }

    // fine location in voxel coordinates
    val fineLocation_vox = {
      val relOpt = bbVolume.getMaxPoint(Config.BBMinimumStandardDeviation)
      if (relOpt.isDefined) {
        val rel = relOpt.get
        val finloc = new Point3d(rel.getX + bbVolumeStart(0), rel.getY + bbVolumeStart(1), rel.getZ + bbVolumeStart(2))
        Some(finloc)
      } else None
    }

    val volTrans = new VolumeTranslator(sorted)
    // fine location in mm coordinates
    if (fineLocation_vox.isDefined) {
      val fineLocation_mm = volTrans.vox2mm(fineLocation_vox.get)
      val imageXYZ = makeImagesXYZ(entireVolume, fineLocation_vox.get, fineLocation_mm, voxSize_mm)
      Right(fineLocation_mm, imageXYZ)
    } else {
      Left("No BB found")
    }
  }
}
