package org.aqa.webrun.phase2.cbctAlign

import scala.xml.Elem
import javax.vecmath.Point3d
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.ExtendedData
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

case class CBCTAlign(x: Double, y: Double, z: Double) {
  // TODO
}

object CBCTAnalysis extends Logging {

  private val subProcedureName = "CBCT Alignment"

  case class CBCTAlignResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[Point3d]) extends SubProcedureResult(summry, stats, subProcedureName)

  private def slicePosition(attributeList: AttributeList): Double = attributeList.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  /**
   * Find the slice spacing by looking at the distance between consecutive sclices.  Use a few of
   * the smaller ones just in case there is a spacial discontinuity.
   */
  private def getSliceSpacing(sorted: Seq[AttributeList]) = {
    val sampleSize = 5
    val smallest = sorted.indices.tail.map(i => (slicePosition(sorted(i)) - slicePosition(sorted(i - 1))).abs).sorted.take(sampleSize)
    val size = smallest.sum / smallest.size
    size
  }

  /**
   * Get the size of a voxel in mm
   */
  def getVoxSize_mm(sorted: Seq[AttributeList]) = {
    val xSize = sorted.head.get(TagFromName.PixelSpacing).getDoubleValues()(0)
    val ySize = sorted.head.get(TagFromName.PixelSpacing).getDoubleValues()(1)
    val zSize = getSliceSpacing(sorted)
    Seq(xSize, ySize, zSize)
  }

  /**
   * Get the volume that needs to be searched for the BB.
   */
  private def getVolumeOfInterest(sorted: Seq[AttributeList], voxSize_mm: Point3d): DicomVolume = {
    val rows = sorted.head.get(TagFromName.Rows).getIntegerValues().head
    val columns = sorted.head.get(TagFromName.Columns).getIntegerValues().head
    // centers of volume in mm
    val xCenter_mm = ((rows - 1) * voxSize_mm.getX) / 2
    val yCenter_mm = ((columns - 1) * voxSize_mm.getY) / 2
    val zCenter_mm = (slicePosition(sorted.head) + slicePosition(sorted.head)) / 2.0

    // consider only slices that are sufficiently close to the center of the volume.
    val zSlices = sorted.filter(al => (slicePosition(al) - zCenter_mm).abs <= Config.DailyPhantomSearchDistance_mm)

    // rectangle within a slice in voxels
    val xyRect = {
      val x = (xCenter_mm - Config.DailyPhantomSearchDistance_mm) / voxSize_mm.getX
      val y = (yCenter_mm - Config.DailyPhantomSearchDistance_mm) / voxSize_mm.getY
      val width = (Config.DailyPhantomSearchDistance_mm * 2) / voxSize_mm.getX
      val height = (Config.DailyPhantomSearchDistance_mm * 2) / voxSize_mm.getY
      val rect = new Rectangle(x.round.toInt, y.round.toInt, width.round.toInt, height.round.toInt)
      rect
    }

    val imageList = zSlices.map(al => (new DicomImage(al)).getSubimage(xyRect))

    def zOffset = {
      val firstSop = Util.sopOfAl(zSlices.head)
      sorted.map(al => Util.sopOfAl(al)).indexOf(firstSop)
    }

    val offset = new Point3d(xyRect.getX, xyRect.getY, zOffset)

    new DicomVolume(imageList)
  }

  /**
   * Ensure that the profile contains a BB.
   */

  // TODO

  /**
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   */
  private def getMaxPoint(volOfInt: DicomVolume): Point3d = {

    val xPlaneProfile = volOfInt.volume.map(img => img.rowSums).map(voxel => voxel.sum)
    val yPlaneProfile = volOfInt.volume.map(img => img.columnSums).map(voxel => voxel.sum)
    val zPlaneProfile = volOfInt.volume.map(img => img.rowSums.sum)

    val xMax = xPlaneProfile.indexOf(xPlaneProfile.max)
    val yMax = yPlaneProfile.indexOf(yPlaneProfile.max)
    val zMax = zPlaneProfile.indexOf(zPlaneProfile.max)

    val xPosn = LocateMax.locateMax(xPlaneProfile)
    val yPosn = LocateMax.locateMax(yPlaneProfile)
    val zPosn = LocateMax.locateMax(zPlaneProfile)

    new Point3d(xPosn, yPosn, zPosn)
  }

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

  private def makeImagesXYZ(entireVolume: DicomVolume, fineLocation: Point3d, voxSize: Seq[Double]) = {

    val mm = Config.DailyPhantomBBPenumbra_mm
    val start = new Point3i(
      (fineLocation.getX - (mm / voxSize(0))).ceil.toInt,
      (fineLocation.getY - (mm / voxSize(1))).ceil.toInt,
      (fineLocation.getZ - (mm / voxSize(2))).ceil.toInt)

    val size = new Point3i(
      ((mm * 2) / voxSize(0)).ceil.toInt,
      ((mm * 2) / voxSize(1)).ceil.toInt,
      ((mm * 2) / voxSize(2)).ceil.toInt)

    val yVol = entireVolume.getSubVolume(new Point3i(0, start.getY, 0), new Point3i(entireVolume.xSize, size.getY, entireVolume.zSize))
    val zVol = entireVolume.getSubVolume(new Point3i(0, 0, start.getZ), new Point3i(entireVolume.xSize, entireVolume.ySize, size.getZ))

    val xImage = {
      val xVol = entireVolume.getSubVolume(new Point3i(start.getX, 0, 0), new Point3i(size.getX, entireVolume.ySize, entireVolume.zSize))
      def xSum(y: Int, z: Int): Float = {
        if (true) { // TODO rm
          try {
            val j = for (x <- 0 until xVol.xSize) yield (xVol.getXYZ(x, y, z))
          } catch {
            case t: Throwable => {
              0.0
            }
          }
        }
        val xx = for (x <- 0 until xVol.xSize) yield (xVol.getXYZ(x, y, z))
        (xx.sum) / xVol.xSize
      }

      def xRow(y: Int): IndexedSeq[Float] = {
        for (z <- 0 until xVol.zSize) yield (xSum(y, z))
      }

      val grid = for (y <- 0 until xVol.ySize) yield (xRow(y)).toIndexedSeq
      val image = new DicomImage(grid)

      if (true) { // TODO rm
        val name = System.currentTimeMillis + "_x.png"
        val outFile = new File("""D:\tmp\aqa\CBCT\perspectives\""" + name)
        outFile.getParentFile.mkdirs
        outFile.delete
        val bufImg = image.toDeepColorBufferedImage(0)
        ImageUtil.writePngFile(bufImg, outFile)
        println("wrote file: " + outFile.getAbsolutePath)
      }

    }

    //???
  }

  /**
   * Look in a center cubic volume of the CBCT set for the BB.
   *
   * The BB must be within a certain distance or the test fails.  Taking advantage of this
   * requirement greatly speeds the algorithm because it has fewer voxels to search.
   */
  private def analyze(cbctSeries: Seq[AttributeList]): Either[String, Point3d] = {
    val sorted = cbctSeries.sortBy(al => slicePosition(al))

    val voxSize_mm = getVoxSize_mm(sorted)
    Trace.trace(voxSize_mm)
    val entireVolume = DicomVolume.constructDicomVolume(sorted)
    Trace.trace(entireVolume.volSize)

    val searchStart = startOfSearch(entireVolume, voxSize_mm)
    Trace.trace(searchStart)
    val searchSize = sizeOfSearch(voxSize_mm)
    Trace.trace(searchSize)

    // sub-volume of the CBCT volume to be searched for the BB.
    val searchVolume = entireVolume.getSubVolume(searchStart, searchSize)
    Trace.trace

    // coarse location relative to entire volume
    //    val coarseLocation = {
    //      val rel = searchVolume.getMaxPoint // relative to search volume
    //      Trace.trace(rel)
    //      Seq(rel.getX + searchStart.getX, rel.getY + searchStart.getY, rel.getZ + searchStart.getZ)
    //    }
    //    Trace.trace(coarseLocation)

    val coarse2 = {
      val size = new Point3i(4, 4, 2)
      val high = searchVolume.getHighest(size)
      Trace.trace(high)
      high.add(searchStart)
      Trace.trace(high)
      high.add(new Point3i(-2, -2, -1))
      Trace.trace(high)
      high
    }
    Trace.trace(coarse2)
    val coarseLocation = Seq(coarse2.getX.toDouble, coarse2.getY.toDouble, coarse2.getZ.toDouble)

    val bbVolumeStart = coarseLocation.zip(voxSize_mm).map(cs => (cs._1 - (Config.DailyPhantomBBPenumbra_mm / cs._2)).round.toInt)
    Trace.trace(bbVolumeStart)

    // sub-volume of the entire volume that contains just the BB according to the coarse location.
    val bbVolume = {
      val bbVolumeSize = voxSize_mm.map(s => ((Config.DailyPhantomBBPenumbra_mm * 2) / s).round.toInt)
      Trace.trace(bbVolumeSize)
      entireVolume.getSubVolume(new Point3i(bbVolumeStart.toArray), new Point3i(bbVolumeSize.toArray))
    }
    Trace.trace

    val fineLocation = {
      val rel = bbVolume.getMaxPoint
      val finloc = new Point3d(rel.getX + bbVolumeStart(0), rel.getY + bbVolumeStart(1), rel.getZ + bbVolumeStart(2))
      finloc
    }

    Trace.trace(fineLocation)

    makeImagesXYZ(entireVolume, fineLocation, voxSize_mm)
    Right(fineLocation)
  }

  /**
   * For testing only.
   */
  def testAnalyze(cbctSeries: Seq[AttributeList]) = analyze(cbctSeries)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, CBCTAlignResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CBCT Alignment")
      logger.info("Finished analysis of CBCT Alignment")
      ???
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}

