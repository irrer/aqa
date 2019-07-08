package org.aqa.webrun.phase2.centerDose

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
  private def sliceSpacing(sorted: Seq[AttributeList]) = {
    val sampleSize = 5
    val smallest = sorted.indices.tail.map(i => (slicePosition(sorted(i)) - slicePosition(sorted(i - 1))).abs).sorted.take(sampleSize)
    val size = smallest.sum / smallest.size
    size
  }

  private def coarseCenter(sorted: Seq[AttributeList], voxSize: Point3d): Either[String, Point3d] = {

    ???
  }

  /**
   * Get the volume that needs to be searched for the BB.
   */
  private def getVolumeOfInterest(sorted: Seq[AttributeList], voxSize_mm: Point3d): VolumeOfInterest = {
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

    new DicomVolume(imageList, offset)
  }

  /**
   * Ensure that the profile contains a BB.
   */

  /**
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   */
  private def getMaxPoint(volOfInt: VolumeOfInterest): Point3d = {

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
   * Look in a center cubic volume of the CBCT set for the BB.
   *
   * The BB must be within a certain distance or the test fails.  Taking advantage of this
   * requirement greatly speeds the algorithm because it has fewer voxels to search.
   */
  private def analyze(cbctSeries: Seq[AttributeList]): Either[String, Point3d] = {
    val sorted = cbctSeries.sortBy(al => slicePosition(al))
    val xSize = cbctSeries.head.get(TagFromName.PixelSpacing).getDoubleValues()(0)
    val ySize = cbctSeries.head.get(TagFromName.PixelSpacing).getDoubleValues()(1)
    val zSize = sliceSpacing(sorted)

    val voxSize_mm = new Point3d(xSize, ySize, zSize)

    val volOfIntr = getVolumeOfInterest(sorted, voxSize_mm)

    //val maxPoint getMaxPoint (volOfIntr)

    ???
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
