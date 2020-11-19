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

  private def d2i(d: Double): Int = d.round.toInt

  private def d2i(d: Point3d): Point3i = new Point3i(d2i(d.getX), d2i(d.getY), d2i(d.getZ))

  private def i2d(i: Point3i): Point3d = new Point3d(i.getX.toDouble, i.getY.toDouble, i.getZ)

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
      val imageXYZ = BBbyCBCTMakeImageXYZ.makeImagesXYZ(entireVolume, fineLocation_vox, voxSize_mm)
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

    val coarseOpt_vox = (new BBbyCBCTCoarseCenter(entireVolume, voxSize_mm)).getCoarseCenter_vox

    coarseOpt_vox match {
      case Some(coarse_vox) => {

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
  }
}
