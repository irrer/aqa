package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.dailyQA.DailyQAActivity
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Response

import java.io.File
import javax.vecmath.Matrix4d
import javax.vecmath.Point3d

/**
 * After the data has has been validated as sufficient to do the analysis, perform the
 * various processing steps, including finding the BB in 3D, saving results to the
 * database, and generating an HTML report.
 */
object BBbyCBCTExecute extends Logging {

  private val subProcedureName = "CBCT Alignment"

  private def showFailure(message: String, extendedData: ExtendedData, runReq: BBbyCBCTRunReq): Unit = {
    val content = {
      <div class="row col-md-10 col-md-offset-1">
        <h4>
          Failed to process CBCT images:
          <br></br>
          <div class="row col-md-10 col-md-offset-1">
            <i>
              {message}
            </i>
          </div>
          <div class="row col-md-10 col-md-offset-1">
            {BBbyCBCTHTML.makeCbctSlices(extendedData, runReq)}
          </div>
        </h4>
      </div>
    }

    val text = WebUtil.wrapBody(BBbyCBCTHTML.wrap(content, extendedData), "CBCT Analysis Failed")

    val display = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(display, text)
  }

  /**
   * Construct the database row and insert it.
   */
  private def saveToDb(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, bbPointInRtplan: Point3d): BBbyCBCT = {

    val rtplanIsocenter = Util.getPlanIsocenterList(runReq.rtplan).head

    def getDbl(tag: AttributeTag) = runReq.cbctList.head.get(tag).getDoubleValues.head

    // grab the first slice of the series, remove the image data, and zip it into a byte array
    val metadata_dcm_zip = {
      val al = DicomUtil.clone(runReq.cbctList.head)
      al.remove(TagFromName.PixelData)
      Some(DicomUtil.dicomToZippedByteArray(Seq(al)))
    }

    val bbByCBCT = new BBbyCBCT(
      None, // bbByCBCTPK
      extendedData.output.outputPK.get, // outputPK
      Util.sopOfAl(runReq.rtplan), // rtplanSOPInstanceUID
      Util.serInstOfAl(runReq.cbctList.head), // cbctSeriesInstanceUid
      rtplanIsocenter.distance(bbPointInRtplan), // offset_mm
      ProcedureStatus.pass.toString, // status
      rtplanIsocenter.getX, // planX_mm
      rtplanIsocenter.getY, // planY_mm
      rtplanIsocenter.getZ, // planZ_mm
      bbPointInRtplan.getX, // cbctX_mm
      bbPointInRtplan.getY, // cbctY_mm
      bbPointInRtplan.getZ, // cbctZ_mm
      getDbl(TagByName.TableTopLateralPosition), // tableXlateral_mm
      -getDbl(TagFromName.TableHeight), // tableYvertical_mm.  Intentionally negated to conform to physicists' view.
      getDbl(TagByName.TableTopLongitudinalPosition), // tableZlongitudinal_mmf
      metadata_dcm_zip)

    logger.info("Inserting BBbyCBCT into database: " + bbByCBCT)

    bbByCBCT.insert
    bbByCBCT
  }


  private def getRegMatrix(runReq: BBbyCBCTRunReq): Matrix4d = {

    val matrix = if (runReq.reg.isDefined) {
      runReq.imageRegistration.get.getMatrix
    } else {
      logger.info("Using identity matrix for transforming points between CBCT frame of reference and RTPLAN frame of reference.")
      val m = new Matrix4d
      m.setIdentity()
      m
    }
    matrix
  }


  /**
   * Get the image position patient vector for the point where the BB was found.  Do this by
   * linearly interpolating between the two adjacent slices.
   *
   * @param runReq              Data being processed.
   * @param preciseLocation_vox Location of bb found via image analysis in voxels in CBCT coordinate space.
   * @return Patient position at the location of the BB.
   */
  private def getLinearlyInterpolatedImagePositionPatient(runReq: BBbyCBCTRunReq, preciseLocation_vox: Point3d) = {

    // The fraction of the distance between the farthest planes of the volume.
    val ratio = {
      val span = runReq.cbctList.size - 1
      preciseLocation_vox.getZ / span
    }

    // Get ipp (ImagePositionPatient) of an attribute list
    def ippOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues

    // get ImagePositionPatient of the slices adjacent to the bb center
    val loIPP = ippOf(runReq.cbctList.head)
    val hiIPP = ippOf(runReq.cbctList.last)

    def linearInterpolationOf(lo: Double, hi: Double) = ((hi - lo) * ratio) + lo

    val x = linearInterpolationOf(loIPP(0), hiIPP(0))
    val y = linearInterpolationOf(loIPP(1), hiIPP(1))
    val z = linearInterpolationOf(loIPP(2), hiIPP(2))

    val ipp = new Point3d(x, y, z)
    logger.info("ImagePositionPatient: " + ipp)
    ipp
  }


  /**
   * Get matrix that translates the image's patient position.  This is controlled by
   * ImageOrientationPatient and ImagePositionPatient.  The method here is to use
   * the voxel location to linearly interpolate the ImagePositionPatient between the
   * first and last slice.  That is then combined with ImageOrientationPatient and
   * pixel size to make a matrix that works in CBCT space in mm.
   *
   * @param runReq              DICOM used as input.
   * @param preciseLocation_vox Precisely located center of the BB in voxel coordinates in the CBCT space.
   * @return A matrix that will compensate for the table being angled.
   */
  private def getIppMatrix(runReq: BBbyCBCTRunReq, preciseLocation_vox: Point3d): Matrix4d = {
    val ipp = getLinearlyInterpolatedImagePositionPatient(runReq, preciseLocation_vox)

    val pixelSpacing = runReq.cbctList.head.get(TagByName.PixelSpacing).getDoubleValues
    // Get pixel size (image resolution).
    val psX = pixelSpacing.head
    val psY = pixelSpacing(1)

    // 2x3 matrix with 6 values indicating patient position.  Often this is 1 0 0 / 0 1 0, but only
    // if the table is straight (not angled).  While the DICOM standard allows it to be different for
    // each slice, it is the same of all slices of an CBCT image series.
    val iop = runReq.cbctList.head.get(TagByName.ImageOrientationPatient).getDoubleValues
    logger.info("ImageOrientationPatient:\n" + iop.mkString("Array(", ", ", ")"))
    val ippMatrix = new Matrix4d(
      iop(0) * psX, iop(3) * psY, 0, ipp.getX,
      iop(1) * psX, iop(4) * psY, 0, ipp.getY,
      iop(2) * psX, iop(5) * psY, 0, ipp.getZ,
      0, 0, 0, 1)
    ippMatrix
  }


  /**
   * Calculate the matrix that translates voxel coordinates in CBCT space to RTPLAN coordinates in mm.
   *
   * @param runReq              DICOM input.
   * @param preciseLocation_vox Location of BB in voxels in CBCT space.
   * @return Matrix that translates voxel coordinates in CBCT space to RTPLAN coordinates in mm.
   */
  private def getCombinedMatrix(runReq: BBbyCBCTRunReq, preciseLocation_vox: Point3d): Matrix4d = {
    val ippMatrix = getIppMatrix(runReq, preciseLocation_vox)
    val regMatrix = getRegMatrix(runReq)
    val combinedMatrix = Util.multiplyMatrix(regMatrix, ippMatrix)
    logger.info("ImagePatientPosition matrix:" + Util.formatMatrix(ippMatrix))
    logger.info("REG matrix:" + Util.formatMatrix(regMatrix))
    logger.info("combinedMatrix:" + Util.formatMatrix(combinedMatrix))
    logger.info("combinedMatrix:" + Util.formatMatrix(combinedMatrix))
    logger.info("combinedMatrix.determinant: " + combinedMatrix.determinant())
    combinedMatrix
  }


  /**
   * Determine the center of the BB in mm in RTPLAN coordinates.
   *
   * @param runReq              DICOM input.
   * @param preciseLocation_vox Location of BB in voxels in CBCT space.
   * @return The center of the BB in mm in RTPLAN coordinates.
   */
  private def calculateBbCenterInRtplan(runReq: BBbyCBCTRunReq, preciseLocation_vox: Point3d): Point3d = {
    val vec = new Point3d(preciseLocation_vox.getX, preciseLocation_vox.getY, 0)
    val combinedMatrix = getCombinedMatrix(runReq, preciseLocation_vox)
    val planPoint = Util.transform(combinedMatrix, vec)
    planPoint
  }


  /** For testing only */
  def testCalculate(runReq: BBbyCBCTRunReq, preciseLocation_vox: Point3d): Point3d = calculateBbCenterInRtplan(runReq, preciseLocation_vox)


  /**
   * Round trip RTPLAN origin to see if values match.  Do this by taking the calculated RTPLAN in voxel space
   * and multiplying it by the combined matrix.  It should end up with the same coordinates as the one retrieve
   * from the RTPLAN. They will not match exactly because of round off errors.  Round off errors are acceptable
   * if they are less than 10 significant figures, and in this case are even more so because the RTPLAN voxel
   * position is only used for annotating the image, and pixels on the screen have a much lower resolution
   * than the round off errors.
   *
   * This is only shown for debugging and diagnostic reasons.
   *
   * @param runReq           DICOM data needed to calculate combined matrix.
   * @param rtplanOrigin_vox Isocenter of RTPLAN in CBCT space in voxels.
   * @param rtplanOrigin_mm  Isocenter of RTPLAN in RTPLAN space in mm.
   */
  private def showRtplanRoundTrip(runReq: BBbyCBCTRunReq, rtplanOrigin_vox: Point3d, rtplanOrigin_mm: Point3d): Unit = {
    val combinedMatrix = getCombinedMatrix(runReq, rtplanOrigin_vox)
    val roundTrip = Util.transform(combinedMatrix, rtplanOrigin_vox)

    logger.info("bbByCBCT.rtplan vox                     : " + rtplanOrigin_vox)
    logger.info("bbByCBCT.rtplan mm                      : " + rtplanOrigin_mm)
    logger.info("round trip (should match above numbers) : " + roundTrip)
  }

  /**
   * Main entry for processing Daily QA CBCT data.
   *
   * @param extendedData Metadata
   * @param runReq       DICOM data
   * @param response     Report showing results.
   * @return Status indicating termination state.
   */
  def runProcedure(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, response: Response): ProcedureStatus.Value = {
    try {
      // This code only reports values and considers the test to have passed if
      // it found the BB, regardless of whether the BB was positioned within
      // tolerance of the plan's isocenter.
      logger.info("Starting analysis of CBCT Alignment for machine " + extendedData.machine.id)
      DailyQAActivity.update() // tell web page that data has changed
      val result = BBbyCBCTAnalysis.volumeAnalysis(runReq.cbctList, extendedData.output.dir)
      if (result.isRight) {
        val imageXYZ = result.right.get.imageXYZ
        val preciseLocation_vox = result.right.get.fineLocation_vox
        logger.info("Found BB in CBCT volume.  XYZ Coordinates in original CBCT space in voxels (preciseLocation_vox): " + preciseLocation_vox)

        val bbPointInRtplan = calculateBbCenterInRtplan(runReq, preciseLocation_vox)

        val bbByCBCT = saveToDb(extendedData, runReq, bbPointInRtplan)
        DailyQAActivity.update() // tell web page that data has changed

        // origin of RTPLAN in the CBCT voxel space
        val rtplanOrigin_vox = {
          val voxSize = Util.getVoxSize_mm(runReq.cbctList)
          val x_vox = preciseLocation_vox.getX - (bbByCBCT.err_mm.getX / voxSize.getX)
          val y_vox = preciseLocation_vox.getY - (bbByCBCT.err_mm.getY / voxSize.getY)
          val z_vox = preciseLocation_vox.getZ - (bbByCBCT.err_mm.getZ / voxSize.getZ)
          val o = new Point3d(x_vox, y_vox, z_vox)
          o
        }

        showRtplanRoundTrip(runReq, rtplanOrigin_vox, bbByCBCT.rtplan)

        // Creating images takes a long time.
        val annotatedImages = BBbyCBCTAnnotateImages.annotate(bbByCBCT, imageXYZ, runReq, preciseLocation_vox, rtplanOrigin_vox, extendedData.input.dataDate.get)

        // Generating HTML takes a little time.
        BBbyCBCTHTML.generateHtml(extendedData, bbByCBCT, annotatedImages, runReq, result.right.get, response)
        logger.info("Finished analysis of CBCT Alignment for machine " + extendedData.machine.id)
        ProcedureStatus.pass
      } else {
        showFailure(result.left.get, extendedData, runReq)
        DailyQAActivity.update() // tell web page that data has changed
        ProcedureStatus.fail
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
        ProcedureStatus.crash
    }
  }
}
