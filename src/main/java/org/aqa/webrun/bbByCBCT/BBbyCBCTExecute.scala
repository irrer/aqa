package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
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
   * Get the matrix from the REG DICOM and multiply it by the given point.
   *
   * @param runReq                      Contains REG
   * @param cbctFrameOfRefLocationBB_mm Point in CBCT space to be multiplied.
   * @return location of translated point.
   */
  private def getTransformMatrix(runReq: BBbyCBCTRunReq, cbctFrameOfRefLocationBB_mm: Point3d): Matrix4d = {
    val matrix = getRegMatrix(runReq)

    def f(d: Double) = d.formatted("%12.6f")

    def sp = " ".formatted("%12s")

    val p = Util.transform(matrix, cbctFrameOfRefLocationBB_mm)

    //      if (runReq.reg.isDefined) runReq.imageRegistration.get.transform(cbctFrameOfRefLocationBB_mm) else cbctFrameOfRefLocationBB_mm

    logger.info("Matrix for transforming points between CBCT frame of reference and RTPLAN frame of reference.\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getX)} * ${f(matrix.getM00)}, ${f(matrix.getM01)}, ${f(matrix.getM02)}, ${f(matrix.getM03)}, ${f(p.getX)}\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getY)} * ${f(matrix.getM10)}, ${f(matrix.getM11)}, ${f(matrix.getM12)}, ${f(matrix.getM13)}, ${f(p.getY)}\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getZ)} * ${f(matrix.getM20)}, ${f(matrix.getM21)}, ${f(matrix.getM22)}, ${f(matrix.getM23)}, ${f(p.getZ)}\n" +
      s"$sp * ${f(matrix.getM30)}, ${f(matrix.getM31)}, ${f(matrix.getM32)}, ${f(matrix.getM33)}")
    matrix

  }

  /*
   * Get the image position patient vector for the point where the BB was found.  Do this by
   * linearly interpolating between the two adjacent slices.
   *
   * @param runReq                      Data being processed.
   * @param cbctFrameOfRefLocationBB_mm Location of bb found via image analysis in mm in CBCT coordinate space.
   * @return
   *
  private def getLinearlyInterpolatedImagePositionPatient(runReq: BBbyCBCTRunReq, cbctFrameOfRefLocationBB_mm: Point3d) = {

    // z position in voxels truncated to an integer
    val zInt = cbctFrameOfRefLocationBB_mm.getZ.floor.toInt

    // The fraction of a voxel in the Z axis that the BB is between the slices
    val ratio = cbctFrameOfRefLocationBB_mm.getZ - zInt
    Trace.trace("ratio: " + ratio)
    Trace.trace("floor: " + cbctFrameOfRefLocationBB_mm.getZ.floor)

    // Get ipp (ImagePositionPatient) of an attribute list
    def ippOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues

    def zOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues()(2)

    Trace.trace("all Z: " + runReq.cbctList.map(zOf).sorted.mkString("    "))
    // get ImagePositionPatient of the slices adjacent to the bb center
    val loIPP = ippOf(runReq.cbctList(zInt))
    val hiIPP = ippOf(runReq.cbctList(zInt + 1))

    def linearInterpolationOf(lo: Double, hi: Double) = ((hi - lo) * ratio) + lo

    val x = linearInterpolationOf(loIPP(0), hiIPP(0))
    val y = linearInterpolationOf(loIPP(1), hiIPP(1))
    val z = linearInterpolationOf(loIPP(2), hiIPP(2))

    val ipp = new Point3d(x, y, z)
    Trace.trace("ipp: " + ipp)
    ipp
  }
  */


  /**
   * Get the image position patient vector for the point where the BB was found.  Do this by
   * linearly interpolating between the two adjacent slices.
   *
   * @param runReq           Data being processed.
   * @param fineLocation_vox Location of bb found via image analysis in voxels in CBCT coordinate space.
   * @return Patient position at the location of the BB.
   */
  private def getLinearlyInterpolatedImagePositionPatient(runReq: BBbyCBCTRunReq, fineLocation_vox: Point3d) = {

    // The fraction of a voxel in the Z axis that the BB is between the slices
    val ratio = {
      val span = runReq.cbctList.size - 1
      (span - fineLocation_vox.getZ) / span
    }
    Trace.trace("ratio: " + ratio)
    Trace.trace("floor: " + fineLocation_vox.getZ.floor)

    // Get ipp (ImagePositionPatient) of an attribute list
    def ippOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues

    def zOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues()(2)

    // Trace.trace("all Z: " + runReq.cbctList.map(zOf).sorted.mkString("    "))
    // get ImagePositionPatient of the slices adjacent to the bb center
    val loIPP = ippOf(runReq.cbctList.head)
    val hiIPP = ippOf(runReq.cbctList.last)

    def linearInterpolationOf(lo: Double, hi: Double) = ((hi - lo) * ratio) + lo

    val x = linearInterpolationOf(loIPP(0), hiIPP(0))
    val y = linearInterpolationOf(loIPP(1), hiIPP(1))
    val z = linearInterpolationOf(loIPP(2), hiIPP(2))

    val ipp = new Point3d(x, y, z)
    Trace.trace("ipp: " + ipp)
    ipp
  }

  private def calculate(runReq: BBbyCBCTRunReq, fineLocation_vox: Point3d): Point3d = {

    (0 to 5).foreach(i => println("----------------------------------------------------------------------"))
    val ipp = getLinearlyInterpolatedImagePositionPatient(runReq, fineLocation_vox)

    val pixelSpacing = runReq.cbctList.head.get(TagByName.PixelSpacing).getDoubleValues
    val psX = pixelSpacing.head
    val psY = pixelSpacing(1)


    // 2x3 matrix with 6 values indicating patient position.  Often this is 1 0 0 / 0 1 0, but only if the table is straight (not angled)
    val iop = runReq.cbctList.head.get(TagByName.ImageOrientationPatient).getDoubleValues
    Trace.trace("iop:\n" + iop)
    val jkm = new Matrix4d(
      iop(0) * psX, iop(3) * psY, 0, ipp.getX,
      iop(1) * psX, iop(4) * psY, 0, ipp.getY,
      iop(2) * psX, iop(5) * psY, 0, ipp.getZ,
      0, 0, 0, 1)

    Trace.trace("jkm matrix:\n" + jkm.toString)
    // val vec = new Point4d(cbctFrameOfRefLocationBB_mm.getX, cbctFrameOfRefLocationBB_mm.getY, 0, 1)
    val vec = new Point3d(fineLocation_vox.getX, fineLocation_vox.getY, 0)

    val transformMatrix = getTransformMatrix(runReq, fineLocation_vox)
    Trace.trace("transformMatrix matrix:\n" + transformMatrix.toString)

    val regMatrix = getRegMatrix(runReq)
    Trace.trace("regMatrix:\n" + regMatrix.toString)

    if (true) {
      val m = getRegMatrix(runReq)
      println("m:\n" + m)


      /*
      val d = Util.multiplyMatrix(tp2, jkm)
      println("d:\n" + d)
      val e = Util.transform(d, vec)
      println("e: " + e)
      */
    }
    //
    val a = getRegMatrix(runReq) //  Util.transposeMatrix(transformMatrix)
    Trace.trace("a matrix:\n" + a.toString)
    val b = Util.multiplyMatrix(a, jkm)
    Trace.trace("b matrix:\n" + b.toString)

    val c = Util.transform(b, vec)
    Trace.trace("c matrix:\n" + c.toString)

    (0 to 5).foreach(i => println("----------------------------------------------------------------------"))
    c
  }

  /** For testing only */
  def testCalculate(runReq: BBbyCBCTRunReq, fineLocation_vox: Point3d): Point3d = calculate(runReq, fineLocation_vox)

  def runProcedure(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, response: Response): ProcedureStatus.Value = {
    try {
      // This code only reports values and considers the test to have passed if
      // it found the BB, regardless of whether the BB was positioned within
      // tolerance of the plan's isocenter.
      logger.info("Starting analysis of CBCT Alignment for machine " + extendedData.machine.id)
      val result = BBbyCBCTAnalysis.volumeAnalysis(runReq.cbctList, extendedData.output.dir)
      if (result.isRight) {
        val cbctFrameOfRefLocationBB_mm = result.right.get.cbctFrameOfRefLocation_mm
        val imageXYZ = result.right.get.imageXYZ
        val fineLocation_vox = result.right.get.fineLocation_vox
        logger.info("Found BB in CBCT volume.  XYZ Coordinates in original CBCT space in voxels (fineLocation_vox): " + fineLocation_vox)
        // logger.info("Found BB in CBCT volume.  XYZ Coordinates in original CBCT space in mm: " + cbctFrameOfRefLocationBB_mm)

        // val newPos = calculate(runReq, fineLocation_vox)
        // Trace.trace("newPos: " + newPos)

        // frame of reference matrix
        val forMatrix = getTransformMatrix(runReq, cbctFrameOfRefLocationBB_mm)

        // val bbPointInRtplanOld = Util.transform(forMatrix, cbctFrameOfRefLocationBB_mm)
        val bbPointInRtplan = calculate(runReq, fineLocation_vox)
        // Trace.trace("oldPos: " + bbPointInRtplan)

        val bbByCBCT = saveToDb(extendedData, runReq, bbPointInRtplan)
        //  val cbctFrameOfRefLocationRtplanOrigin_mm = if (runReq.reg.isDefined)   getCbctFrameOfRefLocationRtplanOrigin_mm(  runReq.imageRegistration.get.getMatrix
        //def mm2vox(p: Point3d) = result.right.get.volumeTranslator.mm2vox(p) // .mm2vox(p)

        def mm2vox(x: Double, y: Double, z: Double) = {
          val pRTPLAN = new Point3d(x, y, z)
          val pCBCT = Util.invTransform(forMatrix, pRTPLAN)
          result.right.get.volumeTranslator.mm2vox(pCBCT)
        }

        val bb_vox = mm2vox(bbByCBCT.cbctX_mm, bbByCBCT.cbctY_mm, bbByCBCT.cbctZ_mm)

        // origin of RTPLAN in the CBCT voxel space
        val rtplanOrigin_vox = mm2vox(bbByCBCT.rtplanX_mm, bbByCBCT.rtplanY_mm, bbByCBCT.rtplanZ_mm)

        // Creating images takes a long time.
        val annotatedImages = BBbyCBCTAnnotateImages.annotate(bbByCBCT, imageXYZ, runReq, bb_vox, rtplanOrigin_vox, extendedData.input.dataDate.get)

        // Generating HTML takes a little time.
        BBbyCBCTHTML.generateHtml(extendedData, bbByCBCT, annotatedImages, runReq, result.right.get, response)
        logger.info("Finished analysis of CBCT Alignment for machine " + extendedData.machine.id)
        ProcedureStatus.pass
      } else {
        showFailure(result.left.get, extendedData, runReq)
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
