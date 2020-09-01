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
import org.aqa.db.BBbyCBCT
import org.aqa.web.WebUtil
import org.aqa.db.Output
import com.pixelmed.dicom.AttributeTag
import org.restlet.Response
import javax.vecmath.Matrix4d
import edu.umro.ScalaUtil.DicomUtil

/**
 * After the data has has been validated as sufficient to do the analysis, perform the
 * various processing steps, including finding the BB in 3D, saving results to the
 * database, and generating an HTML report.
 */
object BBbyCBCTExecute extends Logging {

  private val subProcedureName = "CBCT Alignment"
  private val tableDefault = 2000000.0

  private def showFailure(message: String, extendedData: ExtendedData, runReq: BBbyCBCTRunReq) = {
    val content = {
      <div class="row col-md-10 col-md-offset-1">
        <h4>
          Failed to process CBCT images:<br></br>
          <div class="row col-md-10 col-md-offset-1">
            <i>{ message }</i>
          </div>
          <div class="row col-md-10 col-md-offset-1">
            { BBbyCBCTHTML.makeCbctSlices(extendedData, runReq) }
          </div>
        </h4>
      </div>
    }
    val text = WebUtil.wrapBody(content, "CBCT Analysis Failed")
    val display = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(display, text)
  }

  /**
   * Construct the database row and insert it.
   */
  private def saveToDb(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, bbPointInRtplan: Point3d): BBbyCBCT = {

    val rtplanIsocenter = Util.getPlanIsocenterList(runReq.rtplan).head

    def getDbl(tag: AttributeTag) = runReq.cbctList.head.get(tag).getDoubleValues.head

    val bbByCBCT = new BBbyCBCT(
      None, // bbByCBCTPK
      extendedData.output.outputPK.get, // outputPK
      Util.sopOfAl(runReq.rtplan), // rtplanSOPInstanceUID
      Util.sopOfAl(runReq.cbctList.head), // cbctSeriesInstanceUid
      rtplanIsocenter.distance(bbPointInRtplan), // offset_mm
      ProcedureStatus.pass.toString, // status
      rtplanIsocenter.getX, // planX_mm
      rtplanIsocenter.getY, // planY_mm
      rtplanIsocenter.getZ, // planZ_mm
      bbPointInRtplan.getX, // cbctX_mm
      bbPointInRtplan.getY, // cbctY_mm
      bbPointInRtplan.getZ, // cbctZ_mm
      getDbl(TagFromName.TableTopLateralPosition), // tableXlateral_mm
      -getDbl(TagFromName.TableHeight), // tableYvertical_mm
      getDbl(TagFromName.TableTopLongitudinalPosition) // tableZlongitudinal_mmf
    )

    logger.info("Inserting BBbyCBCT into database: " + bbByCBCT)

    bbByCBCT.insert
    bbByCBCT
  }

  private def getTransformMatrix(runReq: BBbyCBCTRunReq, cbctFrameOfRefLocationBB_mm: Point3d): Matrix4d = {
    val matrix = if (runReq.reg.isDefined) {
      runReq.imageRegistration.get.getMatrix
    } else {
      logger.info("Using identity matrix for transforming points between CBCT frame of reference and RTPLAN frame of reference.")
      val m = new Matrix4d
      m.setIdentity
      m
    }
    def f(d: Double) = d.formatted("%12.6f")
    def sp = " ".formatted("%12s")
    val p = Util.transform(matrix, cbctFrameOfRefLocationBB_mm)

    //      if (runReq.reg.isDefined) runReq.imageRegistration.get.transform(cbctFrameOfRefLocationBB_mm) else cbctFrameOfRefLocationBB_mm

    logger.info("Matrix for transforming points between CBCT frame of reference and RTPLAN frame of reference.\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getX)} * ${f(matrix.getM00)}, ${f(matrix.getM01)}, ${f(matrix.getM02)}, ${f(matrix.getM03)}, ${f(p.getX)}\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getY)} * ${f(matrix.getM10)}, ${f(matrix.getM11)}, ${f(matrix.getM12)}, ${f(matrix.getM13)}, ${f(p.getY)}\n" +
      s"${f(cbctFrameOfRefLocationBB_mm.getZ)} * ${f(matrix.getM20)}, ${f(matrix.getM21)}, ${f(matrix.getM22)}, ${f(matrix.getM23)}, ${f(p.getZ)}\n" +
      s"${sp} * ${f(matrix.getM30)}, ${f(matrix.getM31)}, ${f(matrix.getM32)}, ${f(matrix.getM33)}")
    matrix

  }

  def runProcedure(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, response: Response): ProcedureStatus.Value = {
    try {
      // This code only reports values and considers the test to have passed if
      // it found the BB, regardless of whether the BB was positioned within
      // tolerance of the plan's isocenter.
      logger.info("Starting analysis of CBCT Alignment for machine " + extendedData.machine.id)
      val result = BBbyCBCTAnalysis.volumeAnalysis(runReq.cbctList)
      if (result.isRight) {
        val cbctFrameOfRefLocationBB_mm = result.right.get.cbctFrameOfRefLocation_mm
        val imageXYZ = result.right.get.imageXYZ
        logger.info("Found BB in CBCT volume.  XYZ Coordinates in original CBCT space: " +
          Util.fmtDbl(cbctFrameOfRefLocationBB_mm.getX) + ", " + Util.fmtDbl(cbctFrameOfRefLocationBB_mm.getY) + ", " + Util.fmtDbl(cbctFrameOfRefLocationBB_mm.getZ))

        val transformMatrix = getTransformMatrix(runReq, cbctFrameOfRefLocationBB_mm)

        val bbPointInRtplan = Util.transform(transformMatrix, cbctFrameOfRefLocationBB_mm)

        val bbByCBCT = saveToDb(extendedData, runReq, bbPointInRtplan)
        if (true) { // TODO rm
          logger.info("err_mm: " + bbByCBCT.err_mm.toString)
          Trace.trace("Saving original non-annotated images.")
          imageXYZ.zipWithIndex.map(ii => {
            val name = "orig_" + ii._2 + ".png"
            val pngFile = new java.io.File(extendedData.output.dir, name)
            Util.writePng(ii._1, pngFile)
            Trace.trace("Saved original non-annotated image: " + pngFile.getAbsolutePath)
          })
        }
        //  val cbctFrameOfRefLocationRtplanOrigin_mm = if (runReq.reg.isDefined)   getCbctFrameOfRefLocationRtplanOrigin_mm(  runReq.imageRegistration.get.getMatrix
        //def mm2vox(p: Point3d) = result.right.get.volumeTranslator.mm2vox(p) // .mm2vox(p)

        def mm2vox(x: Double, y: Double, z: Double) = {
          val pRTPLAN = new Point3d(x, y, z)
          val pCBCT = Util.invTransform(transformMatrix, pRTPLAN)
          result.right.get.volumeTranslator.mm2vox(pCBCT)
        }

        val bb_vox = mm2vox(bbByCBCT.cbctX_mm, bbByCBCT.cbctY_mm, bbByCBCT.cbctZ_mm)
        val rtplanOrigin_vox = mm2vox(bbByCBCT.rtplanX_mm, bbByCBCT.rtplanY_mm, bbByCBCT.rtplanZ_mm)

        val annotatedImages = BBbyCBCTAnnotateImages.annotate(bbByCBCT, imageXYZ, runReq, bb_vox, rtplanOrigin_vox, extendedData.input.dataDate.get)
        val html = BBbyCBCTHTML.generateHtml(extendedData, bbByCBCT, annotatedImages, ProcedureStatus.done, runReq, result.right.get, response)
        logger.info("Finished analysis of CBCT Alignment for machine " + extendedData.machine.id)
        ProcedureStatus.pass
      } else {
        showFailure(result.left.get, extendedData, runReq)
        ProcedureStatus.fail
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
        ProcedureStatus.crash
      }
    }
  }
}
