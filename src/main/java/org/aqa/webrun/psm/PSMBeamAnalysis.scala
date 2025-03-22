package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomBeam
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.ExtendedData

import java.awt.Point
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import javax.vecmath.Point2i

/**
  *
  * @param rtplan       DICOM RTPLAN for delivering beams.
  * @param extendedData Metadata
  * @param trans        For translating between pixel and isoplane coordinates.
  * @param rtimage      Beam image.
  */
case class PSMBeamAnalysis(rtplan: AttributeList, extendedData: ExtendedData, trans: IsoImagePlaneTranslator, rtimage: AttributeList) extends Logging {

  /**
    * Attempt to measure the position of each of the four edges.  This is not really necessary, but the data is
    * available so we might as well analyze and save it in case there is a potential use.
    *
    * In some cases there are artifacts in the images which make them impossible to
    *
    * @param center_mm  Center in isoplane in mm.
    * @param rtplanBeam Beam from RTPLAN.
    * @param dicomImage Image pixels.
    * @param beamName   Name of beam.
    * @return
    */
  private def measureEdges(center_mm: Point2D.Double, rtplanBeam: DicomBeam, dicomImage: DicomImage, beamName: String): Option[MeasureTBLREdges.AnalysisResult] = {
    // @formatter:off
    val topPlanned_mm    = center_mm.getY - rtplanBeam.y2Jaw.get
    val bottomPlanned_mm = center_mm.getY - rtplanBeam.y1Jaw.get
    val leftPlanned_mm   = center_mm.getX + rtplanBeam.x1Jaw.get
    val rightPlanned_mm  = center_mm.getX + rtplanBeam.x2Jaw.get
    // @formatter:on

    val tblr = MeasureTBLREdges.TBLR(topPlanned_mm, bottomPlanned_mm, leftPlanned_mm, rightPlanned_mm)

    val result = try {
      val ar = MeasureTBLREdges.measure(
        dicomImage,
        translator = trans,
        expected_mm = Some(tblr),
        collimatorAngle = 0,
        annotate = dicomImage,
        floodOffset = new Point,
        thresholdPercent = 0.5,
        markCenter = false
      )
      Some(ar)
    }
    catch {
      case t: Throwable =>
        Trace.trace(s"Unable to find edges for beam $beamName: ${fmtEx(t)}")
        None
    }

    result
  }

  /**
   * Use the planned center as the center of the beam in isoplane coordinates.
   *
   * @param planBeam beam.
   * @return Center of beam.
   */
  private def findCenter_iso(planBeam: DicomBeam): Point2D.Double = {

    val RTImagePosition = {
      val beamAl = DicomUtil.getBeamOfRtimage(planBeam.rtplan, planBeam.rtimage).get
      DicomUtil.findAllSingle(beamAl, TagByName.RTImagePosition).head.getDoubleValues
    }

    val x = -(RTImagePosition.head / trans.beamExpansionRatio)
    val y = RTImagePosition(1) / trans.beamExpansionRatio

    val p_iso = new Point2D.Double(x, y)

    p_iso
  }


  def measure(): PSMBeamAnalysisResult = {

    // grab part of RTPLAN that specifies this beam
    val rtplanBeam = DicomBeam(rtplan, rtimage)

    // name of beam
    val beamName = Util.normalizeBeamName(rtplanBeam.beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString)

    // Make sure that the collimator angle is 0.  If not, then the X1,X2,Y1,Y2 jaws will not be in the expected orientation
    val collimatorAngle = Util.angleRoundedTo90(rtplanBeam.rtimageCollimatorAngle)
    if (collimatorAngle != 0) {
      throw new RuntimeException("Collimator angle is required to be zero.")
    }

    val dicomImage = new DicomImage(rtimage)

    val RescaleSlope = rtimage.get(TagByName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = rtimage.get(TagByName.RescaleIntercept).getDoubleValues.head

    val center_iso = findCenter_iso(rtplanBeam)

    val center_pix = trans.iso2Pix(center_iso)

    def pixToCU(coordinate: Point2i): Double = (dicomImage.get(coordinate.getX, coordinate.getY) * RescaleSlope) + RescaleIntercept

    val pixelList = PSMUtil.pixelCoordinatesWithinRadius(rtimage, center_pix).map(coordinate => (coordinate, pixToCU(coordinate))).toMap

    val mean_cu = pixelList.values.sum / pixelList.size

    val stdDev_cu = ImageUtil.stdDev(pixelList.values.map(_.toFloat).toSeq)

    val edges = measureEdges(center_iso, rtplanBeam, dicomImage, beamName)

    val ms = if (edges.isDefined) Some(edges.get.measurementSet) else None

    val psmBeam = org.aqa.db.PSMBeam(
      psmBeamPK = None,
      outputPK = extendedData.output.outputPK.get,
      xCenter_mm = center_iso.getX,
      yCenter_mm = center_iso.getY,
      SOPInstanceUID = Util.sopOfAl(rtimage),
      beamName = beamName,
      mean_cu = mean_cu,
      stdDev_cu = stdDev_cu,
      top_mm = ms.map(_.top),
      bottom_mm = ms.map(_.bottom),
      left_mm = ms.map(_.left),
      right_mm = ms.map(_.right)
    )

    val bufferedImage: BufferedImage = {
      if (edges.isDefined)
        edges.get.bufferedImage
      else {
        dicomImage.toDeepColorBufferedImage(0.05)
      }
    }

    PSMBeamAnalysisResult(psmBeam, rtimage, bufferedImage, pixelList)
  }
}
