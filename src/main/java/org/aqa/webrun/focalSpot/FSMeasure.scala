package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomBeam
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.Util
import org.aqa.db.FocalSpot

import java.awt.Color

case class FSMeasure(rtplan: AttributeList, rtimage: AttributeList, outputPK: Long) {

  // val planned = PlannedRectangle(rtplan, rtimage)
  // val plannedTBLR = MeasureTBLREdges.TBLR(top = -planned.top, bottom = -planned.bottom, left = planned.left, right = planned.right)
  private val dicomImage = new DicomImage(rtimage)
  private val translator = new IsoImagePlaneTranslator(rtimage)
  private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val KVP = DicomUtil.findAllSingle(rtimage, TagByName.KVP).head.getDoubleValues.head
  private val ExposureTime = DicomUtil.findAllSingle(rtimage, TagByName.ExposureTime).head.getDoubleValues.head

  private val XRayImageReceptorTranslation = {
    val xrayTrans = DicomUtil.findAllSingle(rtimage, TagByName.XRayImageReceptorTranslation).head.getDoubleValues
    new javax.vecmath.Point3d(xrayTrans.head, xrayTrans(1), xrayTrans(2))
  }

  private val beam = Util.getBeamOfRtimage(plan = rtplan, rtimage).get
  private val dicomBeam = DicomBeam(rtplan, rtimage)

  val gantryAngleRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
  val collimatorAngleRounded_deg = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

  def beamName: String = DicomUtil.getBeamNameOfRtimage(rtplan, rtimage).get

  val NominalBeamEnergy = DicomUtil.findAllSingle(beam, TagByName.NominalBeamEnergy).head.getDoubleValues.head

  val RTImageSID_mm: Double = rtimage.get(TagByName.RTImageSID).getDoubleValues.head
  val dEpid_mm: Double = RTImageSID_mm

  /** True if the collimator angle rounded to 90 degrees is 90. */
  val is090: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 90

  /** True if the collimator angle rounded to 90 degrees is 270. */
  val is270: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 270

  /** True if the X1 and X2 boundaries of the field are defined by the MLC. */
  val isMLC: Boolean = dicomBeam.mlcX1PosList.nonEmpty

  val X1Planned_mm: Double = {
    if (isMLC)
      -dicomBeam.mlcX1PosList(dicomBeam.mlcX1PosList.size / 2)
    else
      -dicomBeam.x1Jaw.get
  }

  val X2Planned_mm: Double = {
    if (isMLC)
      -dicomBeam.mlcX2PosList(dicomBeam.mlcX2PosList.size / 2)
    else
      -dicomBeam.x2Jaw.get
  }

  val Y1Planned_mm: Double = {
    if (isMLC) {
      dicomBeam.mlcX1PosList(dicomBeam.mlcX1PosList.size / 2)
    } else
      dicomBeam.y1Jaw.get
  }

  val Y2Planned_mm: Double = {
    if (isMLC)
      dicomBeam.mlcX1PosList(dicomBeam.mlcX1PosList.size / 2)
    else
      dicomBeam.y1Jaw.get
  }

  /** True if the X1 and X2 boundaries of the field are defined by the jaw. */
  val isJaw: Boolean = !isMLC

  // @formatter:off
  private val analysisResult = MeasureTBLREdges.measure(
    dicomImage,
    translator,
    expected_mm = None, // Some(plannedTBLR),
    collimatorAngle: Double,
    annotate = dicomImage,
    floodOffset = new java.awt.Point(0, 0),
    thresholdPercent = 0.5)

  val focalSpot = FocalSpot(
    focalSpotPK                 = None,
    outputPK                    = outputPK,
    SOPInstanceUID              = Util.sopOfAl(rtimage),
    gantryAngleRounded_deg      = gantryAngleRounded_deg,
    collimatorAngleRounded_deg  = collimatorAngleRounded_deg,
    beamName                    = Util.getBeamNameOfRtimage(rtplan, rtimage).get,
    KVP                         = KVP,
    RTImageSID_mm               = RTImageSID_mm,
    ExposureTime                = ExposureTime,
    topEdge_mm                  = analysisResult.measurementSet.top - XRayImageReceptorTranslation.getY,
    bottomEdge_mm               = analysisResult.measurementSet.bottom - XRayImageReceptorTranslation.getY,
    leftEdge_mm                 = analysisResult.measurementSet.left + XRayImageReceptorTranslation.getX,
    rightEdge_mm                = analysisResult.measurementSet.right + XRayImageReceptorTranslation.getX,
    topEdgePlanned_mm           = if (collimatorAngleRounded_deg == 90) X2Planned_mm else X1Planned_mm, // plannedTBLR.top,
    bottomEdgePlanned_mm        = if (collimatorAngleRounded_deg == 90) X1Planned_mm else X2Planned_mm, // plannedTBLR.bottom,
    leftEdgePlanned_mm          = if (collimatorAngleRounded_deg == 90) Y2Planned_mm else Y1Planned_mm, // plannedTBLR.left,
    rightEdgePlanned_mm         = if (collimatorAngleRounded_deg == 90) Y1Planned_mm else Y2Planned_mm // plannedTBLR.right
  )
  // @formatter:on

  Util.addGraticules(analysisResult.bufferedImage, new IsoImagePlaneTranslator(rtimage), Color.GRAY)
  val bufferedImage = analysisResult.bufferedImage

}
