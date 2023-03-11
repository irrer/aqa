package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.Util
import org.aqa.db.FocalSpot

import java.awt.Color

case class FSMeasure(rtplan: AttributeList, rtimage: AttributeList) {

  // val planned = PlannedRectangle(rtplan, rtimage)
  // val plannedTBLR = MeasureTBLREdges.TBLR(top = -planned.top, bottom = -planned.bottom, left = planned.left, right = planned.right)
  private val dicomImage = new DicomImage(rtimage)
  private val translator = new IsoImagePlaneTranslator(rtimage)
  private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val KVP = DicomUtil.findAllSingle(rtimage, TagByName.KVP).head.getDoubleValues.head
  private val ExposureTime = DicomUtil.findAllSingle(rtimage, TagByName.ExposureTime).head.getDoubleValues.head

  private val beam = Util.getBeamOfRtimage(plan = rtplan, rtimage).get

  def beamName: String = DicomUtil.getBeamNameOfRtimage(rtplan, rtimage).get

  /** True if the collimator angle rounded to 90 degrees is 90. */
  val is090: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 90

  /** True if the collimator angle rounded to 90 degrees is 270. */
  val is270: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 270

  /** True if the X1 and X2 boundaries of the field are defined by the MLC. */
  val isMLC: Boolean = {
    val typeList = DicomUtil.findAllSingle(beam, TagByName.RTBeamLimitingDeviceType).map(_.getSingleStringValueOrEmptyString().toUpperCase().trim)
    typeList.exists(_.equals("MLCX"))
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
    outputPK                    = -1, // TODO
    SOPInstanceUID              = Util.sopOfAl(rtimage),
    gantryAngleRounded_deg      = Util.angleRoundedTo90(Util.gantryAngle(rtimage ))  ,
    collimatorAngleRounded_deg  = Util.angleRoundedTo90(Util.collimatorAngle(rtimage )),
    beamName                    = Util.getBeamNameOfRtimage(rtplan, rtimage).get,
    KVP                         = KVP,
    ExposureTime                = ExposureTime,
    topEdge_mm                  = analysisResult.measurementSet.top,
    bottomEdge_mm               = analysisResult.measurementSet.bottom,
    leftEdge_mm                 = analysisResult.measurementSet.left,
    rightEdge_mm                = analysisResult.measurementSet.right,
    topEdgePlanned_mm           = -1, // plannedTBLR.top,
    bottomEdgePlanned_mm        = -1, // plannedTBLR.bottom,
    leftEdgePlanned_mm          = -1, // plannedTBLR.left,
    rightEdgePlanned_mm         = -1  // plannedTBLR.right
  )
  // @formatter:on

  Util.addGraticules(analysisResult.bufferedImage, new IsoImagePlaneTranslator(rtimage), Color.GRAY)
  val bufferedImage = analysisResult.bufferedImage

}
