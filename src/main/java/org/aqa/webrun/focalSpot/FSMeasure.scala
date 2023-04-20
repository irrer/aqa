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
import java.awt.image.BufferedImage

case class FSMeasure(rtplan: AttributeList, rtimage: AttributeList, outputPK: Long) {

  // val planned = PlannedRectangle(rtplan, rtimage)
  // val plannedTBLR = MeasureTBLREdges.TBLR(top = -planned.top, bottom = -planned.bottom, left = planned.left, right = planned.right)
  private val dicomImage = new DicomImage(rtimage)
  private val translator = {
    val RTImageSID: Double = FocalSpot.roundRTImageSID(rtimage.get(TagByName.RTImageSID).getDoubleValues.head)
    new IsoImagePlaneTranslator(rtimage, Some(RTImageSID))
  }
  private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val KVP = DicomUtil.findAllSingle(rtimage, TagByName.KVP).head.getDoubleValues.head
  private val mv = KVP / 1000.0

  /** MV energy formatted to the minimal string that represents its full precision. */
  val mvText: String = if (mv.round == mv) mv.round.toString else mv.toString
  private val ExposureTime = DicomUtil.findAllSingle(rtimage, TagByName.ExposureTime).head.getDoubleValues.head

  private val XRayImageReceptorTranslation = {
    val xrayTrans = DicomUtil.findAllSingle(rtimage, TagByName.XRayImageReceptorTranslation).head.getDoubleValues
    new javax.vecmath.Point3d(xrayTrans.head, xrayTrans(1), xrayTrans(2))
  }

  private val beam = Util.getBeamOfRtimage(plan = rtplan, rtimage).get
  private val dicomBeam = DicomBeam(rtplan, rtimage)

  val gantryAngleRounded_deg: Int = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
  val collimatorAngleRounded_deg: Int = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

  def beamName: String = DicomUtil.getBeamNameOfRtimage(rtplan, rtimage).get

  val NominalBeamEnergy: Double = DicomUtil.findAllSingle(beam, TagByName.NominalBeamEnergy).head.getDoubleValues.head

  val RTImageSID_mm: Double = translator.rtimageSid // rtimage.get(TagByName.RTImageSID).getDoubleValues.head
  /** Distance in mm from source to EPID. */
  val dEpid_mm: Double = RTImageSID_mm

  /** True if the collimator angle rounded to 90 degrees is 90. */
  val is090: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 90

  /** True if the collimator angle rounded to 90 degrees is 270. */
  val is270: Boolean = Util.angleRoundedTo90(Util.collimatorAngle(rtimage)) == 270

  /** True if the X1 and X2 boundaries of the field are defined by the MLC. */
  val isMLC: Boolean = dicomBeam.mlcX1PosList.nonEmpty

  private val X1Planned_mm: Double = {
    if (isMLC)
      -dicomBeam.mlcX1PosList(dicomBeam.mlcX1PosList.size / 2)
    else
      -dicomBeam.x1Jaw.get
  }

  private val X2Planned_mm: Double = {
    if (isMLC)
      -dicomBeam.mlcX2PosList(dicomBeam.mlcX2PosList.size / 2)
    else
      -dicomBeam.x2Jaw.get
  }

  private val Y1Planned_mm: Double = {
    if (isMLC) {

      val list1 = dicomBeam.mlcX1PosList
      val indices = list1.indices.dropRight(1)
      val index1 = indices.find(i => list1(i) != list1(i + 1))

      val list2 = dicomBeam.mlcX2PosList
      val index2 = indices.find(i => list2(i) != list2(i + 1))

      val index = Seq(index1, index2).flatten.head

      dicomBeam.leafBoundaryList(index + 1)
    } else
      dicomBeam.y1Jaw.get
  }

  private val Y2Planned_mm: Double = {
    if (isMLC) {
      val list1 = dicomBeam.mlcX1PosList
      //noinspection ReverseFind
      val indices = list1.indices.tail.reverse
      val index1 = indices.find(i => list1(i) != list1(i - 1))

      val list2 = dicomBeam.mlcX2PosList
      //noinspection ReverseFind
      val index2 = indices.find(i => list2(i) != list2(i - 1))

      val index = Seq(index1, index2).flatten.head

      dicomBeam.leafBoundaryList(index)
    } else
      -dicomBeam.y1Jaw.get
  }

  /** True if the X1 and X2 boundaries of the field are defined by the jaw. */
  val isJaw: Boolean = !isMLC

  /** True if this is an FFF beam. */
  val isFFF: Boolean = DicomUtil.findAllSingle(beam, TagByName.FluenceModeID).map(_.getSingleStringValueOrEmptyString()).exists(_.toUpperCase().contains("FFF"))

  // @formatter:off
  val analysisResult: MeasureTBLREdges.AnalysisResult = MeasureTBLREdges.measure(
    dicomImage,
    translator,
    expected_mm = None, // Some(plannedTBLR),
    collimatorAngle: Double,
    annotate = dicomImage,
    floodOffset = new java.awt.Point(0, 0),
    thresholdPercent = 0.5)

  val focalSpot: FocalSpot = FocalSpot(
    focalSpotPK                      = None,
    outputPK                         = outputPK,
    SOPInstanceUID                   = Util.sopOfAl(rtimage),
    gantryAngleRounded_deg           = gantryAngleRounded_deg,
    collimatorAngleRounded_deg       = collimatorAngleRounded_deg,
    beamName                         = Util.getBeamNameOfRtimage(rtplan, rtimage).get,
    isJaw                            = isJaw,
    isFFF                            = isFFF,
    KVP_kv                           = KVP,
    RTImageSID_mm                    = rtimage.get(TagByName.RTImageSID).getDoubleValues.head,
    ExposureTime                     = ExposureTime,
    XRayImageReceptorTranslationX_mm = XRayImageReceptorTranslation.getX,
    XRayImageReceptorTranslationY_mm = XRayImageReceptorTranslation.getY,
    XRayImageReceptorTranslationZ_mm = XRayImageReceptorTranslation.getZ,
    topEdge_mm                       = translator.pix2IsoCoordY(analysisResult.measurementSet.top),
    bottomEdge_mm                    = translator.pix2IsoCoordY(analysisResult.measurementSet.bottom),
    leftEdge_mm                      = translator.pix2IsoCoordX(analysisResult.measurementSet.left),
    rightEdge_mm                     = translator.pix2IsoCoordX(analysisResult.measurementSet.right),
    topEdgePlanned_mm                = X2Planned_mm, // plannedTBLR.top,
    bottomEdgePlanned_mm             = X1Planned_mm, // plannedTBLR.bottom,
    leftEdgePlanned_mm               = Y1Planned_mm, // plannedTBLR.left,
    rightEdgePlanned_mm              = Y2Planned_mm  // plannedTBLR.right
  )
  // @formatter:on

  val fluenceName: String = focalSpot.fluenceName

  Util.addGraticules(analysisResult.bufferedImage, new IsoImagePlaneTranslator(rtimage), Color.GRAY)
  val bufferedImage: BufferedImage = analysisResult.bufferedImage

}
