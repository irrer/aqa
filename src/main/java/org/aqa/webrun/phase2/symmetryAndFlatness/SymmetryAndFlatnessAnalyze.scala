package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.db.SymmetryAndFlatness
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessRun.SymmetryAndFlatnessBeamResult
import org.aqa.Config
import org.aqa.webrun.psm.PSMGrid
import org.aqa.Logging
import org.aqa.db.Output
import org.aqa.db.PSMBeam

import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.Color
import java.awt.image.BufferedImage
import java.sql.Timestamp

/**
  * Perform the core analysis.
  *
  * @param outputPK                                Attach to this output.
  * @param procedurePK                             Procedure.
  * @param machinePK                               Machine.
  * @param dataDate                                Output's data date
  * @param attributeList                           DICOM of this beam.
  * @param beamName                                Name of beam.
  * @param collimatorCenter                        Collimator centering offset.
  * @param floodField                              PhaseAny flood field.
  * @param symmetryAndFlatnessBaselineRedoBeamList List of beams that were marked as baselines by the user.
  */
case class SymmetryAndFlatnessAnalyze( //
                                       outputPK: Long,
                                       procedurePK: Long,
                                       machinePK: Long,
                                       dataDate: Timestamp,
                                       attributeList: AttributeList,
                                       beamName: String,
                                       collimatorCenter: Point2D.Double,
                                       floodField: Option[AttributeList],
                                       symmetryAndFlatnessBaselineRedoBeamList: Seq[String]
) extends Logging {

  private def circleRadiusInPixels(isoImageTrans: IsoImagePlaneTranslator, radius_mm: Double): Double = {
    val imagePlaneCenterInPixels = isoImageTrans.iso2Pix(0, 0)
    val radiusInPixels = isoImageTrans.iso2Pix(radius_mm, radius_mm).distance(imagePlaneCenterInPixels)
    radiusInPixels
  }

  // val attributeList: AttributeList = getAttributeList(beamName, runReq)
  // val dicomImage = new DicomImage(attributeList)
  private val translator = new IsoImagePlaneTranslator(attributeList)
  private val widthOfBand_pix: Int = translator.iso2PixDistX(Config.SymmetryAndFlatnessDiameter_mm).round.toInt
  private val heightOfBand_pix: Int = translator.iso2PixDistY(Config.SymmetryAndFlatnessDiameter_mm).round.toInt

  private val scaledImage: DicomImage = new DicomImage(attributeList).scalePixels(attributeList)

  /**
    * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
    *
    * @param point : Center of circle in image.
    * @return Mean value of pixels in circle in CU.
    */
  private def evalPoint(point: SymmetryAndFlatnessPoint, image: DicomImage): Double = {
    val center = new Point2D.Double(point.x_mm + collimatorCenter.getX, point.y_mm + collimatorCenter.getY)
    val pixList = Phase2Util.makeCenterDosePointList(attributeList, center)
    val avg = pixList.map(p => image.get(p.x, p.y)).sum / pixList.size
    avg
  }

  /**
    * Get the standard deviation of the pixel values in CU for one spot in HU or CU or whatever units the image is using.
    *
    * @param point : Center of circle in image.
    * @return Standard deviation of pixels in circle in CU.
    */
  private def evalPointStdDev(point: SymmetryAndFlatnessPoint): Double = {
    val center = new Point2D.Double(point.x_mm + collimatorCenter.getX, point.y_mm + collimatorCenter.getY)
    val pixList = Phase2Util.makeCenterDosePointList(attributeList, center)
    val cuList = pixList.map(p => scaledImage.get(p.x, p.y))
    val stdDev_cu = ImageUtil.stdDev(cuList)
    stdDev_cu
  }

  private val transverseProfile: IndexedSeq[Double] = {
    logger.info("Making transverse profile of beam " + beamName)
    val y = ((translator.height - heightOfBand_pix) / 2.0).round.toInt
    val rectangle = new Rectangle(0, y, translator.width, heightOfBand_pix)
    val subImage = scaledImage.getSubimage(rectangle)
    val cuList = subImage.columnSums.map(_ / heightOfBand_pix.toDouble)
    cuList
  }

  private val axialProfile: IndexedSeq[Double] = {
    logger.info("Making axial profile of beam " + beamName)
    val x = ((translator.width - widthOfBand_pix) / 2.0).round.toInt
    val rectangle = new Rectangle(x, 0, widthOfBand_pix, translator.height)
    val subImage = scaledImage.getSubimage(rectangle)
    val cuList = subImage.rowSums.map(_ / widthOfBand_pix.toDouble)
    cuList
  }

  private val transverse_pct: IndexedSeq[Double] = (0 until translator.width).map(x => translator.pix2Iso(x, 0).getX)
  private val axial_pct: IndexedSeq[Double] = (0 until translator.height).map(y => translator.pix2Iso(0, y).getY)

  private def makeAnnotatedImage( //
                                  correctedImage: DicomImage,
                                  symmetryAndFlatness: SymmetryAndFlatness,
                                  psmGrid: Option[PSMGrid] = None // if defined, use the spacing in this grid
  ): BufferedImage = {
    val image = correctedImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(image)
    val graphics = ImageUtil.getGraphics(image)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius_mm = if (psmGrid.isDefined) Config.PSMRadius_mm else Config.SymmetryAndFlatnessDiameter_mm / 2
    val radius = circleRadiusInPixels(translator, radius_mm)
    val circleSize = (radius * 2).round.toInt

    Util.addGraticules(image, translator, Color.gray)

    Util.addAxialAndTransverse(image)

    def dbl2Text(d: Double): String = if (d.round.toInt == d) d.toInt.toString else "%8.3f".format(d).trim

    def annotatePoint(point: SymmetryAndFlatnessPoint, value: Double): Unit = {
      graphics.setColor(Color.black)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      val description = point.name + " " + dbl2Text(point.x_mm) + ", " + dbl2Text(point.y_mm)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY - radius, description, 90)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY + radius, value.formatted("%6.4f"), 270)
    }

    def pointOf(configured: SymmetryAndFlatnessPoint, psmBeam: Option[PSMBeam]): SymmetryAndFlatnessPoint = {
      if (psmBeam.isEmpty)
        configured
      else
        SymmetryAndFlatnessPoint(configured.name, psmBeam.get.xCenter_mm, psmBeam.get.yCenter_mm)
    }

    // @formatter:off
    annotatePoint(pointOf(Config.SymmetryPointTop   , psmGrid.map(_.topBeam   )), symmetryAndFlatness.top_cu   )
    annotatePoint(pointOf(Config.SymmetryPointBottom, psmGrid.map(_.bottomBeam)), symmetryAndFlatness.bottom_cu)
    annotatePoint(pointOf(Config.SymmetryPointRight , psmGrid.map(_.rightBeam )), symmetryAndFlatness.right_cu )
    annotatePoint(pointOf(Config.SymmetryPointLeft  , psmGrid.map(_.leftBeam  )), symmetryAndFlatness.left_cu  )
    annotatePoint(pointOf(Config.SymmetryPointCenter, psmGrid.map(_.centerBeam)), symmetryAndFlatness.center_cu)
    // @formatter:on

    image
  }

  /**
   * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
   * the database and generating a report.
   *
   */
  def analyze(
               correctedImage: DicomImage,
               psmGrid: Option[PSMGrid] = None // if defined, use the spacing in this grid
             ): SymmetryAndFlatnessBeamResult = {
    logger.info("Begin analysis of beam " + beamName)

    val symmetryAndFlatness = {

      /**
       * Get the distance between the centers of opposing measurement points.
       */
      val span_mm: Double = {
        if (psmGrid.isEmpty) {
          // no PSM, so use the configured values.
          val xList = Config.SymmetryAndFlatnessPointList.map(_.x_mm)
          val yList = Config.SymmetryAndFlatnessPointList.map(_.y_mm)
          val x = (xList.max - xList.min).abs
          val y = (yList.max - yList.min).abs
          Math.min(x, y)
        } else
          psmGrid.get.span

      }

      val psmDataDate = if (psmGrid.isDefined) {
        val psmOutput = Output.get(psmGrid.get.resultList.head.psmBeam.outputPK).get
        psmOutput.dataDate
      } else
        None

      val doPsm = psmGrid.isDefined

      def beamToPoint(name: String, beam: PSMBeam): SymmetryAndFlatnessPoint =
        SymmetryAndFlatnessPoint(name, beam.xCenter_mm, beam.yCenter_mm)

      def calcCu(psmBeam: Option[PSMBeam], symFlatPoint: SymmetryAndFlatnessPoint): Double = {
        if (doPsm) {
          val psmPoint = SymmetryAndFlatnessPoint("dummyName", psmBeam.get.xCenter_mm, psmBeam.get.yCenter_mm)
          val monthlyWd = evalPoint(psmPoint, scaledImage)
          val phaseAnyFloodField = psmBeam.get.floodField_cu.get
          val psm = psmBeam.get.psm
          val value = (monthlyWd / phaseAnyFloodField) / psm
          value
        } else
          evalPoint(symFlatPoint, scaledImage)

      }

      // @formatter:off

      val top_cu    = calcCu(psmGrid.map(_.topBeam   ), Config.SymmetryPointTop)
      val bottom_cu = calcCu(psmGrid.map(_.bottomBeam), Config.SymmetryPointBottom)
      val left_cu   = calcCu(psmGrid.map(_.leftBeam  ), Config.SymmetryPointLeft)
      val right_cu  = calcCu(psmGrid.map(_.rightBeam ), Config.SymmetryPointRight)
      val center_cu = calcCu(psmGrid.map(_.centerBeam), Config.SymmetryPointCenter)

      val topPoint    = if (doPsm) beamToPoint("top"   , psmGrid.get.topBeam   ) else Config.SymmetryPointTop
      val bottomPoint = if (doPsm) beamToPoint("bottom", psmGrid.get.bottomBeam) else Config.SymmetryPointBottom
      val leftPoint   = if (doPsm) beamToPoint("left"  , psmGrid.get.leftBeam  ) else Config.SymmetryPointLeft
      val rightPoint  = if (doPsm) beamToPoint("right" , psmGrid.get.rightBeam ) else Config.SymmetryPointRight
      val centerPoint = if (doPsm) beamToPoint("center", psmGrid.get.centerBeam) else Config.SymmetryPointCenter
      // @formatter:on

      val diameter_mm: Double = {
        if (psmGrid.isDefined)
          Config.PSMRadius_mm * 2
        else
          Config.SymmetryAndFlatnessDiameter_mm
      }

      new SymmetryAndFlatness( //
        symmetryAndFlatnessPK = None,
        outputPK = outputPK,
        SOPInstanceUID = Util.sopOfAl(attributeList),
        beamName = beamName,
        isBaseline = symmetryAndFlatnessBaselineRedoBeamList.contains(beamName),

        top_cu = top_cu,
        bottom_cu = bottom_cu,
        left_cu = left_cu,
        right_cu = right_cu,
        center_cu = center_cu,

        topStdDev_cu = evalPointStdDev(topPoint),
        bottomStdDev_cu = evalPointStdDev(bottomPoint),
        leftStdDev_cu = evalPointStdDev(leftPoint),
        rightStdDev_cu = evalPointStdDev(rightPoint),
        centerStdDev_cu = evalPointStdDev(centerPoint),

        psmDataDate = psmDataDate, // psmImageHash_md5,
        span_mm = Some(span_mm),
        diameter_mm = Some(diameter_mm),
        RTImageSID_mm = Some(attributeList.get(TagByName.RTImageSID).getDoubleValues.head)
      )
      // @formatter:on
    }

    logger.info("Getting baseline values for beam " + beamName)

    // Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
    val baseline = SymmetryAndFlatness.getBaseline(machinePK, //
      span_mm = symmetryAndFlatness.span_mm,
      diameter_mm = symmetryAndFlatness.diameter_mm,
      RTImageSID_mm = symmetryAndFlatness.RTImageSID_mm,
      beamName = beamName,
      hasPsm = psmGrid.isDefined,
      dataDate = dataDate,
      procedurePK = procedurePK) match {
      case Some(bl) => bl.baseline
      case _ => symmetryAndFlatness
    }

    logger.info("Making annotated image of beam " + beamName)
    val annotatedImage = makeAnnotatedImage(correctedImage, symmetryAndFlatness, psmGrid)
    val result = SymmetryAndFlatnessRun.SymmetryAndFlatnessBeamResult(symmetryAndFlatness, annotatedImage, transverseProfile, transverse_pct, axialProfile, axial_pct, baseline)

    logger.info("Finished analysis of beam " + beamName)

    result
  }

}
