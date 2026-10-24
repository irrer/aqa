package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.db.SymmetryAndFlatness
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessRun.SymmetryAndFlatnessBeamResult
import org.aqa.Config
import org.aqa.webrun.psm.PSMGrid
import org.aqa.Logging
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
  * @param symmetryAndFlatnessBaselineRedoBeamList List of beams that were marked as baselines by the user.
  * @param isFlatteningFilterFree                  True if this is an FFF beam.
  * @param withPSM                                 If true, use PSM defined areas for sampling image
  */
case class SymmetryAndFlatnessAnalyze( //
    outputPK: Long,
    procedurePK: Long,
    machinePK: Long,
    dataDate: Timestamp,
    attributeList: AttributeList,
    beamName: String,
    collimatorCenter: Point2D.Double,
    // floodField: Option[AttributeList],
    symmetryAndFlatnessBaselineRedoBeamList: Seq[String],
    isFlatteningFilterFree: Boolean,
    withPSM: Boolean
) extends Logging {

  private def circleRadiusInPixels(isoImageTrans: IsoImagePlaneTranslator, radius_mm: Double): Double = {
    val imagePlaneCenterInPixels = isoImageTrans.iso2Pix(0, 0)
    val radiusInPixels = isoImageTrans.iso2Pix(radius_mm, radius_mm).distance(imagePlaneCenterInPixels)
    radiusInPixels
  }

  /** Energy for this beam. */
  private val KVP: Double = {
    val v = DicomUtil.findAllTag(attributeList, TagByName.KVP).head.getDoubleValues.head
    v
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
    * @param point                  Center of circle in image.
    * @param image                  Process this image
    * @param useCollimatorCentering If true, apply collimator centering.
    * @return Mean value of pixels in circle in CU.
    */
  private def evalPoint( //
      point: SymmetryAndFlatnessPoint,
      image: DicomImage,
      useCollimatorCentering: Boolean
  ): Double = {
    val center =
      if (useCollimatorCentering)
        new Point2D.Double(point.x_mm + collimatorCenter.getX, point.y_mm + collimatorCenter.getY)
      else
        new Point2D.Double(point.x_mm, point.y_mm)

    val pixList = Phase2Util.makeCenterDosePointList(attributeList, center, radius_mm = Config.SymmetryAndFlatnessDiameter_mm / 2)
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
    val pixList = Phase2Util.makeCenterDosePointList(attributeList, center, radius_mm = Config.SymmetryAndFlatnessDiameter_mm / 2)
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
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY + radius, "%6.4f".format(value), 270)
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
        if (withPSM)
          Config.PSMSpan_mm
        else
          Config.SymmetrySpan_mm
      }

      def beamToPoint(name: String, beam: PSMBeam): SymmetryAndFlatnessPoint =
        SymmetryAndFlatnessPoint(name, beam.xCenter_mm, beam.yCenter_mm)


      val radius_mm: Double = if (withPSM) Config.PSMRadius_mm else Config.CenterDoseRadius_mm

      def makeValueList(center: Point2D.Double): Seq[Float] = {
        val pointList = Phase2Util.makeCenterDosePointList(attributeList, center, radius_mm)
        pointList.map(p => scaledImage.get(p.x, p.y))
      }

      def meanOf(valueList: Seq[Float]): Double = valueList.map(_.toDouble).sum / valueList.size

      // @formatter:off
      val pointTop   : Point2D.Double = if (withPSM) Config.PSMTopPoint    else Config.SymmetryPointTop   .asPoint
      val pointBottom: Point2D.Double = if (withPSM) Config.PSMBottomPoint else Config.SymmetryPointBottom.asPoint
      val pointLeft  : Point2D.Double = if (withPSM) Config.PSMLeftPoint   else Config.SymmetryPointLeft  .asPoint
      val pointRight : Point2D.Double = if (withPSM) Config.PSMRightPoint  else Config.SymmetryPointRight .asPoint
      val pointCenter: Point2D.Double = if (withPSM) Config.PSMCenterPoint else Config.SymmetryPointCenter.asPoint

      val valueListTop    = makeValueList(pointTop   )
      val valueListBottom = makeValueList(pointBottom)
      val valueListLeft   = makeValueList(pointLeft  )
      val valueListRight  = makeValueList(pointRight )
      val valueListCenter = makeValueList(pointCenter)

      // @formatter:on

      val diameter_mm: Double = {
        Config.SymmetryAndFlatnessDiameter_mm
      }

      new SymmetryAndFlatness( //
        symmetryAndFlatnessPK = None,
        outputPK = outputPK,
        SOPInstanceUID = Util.sopOfAl(attributeList),
        beamName = beamName,
        isBaseline = symmetryAndFlatnessBaselineRedoBeamList.contains(beamName),

        // @formatter:off
        top_cu =    meanOf(valueListTop   ),
        bottom_cu = meanOf(valueListBottom),
        left_cu =   meanOf(valueListLeft  ),
        right_cu =  meanOf(valueListRight ),
        center_cu = meanOf(valueListCenter),

        topStdDev_cu    = ImageUtil.stdDev(valueListTop   ),
        bottomStdDev_cu = ImageUtil.stdDev(valueListBottom),
        leftStdDev_cu   = ImageUtil.stdDev(valueListLeft  ),
        rightStdDev_cu  = ImageUtil.stdDev(valueListRight ),
        centerStdDev_cu = ImageUtil.stdDev(valueListCenter),
        // @formatter:on

        span_mm = Some(span_mm),
        diameter_mm = Some(diameter_mm),
        RTImageSID_mm = Some(attributeList.get(TagByName.RTImageSID).getDoubleValues.head),
        KVP = Some(KVP),
        isFlatteningFilterFree = Some(isFlatteningFilterFree)
      )
      // @formatter:on
    }

    logger.info("Getting baseline values for beam " + beamName)

    // Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
    val baseline = SymmetryAndFlatness.getBaseline( //
      machinePK,
      span_mm = symmetryAndFlatness.span_mm,
      diameter_mm = symmetryAndFlatness.diameter_mm,
      RTImageSID_mm = symmetryAndFlatness.RTImageSID_mm,
      beamName = beamName,
      hasPSM = psmGrid.isDefined,
      // hasPSM = false,
      dataDate = dataDate,
      procedurePK = procedurePK
    ) match {
      case Some(bl) => bl.baseline
      case _ => symmetryAndFlatness
    }

    logger.info("Making annotated image of beam " + beamName)
    val annotatedImage = makeAnnotatedImage(correctedImage, symmetryAndFlatness, psmGrid)
    val psmGridBaseline: Option[PSMGrid] = {
      baseline.psmGridWithFFFrom() match {
        case Some(pf) => Some(pf.psmGrid)
        case _ => None
      }
    }
    val result = SymmetryAndFlatnessRun.SymmetryAndFlatnessBeamResult( //
      symmetryAndFlatness,
      annotatedImage,
      transverseProfile,
      transverse_pct,
      axialProfile,
      axial_pct,
      baseline,
      psmGrid,
      psmGridBaseline)

    logger.info("Finished analysis of beam " + beamName)

    result
  }

}
