package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.CollimatorCentering
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.SymmetryAndFlatness.PointSet
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.Phase2Util.MaintenanceRecordBaseline
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.Color
import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.sql.Timestamp
import scala.xml.Elem

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

  private def boolToStatus(pass: Boolean) = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

  val axialSymmetryName = "Axial Symmetry"
  val transverseSymmetryName = "Transverse Symmetry"
  val flatnessName = "Flatness"
  val profileConstancyName = "Profile Constancy"

  /**
   * Encapsulate data for generating a report.
   */
  case class SymmetryAndFlatnessBeamResult(
                                            beamName: String,
                                            SOPInstanceUID: String,
                                            pointSet: PointSet,

                                            axialSymmetry: Double,
                                            axialSymmetryBaseline: Double,
                                            axialSymmetryStatus: ProcedureStatus.Value,

                                            transverseSymmetry: Double,
                                            transverseSymmetryBaseline: Double,
                                            transverseSymmetryStatus: ProcedureStatus.Value,

                                            flatness: Double,
                                            flatnessBaseline: Double,
                                            flatnessStatus: ProcedureStatus.Value,

                                            profileConstancy: Double,
                                            profileConstancyBaseline: Double,
                                            profileConstancyStatus: ProcedureStatus.Value,

                                            annotatedImage: BufferedImage,
                                            transverseProfile: Seq[Double], transverse_pct: IndexedSeq[Double],
                                            axialProfile: Seq[Double], axial_pct: IndexedSeq[Double],
                                            baselinePointSet: PointSet) {

    /** True if everything is ok. */
    val pass: Boolean = Seq(axialSymmetryStatus, transverseSymmetryStatus, flatnessStatus).forall(s => s.toString.equals(ProcedureStatus.pass.toString))
    logger.info("sym+flatness pass: " + pass)

    /** Aggregate status. */
    val status: ProcedureStatus.ProcedureStatus = boolToStatus(pass)
    logger.info("sym+flatness aggregate status: " + status)
  }

  def circleRadiusInPixels(isoImageTrans: IsoImagePlaneTranslator): Double = {
    val radius_mm = Config.SymmetryAndFlatnessDiameter_mm / 2
    val imagePlaneCenterInPixels = isoImageTrans.iso2Pix(0, 0)
    val radiusInPixels = isoImageTrans.iso2Pix(radius_mm, radius_mm).distance(imagePlaneCenterInPixels)
    radiusInPixels
  }

  private def makeAnnotatedImage(correctedImage: DicomImage, attributeList: AttributeList, pointSet: PointSet): BufferedImage = {
    val image = correctedImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(image)
    val graphics = ImageUtil.getGraphics(image)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = circleRadiusInPixels(translator)
    val circleSize = (radius * 2).round.toInt

    Util.addGraticules(image, translator, Color.gray)

    Util.addAxialAndTransverse(image)

    def dbl2Text(d: Double): String = if (d.round.toInt == d) d.toInt.toString else d.toString

    def annotatePoint(point: SymmetryAndFlatnessPoint, value: Double): Unit = {
      graphics.setColor(Color.black)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      val description = point.name + " " + dbl2Text(point.x_mm) + ", " + dbl2Text(point.y_mm)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY - radius, description, 90)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY + radius, value.formatted("%6.4f"), 270)
    }

    annotatePoint(Config.SymmetryPointTop, pointSet.top)
    annotatePoint(Config.SymmetryPointBottom, pointSet.bottom)
    annotatePoint(Config.SymmetryPointRight, pointSet.right)
    annotatePoint(Config.SymmetryPointLeft, pointSet.left)
    annotatePoint(Config.SymmetryPointCenter, pointSet.center)

    image
  }

  private def getAttributeList(beamName: String, runReq: RunReq): AttributeList = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood) runReq.flood
    else runReq.derivedMap(beamName).attributeList
  }

  /**
   * For each point (circle) on the image, make a list of average value of the pixels that are included in it.
   */
  private def makePointSet(dicomImage: DicomImage, attributeList: AttributeList, RescaleSlope: Double, RescaleIntercept: Double, collimatorCenterOfRotation: Point2D.Double): PointSet = {
    /**
     * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
     */
    def evalPoint(point: SymmetryAndFlatnessPoint): Double = {
      val center = new Point2D.Double(point.x_mm + collimatorCenterOfRotation.getX, point.y_mm + collimatorCenterOfRotation.getY)
      val pixList = Phase2Util.makeCenterDosePointList(attributeList, center)
      val avg = pixList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pixList.size
      (avg * RescaleSlope) + RescaleIntercept // convert
    }

    PointSet(evalPoint(Config.SymmetryPointTop), evalPoint(Config.SymmetryPointBottom),
      evalPoint(Config.SymmetryPointLeft), evalPoint(Config.SymmetryPointRight),
      evalPoint(Config.SymmetryPointCenter))
  }

  /**
   * public version of function to allow testing
   */
  def testMakePointSet(dicomImage: DicomImage, attributeList: AttributeList, RescaleSlope: Double, RescaleIntercept: Double): PointSet = {
    makePointSet(dicomImage, attributeList, RescaleSlope, RescaleIntercept, new Point2D.Double(0, 0))
  }

  def makeBaselineName(beamName: String, dataName: String): String = dataName + " " + beamName

  // case class BeamResultBaseline(result: SymmetryAndFlatnessBeamResult, maintenanceRecordBaseline: Seq[MaintenanceRecordBaseline], pointSet: PointSet) {}
  case class BeamResultBaseline(result: SymmetryAndFlatnessBeamResult, maintenanceRecordBaseline: Seq[MaintenanceRecordBaseline], pointSet: PointSet) {}

  /**
   * Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
   */
  private def getBaseline(machinePK: Long, beamName: String, dataName: String, attributeList: AttributeList, value: Double, dataDate: Timestamp): MaintenanceRecordBaseline = {
    val id = makeBaselineName(beamName, dataName)
    val maintenanceRecBaseline = Baseline.findLatest(machinePK, id, dataDate) match {
      case Some((maintenanceRecord, baseline)) => MaintenanceRecordBaseline(Some(maintenanceRecord), baseline)
      case _ => MaintenanceRecordBaseline(None, Baseline.makeBaseline(-1, dataDate, Util.sopOfAl(attributeList), id, value))
    }
    maintenanceRecBaseline
  }

  /**
   * Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
   */
  private def getBaseline2(machinePK: Long, beamName: String, dataDate: Timestamp): Option[SymmetryAndFlatness] = {
    val saf = SymmetryAndFlatness.getBaseline(machinePK, beamName, dataDate)
    saf
  }

  /**
   * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
   * the database and generating a report.
   *
   */
  private def analyze(beamName: String, machinePK: Long, dataDate: Timestamp, attributeList: AttributeList,
                      correctedImage: DicomImage, collimatorCentering: CollimatorCentering): BeamResultBaseline = {
    logger.info("Begin analysis of beam " + beamName)
    // val attributeList: AttributeList = getAttributeList(beamName, runReq)
    val dicomImage = new DicomImage(attributeList)
    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head
    val translator = new IsoImagePlaneTranslator(attributeList)
    val widthOfBand = circleRadiusInPixels(translator).round.toInt

    val pointSet = makePointSet(dicomImage, attributeList, RescaleSlope, RescaleIntercept, collimatorCentering.center)

    val axialSymmetry = pointSet.axialSymmetry
    logger.info("Axial symmetry of beam " + beamName + " : " + axialSymmetry)
    val transverseSymmetry = pointSet.transverseSymmetry
    logger.info("transverse symmetry of beam " + beamName + " : " + transverseSymmetry)

    val flatness = pointSet.flatness
    logger.info("Flatness of beam " + beamName + " : " + flatness)

    // logger.info("Getting corrected image of beam " + beamName)
    // val correctedImage = runReq.derivedMap(beamName).pixelCorrectedImage

    logger.info("Making annotated image of beam " + beamName)
    val annotatedImage = makeAnnotatedImage(correctedImage, attributeList, pointSet)

    logger.info("Making transverse profile of beam " + beamName)
    val transverseProfile = {
      val y = ((translator.height - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(0, y, translator.width, widthOfBand)
      dicomImage.getSubimage(rectangle).columnSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    logger.info("Making axial profile of beam " + beamName)
    val axialProfile = {
      val x = ((translator.width - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(x, 0, widthOfBand, translator.height)
      dicomImage.getSubimage(rectangle).rowSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val transverse_pct = (0 until translator.width).map(x => translator.pix2Iso(x, 0).getX)
    val axial_pct = (0 until translator.height).map(y => translator.pix2Iso(0, y).getY)

    logger.info("Getting baseline values for beam " + beamName)
    //val machinePK = extendedData.machine.machinePK.get
    val timestamp = dataDate
    val axialSymmetryBaseline = getBaseline(machinePK, beamName, axialSymmetryName, attributeList, pointSet.axialSymmetry, timestamp)
    val transverseSymmetryBaseline = getBaseline(machinePK, beamName, transverseSymmetryName, attributeList, transverseSymmetry, timestamp)
    val flatnessBaseline = getBaseline(machinePK, beamName, flatnessName, attributeList, flatness, timestamp)
    val topBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointTop.name, attributeList, pointSet.top, timestamp)
    val bottomBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointBottom.name, attributeList, pointSet.bottom, timestamp)
    val leftBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointLeft.name, attributeList, pointSet.left, timestamp)
    val rightBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointRight.name, attributeList, pointSet.right, timestamp)
    val centerBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointCenter.name, attributeList, pointSet.center, timestamp)

    val baselinePointSet = PointSet(
      topBaseline.baseline.value.toDouble,
      bottomBaseline.baseline.value.toDouble,
      leftBaseline.baseline.value.toDouble,
      rightBaseline.baseline.value.toDouble,
      centerBaseline.baseline.value.toDouble)

    val profileConstancy = pointSet.profileConstancy(baselinePointSet)
    logger.info("Profile constancy for beam " + beamName + " : " + profileConstancy)
    val profileConstancyBaseline = getBaseline(machinePK, beamName, profileConstancyName, attributeList, profileConstancy, timestamp)

    //  val topBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointTop.name, attributeList, pointSet)

    def checkPercent(value: Double, baseline: Double, limit: Double) = {
      val diff = value - baseline
      if (limit >= diff.abs) ProcedureStatus.pass else ProcedureStatus.fail
    }

    logger.info("Checking percentages for beam " + beamName)
    val axialSymmetryStatus = checkPercent(axialSymmetry, axialSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val transverseSymmetryStatus = checkPercent(transverseSymmetry, transverseSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val flatnessStatus = checkPercent(flatness, flatnessBaseline.baseline.value.toDouble, Config.FlatnessPercentLimit)
    val profileConstancyStatus = checkPercent(profileConstancy, profileConstancyBaseline.baseline.value.toDouble, Config.ProfileConstancyPercentLimit)

    val maintenanceRecordBaselineList = Seq(axialSymmetryBaseline, transverseSymmetryBaseline, flatnessBaseline, profileConstancyBaseline,
      topBaseline, bottomBaseline, leftBaseline, rightBaseline, centerBaseline)

    logger.info("Assembling result for beam " + beamName)
    val result = SymmetryAndFlatnessBeamResult(beamName, Util.sopOfAl(attributeList), pointSet,
      axialSymmetry,
      axialSymmetryBaseline.baseline.value.toDouble,
      axialSymmetryStatus,
      transverseSymmetry,
      transverseSymmetryBaseline.baseline.value.toDouble,
      transverseSymmetryStatus,
      flatness,
      flatnessBaseline.baseline.value.toDouble,
      flatnessStatus,
      profileConstancy,
      profileConstancyBaseline.baseline.value.toDouble,
      profileConstancyStatus,
      annotatedImage,
      transverseProfile, transverse_pct,
      axialProfile,
      axial_pct,
      baselinePointSet)

    val base2 = getBaseline2(machinePK, beamName, dataDate)
    val j = base2 match {
      case Some(b2) => {
        if (
          (b2.axialSymmetryBaseline_pct == axialSymmetryBaseline.baseline.value.toDouble) &&
            (b2.transverseSymmetry_pct == transverseSymmetryBaseline.baseline.value.toDouble) &&
            (b2.flatnessBaseline_pct == flatnessBaseline.baseline.value.toDouble) &&
            (b2.profileConstancy_pct == profileConstancyBaseline.baseline.value.toDouble)
        )
          Trace.trace("yay")
        else {}
        Trace.trace("What?")

        Trace.trace(b2.axialSymmetryBaseline_pct == axialSymmetryBaseline.baseline.value.toDouble)
        Trace.trace(b2.transverseSymmetry_pct == transverseSymmetryBaseline.baseline.value.toDouble)
        Trace.trace(b2.flatnessBaseline_pct == flatnessBaseline.baseline.value.toDouble)
        Trace.trace(b2.profileConstancy_pct == profileConstancyBaseline.baseline.value.toDouble)

        Trace.trace(b2.axialSymmetryBaseline_pct + "\n" + axialSymmetryBaseline.baseline.value.toDouble + "\n")
        Trace.trace(b2.transverseSymmetry_pct + "\n" + transverseSymmetryBaseline.baseline.value.toDouble + "\n")
        Trace.trace(b2.flatnessBaseline_pct + "\n" + flatnessBaseline.baseline.value.toDouble + "\n")
        Trace.trace(b2.profileConstancy_pct + "\n" + profileConstancyBaseline.baseline.value.toDouble + "\n")
      }
      case _ => ;
    }
    Trace.trace(j)

    logger.info("Finished analysis of beam " + beamName)

    BeamResultBaseline(result, maintenanceRecordBaselineList, pointSet)
  }

  /**
   * Entry point for testing only.
   *
   * @param beamName            Name of beam.
   * @param machinePK           Machine being processed.
   * @param dataDate            Date that data was acquired at the machine.
   * @param attributeList       Image and metadata.
   * @param correctedImage      Image with bad pixels fixed.
   * @param collimatorCentering Collimator center offset.
   * @return
   */
  def testAnalyze(
                   beamName: String,
                   machinePK: Long,
                   dataDate: Timestamp,
                   attributeList: AttributeList,
                   correctedImage: DicomImage,
                   collimatorCentering: CollimatorCentering): BeamResultBaseline = {
    analyze(beamName, machinePK, dataDate, attributeList, correctedImage, collimatorCentering)
  }

  private def storeResultsInDb(resultList: List[SymmetryAndFlatnessAnalysis.BeamResultBaseline], outputPK: Long): Unit = {

    def toSymmetryAndFlatnessDB(sf: SymmetryAndFlatnessBeamResult): SymmetryAndFlatness = {

      new SymmetryAndFlatness(
        None,
        outputPK,
        sf.SOPInstanceUID,
        sf.beamName,
        isBaseline_text = false.toString,

        sf.axialSymmetry,
        sf.axialSymmetryBaseline,
        sf.axialSymmetryStatus.toString,

        sf.transverseSymmetry,
        sf.transverseSymmetryBaseline,
        sf.transverseSymmetryStatus.toString,

        sf.flatness,
        sf.flatnessBaseline,
        sf.flatnessStatus.toString,

        sf.profileConstancy,
        sf.profileConstancyBaseline,
        sf.profileConstancyStatus.toString,

        sf.pointSet.top,
        sf.pointSet.bottom,
        sf.pointSet.right,
        sf.pointSet.left,
        sf.pointSet.center)
    }

    val list = resultList.map(r => toSymmetryAndFlatnessDB(r.result))
    SymmetryAndFlatness.insert(list)
    logger.info("Stored " + list.size + " SymmetryAndFlatness records")
  }

  private def storeMaintenanceRecordInDB(resultList: List[BeamResultBaseline], machinePK: Long, userPK: Long, outputPK: Long, analysisTime: Timestamp): Unit = {

    // make list of baselines that need to be saved
    val baselineList = resultList.flatMap(r => r.maintenanceRecordBaseline).filter(p => p.maintenanceRecord.isEmpty).map(p => p.baseline)

    if (baselineList.nonEmpty) {
      logger.info("Creating MaintenanceRecord record for Symmetry and Flatness")
      val summary = "Automatically created baseline values for Symmetry and Flatness."
      val preamble = "Symmetry and Flatness baseline values are created automatically if they have not been established for the given machine.  The following is a list of the values:\n\n"
      val valueText = baselineList.map(bl => "    " + bl.id + " : " + bl.value).mkString("\n")

      val maintenanceRecord = new MaintenanceRecord(None, MaintenanceCategory.setBaseline, machinePK, analysisTime, userPK, Some(outputPK), summary, preamble + valueText)
      val insertedMaintenanceRecord = maintenanceRecord.insert
      val newMaintenanceRecordPK = insertedMaintenanceRecord.maintenanceRecordPK.get
      logger.info("Created MaintenanceRecord record for Symmetry and Flatness: " + insertedMaintenanceRecord)

      Baseline.insert(baselineList.map(bl => bl.copy(maintenanceRecordPK = newMaintenanceRecordPK)))
      logger.info("Created " + baselineList.size + " new baseline records for Symmetry and Flatness")
    }
  }

  val subProcedureName = "SymmetryAndFlatness"

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return right for proper execution or left for crash.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness for machine " + extendedData.machine.id)

      val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))

      // only process beams that are both configured and have been uploaded
      val resultList = beamNameList.par.map(beamName =>
        analyze(beamName, extendedData.machine.machinePK.get, extendedData.output.dataDate.get,
          attributeList = getAttributeList(beamName, runReq),
          correctedImage = runReq.derivedMap(beamName).pixelCorrectedImage,
          collimatorCentering)).toList

      val pass = {
        0 match {
          case _ if resultList.size == 1 => resultList.head.result.status.toString.equals(ProcedureStatus.pass.toString)
          case _ if resultList.isEmpty => true
          case _ => resultList.map(rb => rb.result.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
        }
      }
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      storeMaintenanceRecordInDB(resultList, extendedData.machine.machinePK.get, extendedData.user.userPK.get, extendedData.output.outputPK.get, extendedData.output.startDate)
      storeResultsInDb(resultList, extendedData.output.outputPK.get)

      val summary = SymmetryAndFlatnessHTML.makeDisplay(extendedData, resultList, boolToStatus(pass), runReq)

      val result = new SymmetryAndFlatnessResult(summary, status)
      logger.info("Finished analysis of SymmetryAndFlatness for machine " + extendedData.machine.id)
      if (pass) Right(result) else Left(result.summary)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }
}
