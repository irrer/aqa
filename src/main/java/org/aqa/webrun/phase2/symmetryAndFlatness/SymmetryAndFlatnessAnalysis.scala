package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Logging
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageText
import java.awt.BasicStroke
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.MaintenanceRecord
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.SymmetryAndFlatness.PointSet
import java.sql.Timestamp
import org.aqa.db.MaintenanceCategory
import org.aqa.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.Phase2Util.MaintenanceRecordBaseline
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.CollimatorCentering
import java.awt.geom.Point2D

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
    val pass = Seq(axialSymmetryStatus, transverseSymmetryStatus, flatnessStatus).filter(s => !(s.toString.equals(ProcedureStatus.pass.toString))).isEmpty
    logger.info("sym+flatness pass: " + pass)

    /** Aggregate status. */
    val status = boolToStatus(pass)
    logger.info("sym+flatness aggregate status: " + status)
  }

  private def makeAnnotatedImage(correctedImage: DicomImage, attributeList: AttributeList, pointSet: PointSet): BufferedImage = {
    val image = correctedImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(image)
    val graphics = ImageUtil.getGraphics(image)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = translator.circleRadiusInPixels
    val circleSize = (radius * 2).round.toInt

    // addGraticules(img, translator)
    Util.addGraticules(image, translator, Color.gray)

    Util.addAxialAndTransverse(image)

    def dbl2Text(d: Double): String = if (d.round.toInt == d) d.toInt.toString else d.toString

    def annotatePoint(point: SymmetryAndFlatnessPoint, value: Double) = {
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

  private def getDicomImage(beamName: String, runReq: RunReq): DicomImage = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood) runReq.floodCorrectedImage
    else runReq.derivedMap(beamName).pixelCorrectedImage
  }

  private def getAttributeList(beamName: String, runReq: RunReq): AttributeList = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood) runReq.flood.attributeList.get
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

    new PointSet(evalPoint(Config.SymmetryPointTop), evalPoint(Config.SymmetryPointBottom),
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

  case class BeamResultBaseline(result: SymmetryAndFlatnessBeamResult, maintenanceRecordBaseline: Seq[MaintenanceRecordBaseline], pointSet: PointSet);

  /**
   * Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
   */
  private def getBaseline(machinePK: Long, beamName: String, dataName: String, attributeList: AttributeList, value: Double, dataDate: Timestamp): MaintenanceRecordBaseline = {
    val id = makeBaselineName(beamName, dataName)
    Baseline.findLatest(machinePK, id, dataDate) match {
      case Some((maintenanceRecord, baseline)) => new MaintenanceRecordBaseline(Some(maintenanceRecord), baseline)
      case _ => new MaintenanceRecordBaseline(None, Baseline.makeBaseline(-1, dataDate, Util.sopOfAl(attributeList), id, value))
    }
  }

  /**
   * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
   * the database and generating a report.
   *
   */
  private def analyze(beamName: String, extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): BeamResultBaseline = {
    logger.info("Begin analysis of beam " + beamName)
    val image = getDicomImage(beamName, runReq)
    val attributeList: AttributeList = getAttributeList(beamName, runReq)
    val dicomImage = new DicomImage(attributeList)
    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head
    val translator = new IsoImagePlaneTranslator(attributeList)
    val widthOfBand = translator.circleRadiusInPixels.round.toInt

    val pointSet = makePointSet(dicomImage, attributeList, RescaleSlope, RescaleIntercept, collimatorCentering.center)

    val axialSymmetry = pointSet.axialSymmetry
    logger.info("Axial symmetry of beam " + beamName + " : " + axialSymmetry)
    val transverseSymmetry = pointSet.transverseSymmetry
    logger.info("ransverse symmetry of beam " + beamName + " : " + transverseSymmetry)

    val flatness = pointSet.flatness
    logger.info("Flatness of beam " + beamName + " : " + flatness)

    logger.info("Making corrected image of beam " + beamName)
    val correctedImage = runReq.rtimageMap(beamName).correctedDicomImage.get
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
    val machinePK = extendedData.machine.machinePK.get
    val timestamp = extendedData.output.dataDate.get
    val axialSymmetryBaseline = getBaseline(machinePK, beamName, axialSymmetryName, attributeList, pointSet.axialSymmetry, timestamp)
    val transverseSymmetryBaseline = getBaseline(machinePK, beamName, transverseSymmetryName, attributeList, transverseSymmetry, timestamp)
    val flatnessBaseline = getBaseline(machinePK, beamName, flatnessName, attributeList, flatness, timestamp)
    val topBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointTop.name, attributeList, pointSet.top, timestamp)
    val bottomBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointBottom.name, attributeList, pointSet.bottom, timestamp)
    val leftBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointLeft.name, attributeList, pointSet.left, timestamp)
    val rightBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointRight.name, attributeList, pointSet.right, timestamp)
    val centerBaseline = getBaseline(machinePK, beamName, Config.SymmetryPointCenter.name, attributeList, pointSet.center, timestamp)

    val baselinePointSet = new PointSet(
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
      if (limit >= (diff.abs)) ProcedureStatus.pass else ProcedureStatus.fail
    }

    logger.info("Checking percentages for beam " + beamName)
    val axialSymmetryStatus = checkPercent(axialSymmetry, axialSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val transverseSymmetryStatus = checkPercent(transverseSymmetry, transverseSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val flatnessStatus = checkPercent(flatness, flatnessBaseline.baseline.value.toDouble, Config.FlatnessPercentLimit)
    val profileConstancyStatus = checkPercent(profileConstancy, profileConstancyBaseline.baseline.value.toDouble, Config.ProfileConstancyPercentLimit)

    val maintenanceRecordBaselineList = Seq(axialSymmetryBaseline, transverseSymmetryBaseline, flatnessBaseline, profileConstancyBaseline,
      topBaseline, bottomBaseline, leftBaseline, rightBaseline, centerBaseline)

    logger.info("Assembling result for beam " + beamName)
    val result = new SymmetryAndFlatnessBeamResult(beamName, Util.sopOfAl(attributeList), pointSet,
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

    logger.info("Finished analysis of beam " + beamName)

    new BeamResultBaseline(result, maintenanceRecordBaselineList, pointSet)
  }

  private def storeResultsInDb(resultList: List[SymmetryAndFlatnessAnalysis.BeamResultBaseline], outputPK: Long): Unit = {

    def toSymmetryAndFlatnessDB(sf: SymmetryAndFlatnessBeamResult): SymmetryAndFlatness = {

      new SymmetryAndFlatness(
        None,
        outputPK,
        sf.SOPInstanceUID,
        sf.beamName,

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
    val baselineList = resultList.map(r => r.maintenanceRecordBaseline).flatten.filter(p => p.maintenanceRecord.isEmpty).map(p => p.baseline)

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
      val resultList = beamNameList.par.map(beamName => analyze(beamName, extendedData, runReq, collimatorCentering)).toList

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
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
