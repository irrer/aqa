package org.aqa.webrun.phase2

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
import org.aqa.db.PMI
import org.aqa.db.SymmetryAndFlatness
import java.sql.Timestamp

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

  private def boolToStatus(pass: Boolean) = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

  private val axialSymmetryName = "Axial Symmetry"
  private val transverseSymmetryName = "Transverse Symmetry"
  private val flatnessName = "Flatness"

  /**
   * Encapsulate data for generating a report.
   */
  case class SymmetryAndFlatnessBeamResult(
    beamName: String,
    SOPInstanceUID: String,
    pointMap: Map[SymmetryAndFlatnessPoint, Double],
    axialSymmetry: Double,
    axialSymmetryBaseline: Double,
    transverseSymmetry: Double,
    transverseSymmetryBaseline: Double,
    flatness: Double,
    flatnessBaseline: Double,
    axialSymmetryStatus: ProcedureStatus.Value,
    transverseSymmetryStatus: ProcedureStatus.Value,
    flatnessStatus: ProcedureStatus.Value,
    annotatedImage: BufferedImage,
    transverseProfile: Seq[Double], transverse_mm: IndexedSeq[Double],
    axialProfile: Seq[Double], axial_mm: IndexedSeq[Double]) {

    /** True if everything is ok. */
    val pass = Seq(axialSymmetryStatus, transverseSymmetryStatus, flatnessStatus).filter(s => !(s.toString.equals(ProcedureStatus.pass.toString))).isEmpty
    Trace.trace("pass: " + pass)

    /** Aggregate status. */
    val status = boolToStatus(pass)
    Trace.trace("status: " + status)
  }

  private def makeAnnotatedImage(dicomImage: DicomImage, attributeList: AttributeList, pointMap: Map[SymmetryAndFlatnessPoint, Double]): BufferedImage = {
    val image = dicomImage.toDeepColorBufferedImage
    val graphics = ImageUtil.getGraphics(image)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = translator.circleRadiusInPixels
    val circleSize = (radius * 2).round.toInt

    //    addGraticules(img, translator)
    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt
    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt
    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt
    def pix2Y(yPix: Double) = translator.pix2Iso(0, yPix).getY.round.toInt
    Util.addGraticules(image, x2Pix _, y2Pix _, pix2X _, pix2Y _, Color.gray)

    def dbl2Text(d: Double): String = if (d.round.toInt == d) d.toInt.toString else d.toString

    def annotatePoint(point: SymmetryAndFlatnessPoint) = {
      graphics.setColor(Color.black)
      val value = pointMap(point)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      val description = point.name + " " + dbl2Text(point.x_mm) + ", " + dbl2Text(point.y_mm)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY - radius, description, 90)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY + radius, value.formatted("%6.4f"), 270)
    }

    pointMap.keys.map(p => annotatePoint(p))

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
    else runReq.derivedMap(beamName).dicomFile.attributeList.get
  }

  private def analyzeSymmetry(maxPoint: SymmetryAndFlatnessPoint, minPoint: SymmetryAndFlatnessPoint, pointMap: Map[SymmetryAndFlatnessPoint, Double]): Double = {
    val max = pointMap(maxPoint)
    val min = pointMap(minPoint)
    (max - min) / min
  }

  /**
   * Get the flatness
   */
  private def analyzeFlatness(pointMap: Map[SymmetryAndFlatnessPoint, Double]): Double = {
    val min = pointMap.values.min
    val max = pointMap.values.max
    val flatness = (max - min) / (max + min)
    flatness
  }

  /**
   * For each point (circle) on the image, make a list of pixels that are included in it.
   */
  private def makePointMap(dicomImage: DicomImage, attributeList: AttributeList, RescaleSlope: Double, RescaleIntercept: Double) = {
    val pixMap = SymmetryAndFlatnessAnalysisPixelMap.getPixelMap(attributeList)

    /**
     * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
     */
    def evalPoint(point: SymmetryAndFlatnessPoint): Double = {
      val pixList = pixMap(point)
      val avg = pixList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pixList.size
      (avg * RescaleSlope) + RescaleIntercept // convert
    }
    pixMap.keys.toSeq.map(p => (p, evalPoint(p))).toMap
  }

  def makeBaselineName(beamName: String, dataName: String): String = beamName + " " + dataName

  case class PMIBaseline(pmi: Option[PMI], baseline: Baseline);

  case class BeamResultBaseline(result: SymmetryAndFlatnessBeamResult, pmiBaseline: Seq[PMIBaseline]);

  private def getBaseline(machinePK: Long, beamName: String, dataName: String, attributeList: AttributeList, value: Double): PMIBaseline = {
    val id = makeBaselineName(beamName, dataName)
    Baseline.findLatest(machinePK, id) match {
      case Some((pmi, baseline)) => new PMIBaseline(Some(pmi), baseline)
      case _ => new PMIBaseline(None, Baseline.makeBaseline(-1, attributeList, id, value))
    }
  }

  /**
   * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
   * the database and generating a report.
   *
   */
  private def analyze(beamName: String, extendedData: ExtendedData, runReq: RunReq): BeamResultBaseline = {
    val image = getDicomImage(beamName, runReq)
    val attributeList: AttributeList = getAttributeList(beamName, runReq)
    val dicomImage = new DicomImage(attributeList)
    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head
    val translator = new IsoImagePlaneTranslator(attributeList)
    val widthOfBand = translator.circleRadiusInPixels.round.toInt

    val pointMap = makePointMap(dicomImage, attributeList, RescaleSlope, RescaleIntercept)

    val axialSymmetry = analyzeSymmetry(Config.SymmetryPointTop, Config.SymmetryPointBottom, pointMap)
    val transverseSymmetry = analyzeSymmetry(Config.SymmetryPointRight, Config.SymmetryPointLeft, pointMap)

    val flatness = analyzeFlatness(pointMap)

    val annotatedImage = makeAnnotatedImage(dicomImage, attributeList, pointMap)

    val transverseProfile = {
      val y = ((translator.height - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(0, y, translator.width, widthOfBand)
      dicomImage.getSubimage(rectangle).columnSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val axialProfile = {
      val x = ((translator.width - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(x, 0, widthOfBand, translator.height)
      dicomImage.getSubimage(rectangle).rowSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val transverse_mm = (0 until translator.width).map(x => translator.pix2Iso(x, 0).getX)
    val axial_mm = (0 until translator.height).map(y => translator.pix2Iso(0, y).getY)

    val machinePK = extendedData.machine.machinePK.get
    val axialSymmetryBaseline = getBaseline(machinePK, beamName, axialSymmetryName, attributeList, axialSymmetry)
    val transverseSymmetryBaseline = getBaseline(machinePK, beamName, transverseSymmetryName, attributeList, transverseSymmetry)
    val flatnessBaseline = getBaseline(machinePK, beamName, flatnessName, attributeList, flatness)

    def checkPercent(value: Double, baseline: Double, limit: Double) = {
      val percent = (100 * (value - baseline)) / baseline
      if (limit >= (percent.abs)) ProcedureStatus.pass else ProcedureStatus.fail
    }

    val axialSymmetryStatus = checkPercent(axialSymmetry, axialSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val transverseSymmetryStatus = checkPercent(transverseSymmetry, transverseSymmetryBaseline.baseline.value.toDouble, Config.SymmetryPercentLimit)
    val flatnessStatus = checkPercent(flatness, flatnessBaseline.baseline.value.toDouble, Config.FlatnessPercentLimit)

    val pmiBaselineList = Seq(axialSymmetryBaseline, transverseSymmetryBaseline, flatnessBaseline)

    val result = new SymmetryAndFlatnessBeamResult(beamName, Util.sopOfAl(attributeList), pointMap,
      axialSymmetry,
      axialSymmetryBaseline.baseline.value.toDouble,
      transverseSymmetry,
      transverseSymmetryBaseline.baseline.value.toDouble,
      flatness,
      flatnessBaseline.baseline.value.toDouble,
      axialSymmetryStatus,
      transverseSymmetryStatus,
      flatnessStatus,
      annotatedImage,
      transverseProfile, transverse_mm,
      axialProfile,
      axial_mm)

    new BeamResultBaseline(result, pmiBaselineList)
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
        sf.flatnessStatus.toString)
    }

    val list = resultList.map(r => toSymmetryAndFlatnessDB(r.result))
    SymmetryAndFlatness.insert(list)
    logger.info("Stored " + list.size + " SymmetryAndFlatness records")
  }

  private def storePmiInDB(resultList: List[BeamResultBaseline], machinePK: Long, userPK: Long, outputPK: Long): Unit = {

    // make list of baselines that need to be saved
    val baselineList = resultList.map(r => r.pmiBaseline).flatten.filter(p => p.pmi.isEmpty).map(p => p.baseline)

    if (baselineList.nonEmpty) {
      logger.info("Creating PMI record for Symmetry and Flatness")
      val summary = "Automatically created baseline values for Symmetry and Flatness."
      val preamble = "Symmetry and Flatness baseline values are created automatically if they have not been established for the given machine.  The following is a list of the values:\n\n"
      val valueText = baselineList.map(bl => bl.id + " : " + bl.value).mkString("\n")

      val creationTime = new Timestamp(System.currentTimeMillis)
      val pmi = new PMI(None, machinePK, creationTime, userPK, Some(outputPK), summary, preamble + valueText)
      val insertedPmi = pmi.insert
      val newPmiPK = insertedPmi.pmiPK.get
      logger.info("Created PMI record for Symmetry and Flatness: " + insertedPmi)

      Baseline.insert(baselineList.map(bl => bl.copy(pmiPK = newPmiPK)))
      logger.info("Created " + baselineList.size + " new baseline records for Symmetry and Flatness")
    }
  }

  val subProcedureName = "SymmetryAndFlatness"

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return right for proper execution or left for crash.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness")

      val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))

      // only process beams that are both configured and have been uploaded
      //val resultList = beamNameList.par.map(beamName => analyze(beamName, extendedData, runReq)).toList
      val resultList = beamNameList.map(beamName => analyze(beamName, extendedData, runReq)).toList // change back to 'par' when debugged

      val pass = resultList.map(rb => rb.result.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      storePmiInDB(resultList, extendedData.machine.machinePK.get, extendedData.user.userPK.get, extendedData.output.outputPK.get)
      storeResultsInDb(resultList, extendedData.output.outputPK.get)

      val summary = SymmetryAndFlatnessHTML.makeDisplay(extendedData, resultList, boolToStatus(pass), runReq)

      Right(new SymmetryAndFlatnessResult(summary, status))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
