/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Procedure
import org.aqa.db.PSM
import org.aqa.db.SymmetryAndFlatness
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.psm.PSMUtil

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
      symmetryAndFlatness: SymmetryAndFlatness,
      annotatedImage: BufferedImage,
      transverseProfile: Seq[Double],
      transverse_pct: IndexedSeq[Double],
      axialProfile: Seq[Double],
      axial_pct: IndexedSeq[Double],
      baseline: SymmetryAndFlatness
  ) {

    /** True if everything is ok. */
    /*
    val pass: Boolean = Seq(axialSymmetryStatus, transverseSymmetryStatus, flatnessStatus).forall(s => s.toString.equals(ProcedureStatus.pass.toString))
    logger.info("sym+flatness pass: " + pass)
     */

    /** Aggregate status. */
    /*
    val status: ProcedureStatus.ProcedureStatus = boolToStatus(pass)
    logger.info("sym+flatness aggregate status: " + status)
     */
  }

  private def circleRadiusInPixels(isoImageTrans: IsoImagePlaneTranslator): Double = {
    val radius_mm = Config.SymmetryAndFlatnessDiameter_mm / 2
    val imagePlaneCenterInPixels = isoImageTrans.iso2Pix(0, 0)
    val radiusInPixels = isoImageTrans.iso2Pix(radius_mm, radius_mm).distance(imagePlaneCenterInPixels)
    radiusInPixels
  }

  private def makeAnnotatedImage(correctedImage: DicomImage, attributeList: AttributeList, symmetryAndFlatness: SymmetryAndFlatness): BufferedImage = {
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

    annotatePoint(Config.SymmetryPointTop, symmetryAndFlatness.top_cu)
    annotatePoint(Config.SymmetryPointBottom, symmetryAndFlatness.bottom_cu)
    annotatePoint(Config.SymmetryPointRight, symmetryAndFlatness.right_cu)
    annotatePoint(Config.SymmetryPointLeft, symmetryAndFlatness.left_cu)
    annotatePoint(Config.SymmetryPointCenter, symmetryAndFlatness.center_cu)

    image
  }

  private def getAttributeList(beamName: String, runReq: RunReq): AttributeList = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood && runReq.flood.isDefined) runReq.flood.get
    else runReq.derivedMap(beamName).attributeList
  }

  def makeBaselineName(beamName: String, dataName: String): String = dataName + " " + beamName

  /**
   * Apply
   * @param beamName Name of Beam in RTPLAN
   * @param wd Whole detector - the original beam.
   * @param psm PSM data
   * @return An image corrected for PSM.
   */

  private def psmCorrection(beamName: String, wd: DicomImage, psm: PSM): DicomImage = {
    val psmDicom = psm.dicom
    val psmScaledImage = new DicomImage(psmDicom).scalePixels(psmDicom)

    val ffScaledImage = psm.getFloodFieldScaled

    val wdXff = wd.fun2((a,b) => a*b, ffScaledImage)

    def doRow(y: Int): IndexedSeq[Float] = {
      def doPix(x: Int): Float = {
        val psmVal: Float = {
          psmScaledImage.get(x, y) match {
            case 0 => 1
            case v => v
          }
        }
        (wd.get(x, y) * ffScaledImage.get(x, y)) / psmVal
      }
      (0 until psmScaledImage.width).map(x => doPix(x))
    }



    val pixels = (0 until psmScaledImage.height).map(doRow)

    val beamResponse = new DicomImage(pixels)

    if (true) { // TODO rm
      SymmetryAndFlatnessAnalysis.synchronized {
        Trace.trace(s"\n===== Beam: $beamName =====")
        Trace.trace("ff:\n" + PSMUtil.centerPixelsToString(ffScaledImage) + "\n\n")
        Trace.trace("psm:\n" + PSMUtil.centerPixelsToString(psmScaledImage))
        Trace.trace("wd:\n" + PSMUtil.centerPixelsToString(wd))
        Trace.trace("br:\n" + PSMUtil.centerPixelsToString(beamResponse))
        Trace.trace()
      }
    }

    beamResponse
  }

  /**
    * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
    * the database and generating a report.
    *
    */
  private def analyze(
      outputPK: Long,
      procedurePK: Long,
      beamName: String,
      machinePK: Long,
      dataDate: Timestamp,
      attributeList: AttributeList,
      correctedImage: DicomImage,
      collimatorCenter: Point2D.Double,
      symmetryAndFlatnessBaselineRedoBeamList: Seq[String],
      psm: Option[PSM]
  ): SymmetryAndFlatnessBeamResult = {
    logger.info("Begin analysis of beam " + beamName)

    val scaledImage: DicomImage = {
      val scaled = new DicomImage(attributeList).scalePixels(attributeList)

      val image =
        if (psm.isEmpty)
          scaled
        else
          psmCorrection(beamName, scaled, psm.get)

      image
    }

    // val attributeList: AttributeList = getAttributeList(beamName, runReq)
    // val dicomImage = new DicomImage(attributeList)
    val translator = new IsoImagePlaneTranslator(attributeList)
    val widthOfBand_pix: Int = translator.iso2PixDistX(Config.SymmetryAndFlatnessDiameter_mm).round.toInt
    val heightOfBand_pix: Int = translator.iso2PixDistY(Config.SymmetryAndFlatnessDiameter_mm).round.toInt

    /**
      * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
      *
      * @param point: Center of circle in image.
      *
      * @return Mean value of pixels in circle in CU.
      */
    def evalPoint(point: SymmetryAndFlatnessPoint): Double = {
      val center = new Point2D.Double(point.x_mm + collimatorCenter.getX, point.y_mm + collimatorCenter.getY)
      val pixList = Phase2Util.makeCenterDosePointList(attributeList, center)
      val avg = pixList.map(p => scaledImage.get(p.x, p.y)).sum / pixList.size
      avg
    }

    /**
      * Get the standard deviation of the pixel values in CU for one spot in HU or CU or whatever units the image is using.
      *
      * @param point: Center of circle in image.
      *
      * @return Standard deviation of pixels in circle in CU.
      */
    def evalPointStdDev(point: SymmetryAndFlatnessPoint): Double = {
      val center = new Point2D.Double(point.x_mm + collimatorCenter.getX, point.y_mm + collimatorCenter.getY)
      val pixList = Phase2Util.makeCenterDosePointList(attributeList, center)
      val cuList = pixList.map(p => scaledImage.get(p.x, p.y))
      val stdDev_cu = ImageUtil.stdDev(cuList)
      stdDev_cu
    }

    logger.info("Making transverse profile of beam " + beamName)
    val transverseProfile = {
      val y = ((translator.height - heightOfBand_pix) / 2.0).round.toInt
      val rectangle = new Rectangle(0, y, translator.width, heightOfBand_pix)
      val subImage = scaledImage.getSubimage(rectangle)
      val cuList = subImage.columnSums.map(_ / heightOfBand_pix.toDouble)
      cuList
    }

    logger.info("Making axial profile of beam " + beamName)
    val axialProfile = {
      val x = ((translator.width - widthOfBand_pix) / 2.0).round.toInt
      val rectangle = new Rectangle(x, 0, widthOfBand_pix, translator.height)
      val subImage = scaledImage.getSubimage(rectangle)
      val cuList = subImage.rowSums.map(_ / widthOfBand_pix.toDouble)
      cuList
    }

    val transverse_pct = (0 until translator.width).map(x => translator.pix2Iso(x, 0).getX)
    val axial_pct = (0 until translator.height).map(y => translator.pix2Iso(0, y).getY)

    logger.info("Assembling result for beam " + beamName)

    val symmetryAndFlatness = new SymmetryAndFlatness(
      symmetryAndFlatnessPK = None,
      outputPK = outputPK,
      SOPInstanceUID = Util.sopOfAl(attributeList),
      beamName = beamName,
      isBaseline = symmetryAndFlatnessBaselineRedoBeamList.contains(beamName),
      top_cu = evalPoint(Config.SymmetryPointTop),
      bottom_cu = evalPoint(Config.SymmetryPointBottom),
      left_cu = evalPoint(Config.SymmetryPointLeft),
      right_cu = evalPoint(Config.SymmetryPointRight),
      center_cu = evalPoint(Config.SymmetryPointCenter),
      topStdDev_cu = evalPointStdDev(Config.SymmetryPointTop),
      bottomStdDev_cu = evalPointStdDev(Config.SymmetryPointBottom),
      leftStdDev_cu = evalPointStdDev(Config.SymmetryPointLeft),
      rightStdDev_cu = evalPointStdDev(Config.SymmetryPointRight),
      centerStdDev_cu = evalPointStdDev(Config.SymmetryPointCenter),
      psmImageHash_md5 = if (psm.isDefined) Some(psm.get.imageHash_md5) else None
    )

    logger.info("Getting baseline values for beam " + beamName)

    // Get the baseline for the given beam of the given type (dataName).  If it does not exist, then use this one to establish it.
    val baseline = SymmetryAndFlatness.getBaseline(machinePK, beamName, psm.isDefined, dataDate, procedurePK) match {
      case Some(bl) => bl.baseline
      case _        => symmetryAndFlatness
    }

    logger.info("Making annotated image of beam " + beamName)
    val annotatedImage = makeAnnotatedImage(correctedImage, attributeList, symmetryAndFlatness)
    val result = SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult(symmetryAndFlatness, annotatedImage, transverseProfile, transverse_pct, axialProfile, axial_pct, baseline)

    logger.info("Finished analysis of beam " + beamName)

    result
  }

  /**
    * Entry point for testing only.
    *
    * @param beamName         Name of beam.
    * @param machinePK        Machine being processed.
    * @param dataDate         Date that data was acquired at the machine.
    * @param attributeList    Image and metadata.
    * @param correctedImage   Image with bad pixels fixed.
    * @param collimatorCenter Collimator center offset.
    * @return
    */
  def testAnalyze(beamName: String, machinePK: Long, dataDate: Timestamp, attributeList: AttributeList, correctedImage: DicomImage, collimatorCenter: Point2D.Double): SymmetryAndFlatnessBeamResult = {
    analyze(
      outputPK = -1,
      procedurePK = Procedure.ProcOfPhase2.get.procedurePK.get,
      beamName,
      machinePK,
      dataDate,
      attributeList,
      correctedImage,
      collimatorCenter,
      Seq(),
      None
    )
  }

  /**
    * Put the results in the database.
    *
    * @param resultList List of new results.
    */
  private def storeResultsInDb(resultList: List[SymmetryAndFlatnessBeamResult]): Unit = {
    resultList.foreach(r => r.symmetryAndFlatness.insertOrUpdate())
    logger.info("Stored " + resultList.size + " SymmetryAndFlatness records")
  }

  val subProcedureName = "SymmetryAndFlatness"

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
    * Run the CollimatorPosition sub-procedure, save results in the database, return right for proper execution or left for crash.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness for machine " + extendedData.machine.id)

      // val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))
      val beamNameList = Util.makeSymFlatConstBeamNameList(runReq.rtplan).filter(beamName => runReq.derivedMap.contains(beamName))
      logger.info("Sym+Flat using beams:\n    " + beamNameList.mkString("\n    "))

      val psm = runReq.getPsm(extendedData.machine.machinePK.get)

      if (psm.isEmpty)
        logger.info("No PSM available.")
      else
        logger.info("Using PSM.")

      def doBeam(beamName: String): Seq[SymmetryAndFlatnessBeamResult] = {
        val noPsm = Some(
          analyze(
            extendedData.output.outputPK.get,
            extendedData.output.procedurePK,
            beamName = beamName,
            extendedData.machine.machinePK.get,
            extendedData.output.dataDate.get,
            attributeList = getAttributeList(beamName, runReq),
            correctedImage = runReq.derivedMap(beamName).pixelCorrectedImage,
            collimatorCenteringResource.centerOfBeam(beamName),
            runReq.symmetryAndFlatnessBaselineRedoBeamList,
            None
          )
        )

        val withPsm =
          if (psm.isEmpty)
            None
          else
            Some(
              analyze(
                extendedData.output.outputPK.get,
                extendedData.output.procedurePK,
                beamName = beamName,
                extendedData.machine.machinePK.get,
                extendedData.output.dataDate.get,
                attributeList = getAttributeList(beamName, runReq),
                correctedImage = runReq.derivedMap(beamName).pixelCorrectedImage,
                collimatorCenteringResource.centerOfBeam(beamName),
                runReq.symmetryAndFlatnessBaselineRedoBeamList,
                psm
              )
            )

        Seq(noPsm, withPsm).flatten
      }

      // only process beams that are both configured and have been uploaded
      val resultList: List[SymmetryAndFlatnessBeamResult] = beamNameList.par.flatMap(doBeam).toList

      def showIt(r: SymmetryAndFlatnessBeamResult): String = {
        val bs = r.baseline
        val sf = r.symmetryAndFlatness
        val text = {
          "    " + sf.beamName.format("%16s") + " : " +
            "    axial sym:" + sf.axialSymmetryPass(bs).toString.format("%5s") + " : " +
            "    flatness:" + sf.flatnessPass(bs).toString.format("%5s") + " : " +
            "    transverse sym:" + sf.transverseSymmetryPass(bs).toString.format("%5s") + " : " +
            "    profile const:" + sf.profileConstancyPass(bs).toString.format("%5s") + " : " +
            "    all: " + sf.allPass(bs).toString.format("%5s")
        }
        text
      }

      logger.info("\n" + resultList.map(r => showIt(r)).mkString("\n"))

      //val pass = resultList.map(r => r.symmetryAndFlatness.allPass(r.baseline)).reduce(_ && _)
      val pass = {
        val list = resultList.map(r => r.symmetryAndFlatness.allPass(r.baseline))
        list.nonEmpty && list.reduce(_ && _)
      }
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      storeResultsInDb(resultList)

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
