package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point
import edu.umro.ImageUtil.ImageText
import java.io.File

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

  private def makeAnnotatedImage(dicomImage: DicomImage, attributeList: AttributeList, resultMap: Map[SymmetryAndFlatnessPoint, Double]): BufferedImage = {
    val img = dicomImage.toBufferedImage(Color.cyan)
    val graphics = ImageUtil.getGraphics(img)
    graphics.setColor(Color.black)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = translator.circleRadiusInPixels
    val circleSize = (radius * 2).round.toInt

    def annotatePoint(point: SymmetryAndFlatnessPoint) = {
      val value = resultMap(point)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      val text = value.formatted("%12.6f") + " : " + point.name
      ImageText.drawTextCenteredAt(graphics, center.getX, center.getY, text)
    }

    resultMap.keys.map(p => annotatePoint(p))

    img
  }

  /**
   * Find the average pixel value for each of the spots (of configured diameter) for the given beam.
   */
  private def analyzePoints(beamName: String, extendedData: ExtendedData, runReq: RunReq): (Map[SymmetryAndFlatnessPoint, Double], BufferedImage) = {

    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)

    val image = {
      if (isFlood) runReq.floodCorrectedImage
      else runReq.derivedMap(beamName).pixelCorrectedImage
    }

    val al: AttributeList = {
      if (isFlood) runReq.flood.attributeList.get
      else runReq.derivedMap(beamName).dicomFile.attributeList.get
    }

    val pixMap = SymmetryAndFlatnessAnalysisPixelMap.getPixelMap(al)

    val dicomImage = new DicomImage(al)

    val RescaleSlope = al.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = al.get(TagFromName.RescaleIntercept).getDoubleValues.head

    /**
     * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
     */
    def evalPoint(point: SymmetryAndFlatnessPoint): Double = {
      val pixList = pixMap(point)
      val avg = pixList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pixList.size
      (avg * RescaleSlope) + RescaleIntercept // convert
    }

    val resultMap = pixMap.keys.toSeq.map(p => (p, evalPoint(p))).toMap

    val annotatedImage = makeAnnotatedImage(dicomImage, al, resultMap)

    (resultMap, annotatedImage)
  }

  private def analyzeSymmetry(beamName: String, pointValueAndImageMap: (Map[SymmetryAndFlatnessPoint, Double], BufferedImage), extendedData: ExtendedData, runReq: RunReq): Double = {

    val pointValueMap = pointValueAndImageMap._1
    val image = pointValueAndImageMap._2 // TODO annotate image
    def diffOf(a: SymmetryAndFlatnessPoint, b: SymmetryAndFlatnessPoint) = pointValueMap(a) - pointValueMap(b)

    def showPair(a: SymmetryAndFlatnessPoint, b: SymmetryAndFlatnessPoint) = {
      println("Symmetry: " + a.name + " to " + b.name + "  ==>  " + diffOf(a, b))
    }

    Config.SymmetryAndFlatnessPointPairList.map(pair => showPair(pair._1, pair._2))

    Config.SymmetryAndFlatnessPointPairList.map(pair => diffOf(pair._1, pair._2)).max
  }

  private def analyzeFlatness(beamName: String, pointValueAndImageMap: (Map[SymmetryAndFlatnessPoint, Double], BufferedImage), extendedData: ExtendedData, runReq: RunReq): Double = {
    val min = pointValueAndImageMap._1.values.min
    val max = pointValueAndImageMap._1.values.max
    val flatness = (max - min) / (max + min)
    println("analyzeFlatness: " + flatness) // TODO draw image
    flatness
  }

  val subProcedureName = "SymmetryAndFlatness";

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness")

      val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))
      val beamPointValueAndImageMap = beamNameList.par.map(beamName => (beamName, analyzePoints(beamName, extendedData, runReq))).toList.toMap

      val symmetryList = beamNameList.map(beamName => analyzeSymmetry(beamName, beamPointValueAndImageMap(beamName), extendedData, runReq)).toList
      val flatnessList = beamNameList.map(beamName => analyzeFlatness(beamName, beamPointValueAndImageMap(beamName), extendedData, runReq)).toList

      def saveImage(beamName: String, image: BufferedImage) = {
        val fileName = "Sym_Flat_" + beamName.replace(' ', '_') + ".png"
        val pngFile = new File(extendedData.output.dir, fileName)
        Util.writePng(image, pngFile)
      }

      beamPointValueAndImageMap.keys.map(beamName => saveImage(beamName, beamPointValueAndImageMap(beamName)._2))

      val summary = {
        <div>SymmetryAndFlatness</div>
      }

      Right(new SymmetryAndFlatnessResult(summary, ProcedureStatus.done))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
