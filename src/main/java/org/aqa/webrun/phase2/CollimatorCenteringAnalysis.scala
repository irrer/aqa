package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorCentering
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

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorCenteringAnalysis extends Logging {

  /**
   * Get the center of the image (regardless of collimator) in mm.
   */
  private def getImageCenter_mm(al: AttributeList): Point2D.Double = {
    val ImagePlanePixelSpacing = al.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    val Rows = al.get(TagFromName.Rows).getIntegerValues.head
    val Columns = al.get(TagFromName.Columns).getIntegerValues.head
    new Point2D.Double(Columns * ImagePlanePixelSpacing(0), Rows * ImagePlanePixelSpacing(1))
  }

  /**
   * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): (ProcedureStatus.Value, Elem) = {

    val img090 = runReq.derivedMap(Config.CollimatorCentering090BeamName)
    val img270 = runReq.derivedMap(Config.CollimatorCentering270BeamName)
    val result090 = MeasureNSEWEdges.measure(img090.biasAndPixelCorrectedCroppedImage, runReq.ImagePlanePixelSpacing, img090.originalImage, runReq.floodOffset)
    val result270 = MeasureNSEWEdges.measure(img270.biasAndPixelCorrectedCroppedImage, runReq.ImagePlanePixelSpacing, img270.originalImage, runReq.floodOffset)

    val m090 = result090.measurementSet
    val m270 = result270.measurementSet
    val xCntr = (m090.center.getX + m270.center.getX) / 2
    val yCntr = (m090.center.getY + m270.center.getY) / 2
    val imgCntr = getImageCenter_mm(runReq.rtimageMap(Config.CollimatorCentering090BeamName).attributeList.get)

    val errDistance = (new Point2D.Double(xCntr, yCntr)).distance(imgCntr.getX, imgCntr.getY)
    val pass: Boolean = errDistance <= Config.CollimatorCenteringTolerence_mm
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    logger.info("CollimatorCentering error in mm: " + errDistance + "    Status: " + procedureStatus)

    val collimatorCentering = new CollimatorCentering(None, extendedData.output.outputPK.get, procedureStatus.name,
      xCntr - imgCntr.getX, yCntr - imgCntr.getY,
      xCntr, yCntr,
      m090.north, m090.south, m090.east, m090.west,
      m270.north, m270.south, m270.east, m270.west)
    logger.info("Inserting CollimatorCentering row: " + collimatorCentering)
    collimatorCentering.insert

    val elem = CollimatorCenteringHTML.makeDisplay(extendedData, result090, result270, runReq)
    (procedureStatus, elem)
  }
}
