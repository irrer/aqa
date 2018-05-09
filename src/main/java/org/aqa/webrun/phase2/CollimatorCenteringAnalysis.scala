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

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorCenteringAnalysis extends Logging {

  private case class Measurement(location: Double, dicomImage: DicomImage)

  private case class MeasurementSet(north: Double, south: Double, east: Double, west: Double)

  private def doNorth(mainImage: DicomImage, cntrOfMass: Point2D, imageResolution: Point2D) = {
    val width = (Config.CollimatorCenteringCoarseBandWidth / imageResolution.getY).round.toInt
    val upperLeft = (cntrOfMass.getX - (width / 2.0)).round.toInt
    val subImage = mainImage.getSubimage(new Rectangle(upperLeft, 0, width, mainImage.height / 2))
    
    
    
  }

  private def measure(attributeList: AttributeList): MeasurementSet = {
    val mainImage = new DicomImage(attributeList)
    val cntrOfMass = new Point2D.Double(ImageUtil.centerOfMass(mainImage.columnSums), ImageUtil.centerOfMass(mainImage.rowSums))
    val pixelSpacing = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    val imageResolution = new Point2D.Double(pixelSpacing(0), pixelSpacing(1))

    val north = doNorth
    val rectNorth =

      ???
  }

  /**
   * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(output: Output, collimatorCenteringRunRequirements: CollimatorCenteringRunRequirements): (ProcedureStatus.Value, Elem) = {
    val outPK = output.outputPK.get
    val CollimatorCentering.insert(list)
    val pass: Boolean = list.map(ii => ii.pass).reduce(_ && _)
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    val elem = CollimatorCenteringHTML.makeDisplay(output, positioningCheckRunRequirements, procedureStatus)
    (procedureStatus, elem)
  }
}
