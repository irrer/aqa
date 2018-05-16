package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.DicomFile
//import org.aqa.db.CollimatorCentering
//import com.pixelmed.dicom.AttributeTag
//import com.pixelmed.dicom.AttributeList
//import com.pixelmed.dicom.TagFromName
//import org.aqa.Util
//import scala.collection.Seq
//import scala.xml.Elem
//import org.aqa.db.Output
//import org.aqa.run.ProcedureStatus
//import edu.umro.ImageUtil.DicomImage
//import edu.umro.ImageUtil.ImageUtil
//import java.awt.geom.Point2D
//import org.aqa.Config
//import java.awt.Rectangle
//import edu.umro.ImageUtil.LocateEdge
//import java.awt.image.BufferedImage
//import java.awt.Color
//import java.awt.Graphics2D
//import java.awt.BasicStroke

/**
 * Represent a flood field.
 */
class FloodField(dicomFile: DicomFile) extends Logging {

  val edges = CollimatorCenteringAnalysis.fineMeasure(dicomFile)

  def make = 5
}
