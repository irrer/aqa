package org.aqa.webrun.bbByEpid

import scala.xml.Elem
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import javax.vecmath.Point3d
import java.awt.image.BufferedImage
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import com.pixelmed.dicom.AttributeList
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import com.pixelmed.dicom.TagFromName
import org.aqa.IsoImagePlaneTranslator
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateMax
import edu.umro.ScalaUtil.Trace
import java.awt.geom.Point2D

object BBbyEPIDAnalysis extends Logging {

  private val subProcedureName = "BB by EPID"

  private def di(d: Double) = d.round.toInt

  case class BBbyEPIDResult(summry: Elem, sts: ProcedureStatus.Value, position: Seq[Point3d], images: Seq[BufferedImage])

  /**
   * Obtain the sub-area of the image to be searched for the BB.
   */
  private def searchArea(trans: IsoImagePlaneTranslator, center_mm: Point2D.Double, distance_mm: Double): Rectangle = {
    val corner = trans.iso2Pix(center_mm.getX - distance_mm, center_mm.getY - distance_mm)
    val w = trans.iso2PixDistX(distance_mm * 2)
    val h = trans.iso2PixDistY(distance_mm * 2)
    val rect = new Rectangle(di(corner.getX), di(corner.getY), di(w), di(h))
    rect
  }

  /**
   * Find the maximu point in the given search area.
   * 
   * @param al: Contains image.
   * 
   * @param center_mm: Center of search area
   * 
   * @param distance: Distance in all four directions from center to search.
   */
  private def locMax(al: AttributeList, center_mm: Point2D.Double, distance_mm: Double): Option[Point2D.Double] = {
    val image = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    val rect = searchArea(trans, center_mm, Config.BBbyEPIDSearchDistance_mm)
    val subImage = image.getSubimage(rect)

    val max = subImage.getMaxPoint(Config.BBMinimumStandardDeviation) match {
      case Some(max) => Some(new Point2D.Double(max.getX + rect.getX, max.getY + rect.getY))
      case _ => None
    }
    max
  }

  private def findBB(al: AttributeList) = {
    val image = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    // Using a sub-area eliminates the need for having to deal with other objects, such as the couch rails.
    val rect = searchArea(trans, new Point2D.Double(0, 0), Config.BBbyEPIDSearchDistance_mm)
    val subImage = image.getSubimage(rect)
    val xPos_pix = rect.getX + LocateMax.locateMax(subImage.columnSums)
    val yPos_pix = rect.getY + LocateMax.locateMax(subImage.rowSums)
    Trace.trace("xPos_pix: " + xPos_pix + "    yPos_pix: " + yPos_pix)
    val p1 = new Point2D.Double(xPos_pix, yPos_pix)
    val epidPos = trans.pix2Iso(xPos_pix, yPos_pix)
    Trace.trace("epidPos: " + epidPos)

    if (true) {
      val rect2 = {
        val radius = 4.0
        val x = xPos_pix - trans.iso2PixDistX(radius)
        val y = yPos_pix - trans.iso2PixDistY(radius)
        val w = trans.iso2PixDistX(radius * 2)
        val h = trans.iso2PixDistY(radius * 2)
        new Rectangle(di(x), di(y), di(w), di(h))
      }
      val si = image.getSubimage(rect2)
      val xPos_pix2 = rect2.getX + LocateMax.locateMax(si.columnSums)
      val yPos_pix2 = rect2.getY + LocateMax.locateMax(si.rowSums)
      val p2 = new Point2D.Double(xPos_pix2, yPos_pix2)
      Trace.trace("xPos_pix2: " + xPos_pix2 + "    yPos_pix2: " + yPos_pix2 + "    dist: " + p2.distance(p1))
    }

    val IsocenterPosition = {
      val empty = new Point3d(0.0, 0.0, 0.0)
      val a = al.get(TagFromName.IsocenterPosition)
      if (a == null)
        empty
      else {
        val v = a.getDoubleValues
        if (v.size != 3) empty
        else new Point3d(a.getDoubleValues)
      }
    }

    //    val isoPos = IsocenterPosition.add(???)
    //    ???
    p1
  }

  def testFindBB(al: AttributeList) = findBB(al)

  def runProcedure(extendedData: ExtendedData, imageList: Seq[AttributeList]): Either[Elem, BBbyEPIDResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of EPID Alignment")
      logger.info("Finished analysis of EPID Alignment")
      ???
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}

