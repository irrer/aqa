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
   * Obtain the sub-area of the image to be searched for the BB.  Using a sub-area eliminates the
   * need for having to deal with other objects, such as the couch rails.
   */
  private def searchArea(trans: IsoImagePlaneTranslator): Rectangle = {
    val corner = trans.iso2Pix(-Config.BBbyEPIDSearchDistance_mm, -Config.BBbyEPIDSearchDistance_mm)
    val w = trans.iso2PixDistX(Config.BBbyEPIDSearchDistance_mm * 2)
    val h = trans.iso2PixDistY(Config.BBbyEPIDSearchDistance_mm * 2)
    val rect = new Rectangle(di(corner.getX), di(corner.getY), di(w), di(h))
    rect
  }

  private def findBB(al: AttributeList) = {
    val image = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    val rect = searchArea(trans)
    val subImage = image.getSubimage(rect)
    if (true) { // TODO rm
      val min = subImage.minPixelValue.round.toInt
      val max = subImage.maxPixelValue.round.toInt
      val off = (max + min) / 2
      println
      for (y <- (0 until subImage.height)) {
        for (x <- (0 until subImage.width)) {
          print((subImage.get(x, y) - off).round.toInt.formatted(" %4d"))
        }
        println
      }
    }
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

