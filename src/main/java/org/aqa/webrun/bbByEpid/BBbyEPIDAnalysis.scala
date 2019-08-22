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
import edu.umro.ImageUtil.ImageUtil

object BBbyEPIDAnalysis extends Logging {

  private val subProcedureName = "BB by EPID"

  /**
   * Convert double to integer by first rounding it.
   */
  private def d2i(d: Double) = d.round.toInt

  case class BBbyEPIDResult(summry: Elem, sts: ProcedureStatus.Value, position: Seq[Point3d], images: Seq[BufferedImage])

  /**
   * Obtain the sub-area of the image to be searched for the BB.
   */
  private def searchArea(trans: IsoImagePlaneTranslator, center_mm: Point2D.Double, distance_mm: Double): Rectangle = {
    val corner = trans.iso2Pix(center_mm.getX - distance_mm, center_mm.getY - distance_mm)
    val w = trans.iso2PixDistX(distance_mm * 2)
    val h = trans.iso2PixDistY(distance_mm * 2)
    val rect = new Rectangle(d2i(corner.getX), d2i(corner.getY), d2i(w), d2i(h))
    rect
  }

  private def findBB(al: AttributeList): Option[Point2D.Double] = {
    val wholeImage = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    // Using a sub-area eliminates the need for having to deal with other objects, such as the couch rails.
    val searchRect = searchArea(trans, new Point2D.Double(0, 0), Config.BBbyEPIDSearchDistance_mm)
    // image that contains the area to search
    val searchImage = wholeImage.getSubimage(searchRect)

    val bbSizeX_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) * 2
    val bbSizeY_pix = trans.iso2PixDistY(Config.EPIDBBPenumbra_mm) * 2

    // define a rectangle twice the width and height of the BB centered on the coarse location of the BB
    val bbRect = {
      val bbCorner = searchImage.getMaxRect(d2i(bbSizeX_pix), d2i(bbSizeY_pix))
      val bbRectX = searchRect.getX + bbCorner.getX - (bbSizeX_pix / 2.0)
      val bbRectY = searchRect.getY + bbCorner.getY - (bbSizeY_pix / 2.0)
      val bbRectW = bbSizeX_pix * 2.0
      val bbRectH = bbSizeY_pix * 2.0
      new Rectangle(d2i(bbRectX), d2i(bbRectY), d2i(bbRectW), d2i(bbRectH))
    }

    // make an image of the BB
    val bbImage = wholeImage.getSubimage(bbRect)

    // the minimum value of a pixel to be counted as part of the BB
    val minPixValue = {
      // number of pixels that BB should cover pi-r^2.  This is the number of pixels in the image that
      // should be considered when calculating the center of mass to find the center of the BB.
      val bbCount = Math.PI * ((bbSizeX_pix * bbSizeY_pix) / 4)

      def takeLargest(hist: Seq[DicomImage.HistPoint]): Float = {
        if (hist.map(h => h.count).sum > bbCount)
          takeLargest(hist.tail)
        else
          hist.head.value
      }
      takeLargest(bbImage.histogram)
    }

    // make an image of the BB with the non-BB pixels set to zero.
    val bbImageEnhanced = {
      def ifOk(x: Int, y: Int) = {
        val p = bbImage.get(x, y)
        if (p >= minPixValue) p else 0.toFloat
      }
      val pixels = (0 until bbImage.height).map(y => (0 until bbImage.width).map(x => ifOk(x, y)))
      new DicomImage(pixels)
    }

    // find the exact center within the enhanced image using center of mass
    val xPos_pix = ImageUtil.centerOfMass(bbImageEnhanced.columnSums) + bbRect.getX
    val yPos_pix = ImageUtil.centerOfMass(bbImageEnhanced.rowSums) + bbRect.getY

    Trace.trace("xPos_pix: " + xPos_pix + "    yPos_pix: " + yPos_pix)
    val bbCenter_pix = new Point2D.Double(xPos_pix, yPos_pix)
    val bbCenter_mm = trans.pix2Iso(xPos_pix, yPos_pix)
    Trace.trace("bbCenter_mm: " + bbCenter_mm)

    val valid = {
      val searchImagePix = searchImage.pixelData.flatten
      val searchImageMean = searchImagePix.sum / searchImagePix.size
      val searchStdDev = ImageUtil.stdDev(searchImagePix)
      val bbPixValueList = bbImageEnhanced.pixelData.flatten.filter(p => p != 0)
      val bbMean = bbPixValueList.sum / bbPixValueList.size
      val bbStdDevFactor = (bbMean - searchImageMean).abs / searchStdDev
      val ok = bbStdDevFactor >= Config.EPIDBBMinimumStandardDeviation
      logger.info("EPID bbStdDevFactor: " + bbStdDevFactor + "    valid: " + ok)
      ok
    }

    if (valid)
      Some(bbCenter_mm)
    else {
      logger.warn("Failed to find image of BB in EPID image with sufficient contrast to background.")
      None
    }
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

