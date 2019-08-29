package org.aqa.webrun.bbByEpid

import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import com.pixelmed.dicom.AttributeList
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import java.awt.Rectangle
import edu.umro.ImageUtil.ImageUtil
import javax.vecmath.Point2i
import org.aqa.Util
import javax.vecmath.Point2d

/**
 * Locate the BB in the EPID image.  The following steps are taken:
 *
 * - Limit the search to a square in the center of the image.  This allows the algorithm to ignore unrelated objects like couch rails.
 *
 * - Make a template rectangle the width and height of the BB and search the search area for the rectangle that has the largest sum of pixel values.
 *
 * - Make a template rectangle twice the width and height of the BB and find the brightest small cluster of pixels.  Use that as the core of the BB.
 *
 * - Determine the number of pixels that the BB should cover.
 *
 * - Recursively add to the core pixels, the brightest adjacent pixels, until there are enough to account for the area of the BB.
 *
 * - Perform a center of mass calculation on the core pixels and use the result as the final value.
 */
object BBbyEPIDImageAnalysis extends Logging {

  private val subProcedureName = "BB by EPID"

  /**
   * Convert double to integer by first rounding it.
   */
  private def d2i(d: Double) = d.round.toInt

  /**
   * Obtain the sub-area of the image to be searched for the BB.
   */
  private def searchArea(trans: IsoImagePlaneTranslator, center_mm: Point2d, distance_mm: Double): Rectangle = {
    val corner = trans.iso2Pix(center_mm.getX - distance_mm, center_mm.getY - distance_mm)
    val w = trans.iso2PixDistX(distance_mm * 2)
    val h = trans.iso2PixDistY(distance_mm * 2)
    val rect = new Rectangle(d2i(corner.getX), d2i(corner.getY), d2i(w), d2i(h))
    rect
  }

  /**
   * Determine the center of the BB to a precise degree.
   *
   * @param al image to process
   *
   * @return Position of BB in mm in the isoplane.
   */
  def findBB(al: AttributeList): Either[String, Point2d] = {
    val wholeImage = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    // Using a sub-area eliminates the need for having to deal with other objects, such as the couch rails.
    val searchRect = searchArea(trans, new Point2d(0, 0), Config.BBbyEPIDSearchDistance_mm)
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

    // Number of pixels that occupy BB area.
    val bbCount = Math.PI * ((bbSizeX_pix * bbSizeY_pix) / 4)
    def pixList = for (y <- 0 until bbImage.height; x <- 0 until bbImage.width) yield { new Point2i(x, y) }

    // Get a very small group of pixels that will be assumed be be at the core of the BB.
    val bbCorePix = {
      val coreSize = 2 // just a 2x2 area of pixels
      val corner = bbImage.getMaxRect(coreSize, coreSize)
      for (x <- 0 until coreSize; y <- 0 until coreSize) yield { new Point2i(x + corner.getX, y + corner.getY) }
    }

    val inOut = pixList.groupBy(p => bbCorePix.contains(p))

    /**
     * Get the list of pixels that are in the BB by finding the largest that are adjacent to the core pixels.
     */
    def getListOfPixInBB(inOut: Map[Boolean, Seq[Point2i]]): Map[Boolean, Seq[Point2i]] = {
      val minDist = Math.sqrt(2) * 1.001

      // true if adjacent
      def adjacentTo(p1: Point2i, p2: Point2i) = ((p1.getX - p2.getX).abs < 2) && ((p1.getY - p2.getY).abs < 2)
      val adjacentList = inOut(false).filter(pOut => inOut(true).filter(pIn => adjacentTo(pIn, pOut)).nonEmpty)
      val maxAdjacent = adjacentList.maxBy(p => bbImage.get(p.getX, p.getY))
      val in = inOut(true) :+ maxAdjacent
      val out = inOut(false).diff(Seq(maxAdjacent))
      val newInOut = Map((true, in), (false, out))
      if (in.size >= bbCount) newInOut else getListOfPixInBB(newInOut)
    }

    val in = getListOfPixInBB(inOut)(true)

    val sumMass = in.map(p => bbImage.get(p.getX, p.getY)).sum

    val xPos_pix = (in.map(p => p.getX * bbImage.get(p.getX, p.getY)).sum / sumMass) + bbRect.getX
    val yPos_pix = (in.map(p => p.getY * bbImage.get(p.getX, p.getY)).sum / sumMass) + bbRect.getY

    val bbCenter_pix = new Point2d(xPos_pix, yPos_pix)
    val bbCenter_mm = trans.pix2Iso(xPos_pix, yPos_pix)

    val valid = {
      val searchImagePix = searchImage.pixelData.flatten
      val searchImageMean = searchImagePix.sum / searchImagePix.size
      val searchStdDev = ImageUtil.stdDev(searchImagePix)
      val bbMean = sumMass / in.size
      val bbStdDevFactor = (bbMean - searchImageMean).abs / searchStdDev
      val ok = bbStdDevFactor >= Config.EPIDBBMinimumStandardDeviation
      logger.info("EPID bbStdDevFactor: " + bbStdDevFactor + "    valid: " + ok)
      ok
    }

    if (valid)
      Right(new Point2d(bbCenter_mm.getX, bbCenter_mm.getY))
    else {
      val msg = "Failed to find image of BB in EPID image with sufficient contrast to background. for gantry angle " + Util.gantryAngle(al)
      logger.warn(msg)
      Left(msg)
    }
  }

}

