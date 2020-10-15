package org.aqa.webrun.bbByEpid

import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import com.pixelmed.dicom.AttributeList
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import java.awt.Rectangle
import edu.umro.ImageUtil.ImageUtil
import javax.vecmath.Point2i
import org.aqa.Util
import javax.vecmath.Point2d
import edu.umro.ImageUtil.LocateMax
import edu.umro.ScalaUtil.Trace
import org.aqa.db.BBbyEPID
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeTag
import javax.vecmath.Point3d
import com.pixelmed.dicom.TagFromName

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

  /**
   * Represent an EPID result, with enough information to annotate and generate Matlab script.
   *
   * @param pix: Coordinates of center of BB in pixel space.
   *
   * @param al: Attribute list of EPID image.
   *
   * @param iso: Coordinates of center of BB in DICOM gantry space.
   *
   * @param bbByEpid: Database representation.
   *
   */
  case class Result(pix: Point2d, al: AttributeList, iso: Point2d, bbByEpid: BBbyEPID) {
    def AlOf = al
  }

  case class FailedResult(error: String, al: AttributeList) {
    def AlOf = al
  }

  /** Convenience function for getting the attribute list of a result. */
  def alOf(r: Either[FailedResult, Result]): AttributeList = if (r.isLeft) r.left.get.al else r.right.get.al

  /**
   * Translate BB position in ISO plane to RTPLAN coordinates
   *
   * @param epid EPID DICOM
   *
   * @param bbLocation in EPID translated to mm in DICOM gantry coordinates.
   *
   * @param extendedData Associated DB rows
   */
  private def toBBbyEPID(epid: AttributeList, bbLocation: Point2d, outputPK: Long): BBbyEPID = {
    val gantryAngle_deg = Util.gantryAngle(epid)
    val gantryAngle_rad = Math.toRadians(gantryAngle_deg)

    /**
     * EPID offset in the isoplane in mm.
     */
    val epidOffset = {
      val isoCenter = (new IsoImagePlaneTranslator(epid)).caxCenter_iso
      new Point2D.Double(-isoCenter.getX, isoCenter.getY)
    }

    logger.info("gantryAngle_deg: " + gantryAngle_deg)
    logger.info("Using XRayImageReceptorTranslation in isoplane in mm of: " + epidOffset)
    logger.info("bbLocation in isoplane in mm: " + bbLocation)

    val epid3DX_mm = Math.cos(gantryAngle_rad) * (bbLocation.getX + epidOffset.getX)
    val epid3DY_mm = Math.sin(gantryAngle_rad) * (bbLocation.getX + epidOffset.getX)
    val epid3DZ_mm = bbLocation.getY + epidOffset.getY
    val origin = new Point3d(0, 0, 0)

    def getDbl(tag: AttributeTag) = epid.get(tag).getDoubleValues.head

    val bbByEPID = new BBbyEPID(
      bbByEPIDPK = None,
      outputPK = outputPK,
      // rtplanSOPInstanceUID = rtplanSOP,
      epidSOPInstanceUid = Util.sopOfAl(epid),
      offset_mm = (new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm)).distance(origin),
      gantryAngle_deg = gantryAngle_deg,
      status = ProcedureStatus.done.toString,
      epidImageX_mm = bbLocation.getX,
      epidImageY_mm = bbLocation.getY,
      epid3DX_mm, epid3DY_mm, epid3DZ_mm,
      getDbl(TagFromName.TableTopLateralPosition), // tableXlateral_mm
      getDbl(TagFromName.TableTopVerticalPosition), // tableYvertical_mm
      getDbl(TagFromName.TableTopLongitudinalPosition)) // tableZlongitudinal_mm

    logger.info("constructed BBbyEPID: " + BBbyEPID)

    bbByEPID
  }

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
  def findBB(al: AttributeList, outputPK: Long): Either[FailedResult, Result] = {
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

    //    /**
    //     * Put a white pixel at the center of the BB and write a png file.  This is for
    //     * debugging to verify that the bb is found in the expected position.
    //     */
    //    def saveImage(bbCenter_pix: Point2d) = {
    //
    //      val ga = Util.angleRoundedTo90(Util.gantryAngle(al))
    //
    //      val name = ga.formatted("%03d") + "_" + Util.sopOfAl(al) + ".png"
    //      val dir = new java.io.File("""D:\tmp\j16""")
    //      val pngFile = new java.io.File(dir, name)
    //
    //      val dicomImage = new DicomImage(al)
    //      val buf = dicomImage.toDeepColorBufferedImage(0.005)
    //      buf.setRGB(bbCenter_pix.getX.round.toInt, bbCenter_pix.getY.round.toInt, 0xffffff)
    //      Util.writePng(buf, pngFile)
    //    }

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

    if (true) { // TODO rm?
      val col = bbImage.columnSums
      val row = bbImage.rowSums
      val x = LocateMax.locateMax(col) + bbRect.getX
      val y = LocateMax.locateMax(row) + bbRect.getY
      Trace.trace("x: " + x + "    xPos_pix: " + xPos_pix + "    diff: " + (x - xPos_pix))
      Trace.trace("y: " + y + "    yPos_pix: " + yPos_pix + "    diff: " + (y - yPos_pix))

      val xMax = LocateMax.toCubicSpline(col).evaluate(LocateMax.locateMax(col))
      val yMax = LocateMax.toCubicSpline(row).evaluate(LocateMax.locateMax(row))

      val sdCol = ImageUtil.stdDev(col)
      val sdRow = ImageUtil.stdDev(row)

      println("gantry angle: " + Util.gantryAngle(al).toInt + "   xMax: " + xMax + " sd: " + sdCol + "    mean: " + (col.sum / col.size))
      println("gantry angle: " + Util.gantryAngle(al).toInt + "   yMax: " + yMax + " sd: " + sdRow + "    mean: " + (row.sum / row.size))
    }

    val bbCenter_pix = new Point2d(xPos_pix, yPos_pix)
    //    saveImage(bbCenter_pix) // for debug only
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

    if (valid) {
      val bbLocation = new Point2d(bbCenter_mm.getX, -bbCenter_mm.getY)
      val result = new Result(bbCenter_pix, al, bbLocation, toBBbyEPID(al, bbLocation, outputPK)) // Convert Y to DICOM gantry coordinate system
      Trace.trace("find. gantry angle: " + Util.angleRoundedTo90(Util.gantryAngle(al)).formatted("%3d") + "     bbCenter_pix: " + Util.fmtDbl(bbCenter_pix.getX) + ", " + Util.fmtDbl(bbCenter_pix.getY))
      Right(result)
    } else {
      val msg = "Failed to find image of BB in EPID image with sufficient contrast to background. for gantry angle " + Util.gantryAngle(al)
      logger.warn(msg)
      Left(new FailedResult(msg, al))
    }
  }

}

