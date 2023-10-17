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

package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyEPID
import org.aqa.db.DicomSeries
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.Phase2Util

import java.awt.Rectangle
import java.awt.geom.Point2D
import java.util.Date
import javax.vecmath.Point2d
import javax.vecmath.Point2i
import javax.vecmath.Point3d
import scala.annotation.tailrec

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
  case class Result(
      error: Option[String], // None indicates success, otherwise a message indicates the error.
      pix: Point2d,
      al: AttributeList,
      iso: Point2d,
      bbByEpid: BBbyEPID,
      rawSearchArea: Option[DicomImage] = None,
      processedSearchArea: Option[DicomImage] = None,
      bbPointList: Option[Seq[Point2i]] = None,
      bbPixelCount: Option[Int] = None,
      bbMean_cu: Double,
      minMaxColumnRatio: Option[Double]
  ) {
    def AlOf: AttributeList = al

    def ok: Boolean = error.isEmpty

    /**
      * Format extra information concerning the image analysis.
      *
      * @return User friendly information to aid with diagnosing problems.
      */
    def diagnostics: String = {

      def errorText = {
        if (ok)
          "Image noise was sufficiently low to find BB.\n"
        else
          "Error: " + error + "\n"
      }

      def bbPointListText = {
        if (bbPointList.isDefined) {
          "Number of BB pixels found: " + bbPointList.get.size + "\n"
          "BB pixel coordinate list (search area relative): " + bbPointList.get.mkString("  ") + "\n"
        } else
          ""
      }

      def rawSearchAreaText = {

        if (rawSearchArea.isDefined) {
          "Raw search area pixels:\n" + rawSearchArea.get.pixelsToText + "\n"
        } else ""
      }

      def processedSearchAreaText = {
        if (processedSearchArea.isDefined) {
          "Processed search area pixels:\n" + processedSearchArea.get.pixelsToText + "\n"
        } else ""
      }

      def minMaxColumnRatioText = {
        if (minMaxColumnRatio.isDefined)
          "max background column sum / min background column sum\n  (near 1 if amplifiers are calibrated consistently): " + minMaxColumnRatio.get + "\n"
        else
          ""
      }

      val fullText =
        "" + // makes the auto-formatter line the source code up the way I like
          errorText +
          "Precise pixel coordinates (pix): " + pix + "\n" +
          "Precise isoplane coordinates (mm): " + iso + "\n" +
          bbByEpid.toString + "\n" +
          bbPointListText +
          "bbMean_cu: " + bbMean_cu.formatted("%20.10f").trim + "\n" +
          minMaxColumnRatioText +
          rawSearchAreaText +
          processedSearchAreaText

      fullText
    }

    /** Date and time of attribute list. */
    val dateTime: Date = {
      val c = DicomUtil.getTimeAndDate(al, TagFromName.ContentDate, TagFromName.ContentTime)
      val a = DicomUtil.getTimeAndDate(al, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)
      Seq(c, a).flatten.head
    }
  }

  case class FailedResult(error: String, al: AttributeList) {
    def AlOf: AttributeList = al
  }

  /** Convenience function for getting the attribute list of a result. */
  def alOf(r: Either[FailedResult, Result]): AttributeList = if (r.isLeft) r.left.get.al else r.right.get.al

  private def getIsOpenFieldImage(epidAl: AttributeList): Boolean = {

    /** List of jaw  */
    val rtplanDefaultJawList = Seq(-50.0, 50.0)
    def getLeafJawPositionList(al: AttributeList): Seq[Double] = {
      DicomUtil.findAllSingle(al, TagByName.LeafJawPositions).flatMap(_.getDoubleValues).distinct
    }

    /**
      * Call two values equal if they are within 1% of each other.
      * @param a The one.
      * @param b The other.
      * @return True if approximately equal.
      */
    def approximatelyEqual(a: Double, b: Double): Boolean = {
      ((a - b) / a).abs < 0.01
    }

    val epidLeafJawPositionList = getLeafJawPositionList(epidAl)

    /**
      * Return true if each of the EPID jaw positions can be found in the list of plan jaw positions.
      * @param planJaw List of RTPLAN jaw positions.
      * @return True if all EPID jaw positions can be found in the list of plan jaw positions.
      */
    def jawsMatch(planJaw: Seq[Double]): Boolean = {
      epidLeafJawPositionList.map(ej => planJaw.exists(pj => approximatelyEqual(ej, pj))).reduce(_ && _)
    }

    val isClosedField: Boolean =
      try {
        val rtplanUID = DicomUtil.findAllSingle(epidAl, TagByName.ReferencedSOPInstanceUID).head.getSingleStringValueOrNull
        // The DICOM series referenced by this EPID.  Try for an exact match, and if that fails, use one from the same patient.
        val dicomSeries = DicomSeries.getBySopInstanceUID(rtplanUID).headOption match {
          case Some(ds) =>
            logger.info("Found exact match of RTPLAN referenced by EPID.")
            ds
          case _ =>
            val patientId = epidAl.get(TagByName.PatientID).getSingleStringValueOrNull()
            val ds = DicomSeries.getByPatientIdAndModality(patientId, "RTPLAN").filter(_.modality.equals("RTPLAN")).head
            logger.info("Found match of RTPLAN referenced by EPID using PatientID.")
            ds
        }

        // RTPLAN attribute list
        val rtplan: AttributeList = {
          dicomSeries.attributeListList.find(d => Util.sopOfAl(d).equals(rtplanUID)) match {
            case Some(al) => al
            case _        => dicomSeries.attributeListList.head
          }
        }

        val beamNumber = epidAl.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
        val beamSequence = Phase2Util.getBeamSequence(rtplan, beamNumber)
        val planLeafJawPositionList = DicomUtil.findAllSingle(beamSequence, TagByName.LeafJawPositions).flatMap(_.getDoubleValues).distinct
        jawsMatch(planLeafJawPositionList)
      } catch {
        // could not find an RTPLAN.  Use hard-coded values.
        case _: Throwable =>
          jawsMatch(rtplanDefaultJawList)
      }

    !isClosedField
  }

  /**
    * Translate BB position in ISO plane to RTPLAN coordinates
    *
    * @param epid EPID DICOM
    *
    * @param bbLocation in EPID translated to mm in DICOM gantry coordinates.
    *
    * @param outputPK References parent output.
    *
    * @param pixelStandardDeviation_cu standard deviation of pixel near BB.
    *
    * @param pixelMean_cu Mean value of pixels near BB.
    */
  private def toBBbyEPID(epid: AttributeList, bbLocation: Point2d, outputPK: Long, bbStdDevMultiple: Double, pixelStandardDeviation_cu: Double, pixelMean_cu: Double): BBbyEPID = {
    val gantryAngle_deg = Util.gantryAngle(epid)
    val gantryAngle_rad = Math.toRadians(gantryAngle_deg)

    /**
      * EPID offset in the isoplane in mm.
      */
    val epidOffset = {
      val isoCenter = new IsoImagePlaneTranslator(epid).caxCenter_iso
      new Point2D.Double(-isoCenter.getX, isoCenter.getY)
    }

    logger.info("gantryAngle_deg: " + gantryAngle_deg)
    logger.info("Using XRayImageReceptorTranslation in isoplane in mm of: " + epidOffset)
    logger.info("bbLocation in isoplane in mm: " + bbLocation)

    val epid3DX_mm = Math.cos(gantryAngle_rad) * (bbLocation.getX + epidOffset.getX)
    val epid3DY_mm = Math.sin(gantryAngle_rad) * (bbLocation.getX + epidOffset.getX)
    val epid3DZ_mm = bbLocation.getY + epidOffset.getY
    val origin = new Point3d(0, 0, 0)

    def getDbl(tag: AttributeTag) = {
      val attr = epid.get(tag)
      if ((attr == null) || attr.getDoubleValues.isEmpty)
        Double.NaN
      else
        attr.getDoubleValues.head
    }

    // remove the image data from the DICOM and zip it into a byte array
    val metadata_dcm_zip = {
      val al = DicomUtil.clone(epid)
      al.remove(TagFromName.PixelData)
      Some(DicomUtil.dicomToZippedByteArray(Seq(al)))
    }

    val isOpenFieldImage = getIsOpenFieldImage(epid)

    val bbByEPID = new BBbyEPID(
      bbByEPIDPK = None,
      outputPK = outputPK,
      // rtplanSOPInstanceUID = rtplanSOP,
      epidSOPInstanceUid = Util.sopOfAl(epid),
      offset_mm = new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm).distance(origin),
      gantryAngle_deg = gantryAngle_deg,
      status = ProcedureStatus.done.toString,
      epidImageX_mm = bbLocation.getX,
      epidImageY_mm = bbLocation.getY,
      epid3DX_mm,
      epid3DY_mm,
      epid3DZ_mm,
      getDbl(TagByName.TableTopLateralPosition), // table X lateral mm
      getDbl(TagByName.TableTopVerticalPosition), // table Y vertical mm
      getDbl(TagByName.TableTopLongitudinalPosition), // table Z longitudinal mm
      bbStdDevMultiple = bbStdDevMultiple,
      pixelStandardDeviation_cu,
      pixelMean_cu,
      isOpenFieldImage,
      metadata_dcm_zip
    )

    logger.info("constructed BBbyEPID: " + BBbyEPID)

    bbByEPID
  }

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
    * Take the sub-image defined by the rectangle.  Then correct for columnar amplifier error by
    * normalizing each column.  Return the resulting sub-image.
    *
    * @param wholeImage Take sub-image from here.
    * @param searchRect Defines sub-image
    * @param al: Attribute list of RTIMAGE.
    * @return A corrected image.
    */
  private def correctForColumnarAmplification(wholeImage: DicomImage, searchRect: Rectangle, al: AttributeList): (DicomImage, Double) = {
    val x = searchRect.x
    val columnarCorrectionHeight_pix = d2i(searchRect.getHeight)
    val width_pix = searchRect.width
    val aboveImage = wholeImage.getSubimage(new Rectangle(x, searchRect.y - columnarCorrectionHeight_pix, width_pix, columnarCorrectionHeight_pix))
    val belowImage = wholeImage.getSubimage(new Rectangle(x, searchRect.y + searchRect.height, width_pix, columnarCorrectionHeight_pix))

    val avgOfEachColumn: IndexedSeq[Float] =
      aboveImage.columnSums.zip(belowImage.columnSums).map(ab => (ab._1 + ab._2) / (columnarCorrectionHeight_pix * 2))

    // The 'darkest' column.
    val avgOfMinColumn = avgOfEachColumn.min
    val avgOfMaxColumn = avgOfEachColumn.max

    val rawImage = wholeImage.getSubimage(searchRect)

    // map each pixel in the raw image to a new value by lowering it's value by the difference of the given column and the dark column.
    def correctedRow(y: Int): IndexedSeq[Float] = {

      for (x <- 0 until width_pix)
        yield {
          // rawImage.get(x, y) - (avgOfEachColumn(x) - avgOfMinColumn) // additive approach.  Used decades ago to reduce CPU load.
          rawImage.get(x, y) * (avgOfMinColumn / avgOfEachColumn(x)) // multiplicative approach
        }
    }

    val correctedPixels = for (y <- 0 until searchRect.height) yield correctedRow(y)

    val correctedImage = new DicomImage(correctedPixels)

    (correctedImage, avgOfMaxColumn / avgOfMinColumn)
  }

  /**
    * Get the list of pixels that are in the BB by finding the brightest that are adjacent to the core
    * pixels.  Recursively do this until enough pixels have been acquired to cover the area of the BB.
    * In this manner the BB is 'grown', looking for adjacent bright pixels.
    *
    * @param bbImage Image containing the BB and a minimal amount of its surroundings.
    * @param bbCount Number of pixels covering the expected size of the BB.
    * @param inOut List of coordinates of pixels divided into those that are part of the BB and those that are not.  Starts as an empty list and is iteratively built up.
    * @return List of coordinates of pixels divided into those that are part of the BB and those that are not.
    */
  @tailrec
  private def growBBPixList(bbImage: DicomImage, bbCount: Double, inOut: Map[Boolean, Seq[Point2i]]): Map[Boolean, Seq[Point2i]] = {

    // true if adjacent
    def adjacentTo(p1: Point2i, p2: Point2i) = {
      val a = ((p1.getX - p2.getX).abs < 2) && ((p1.getY - p2.getY).abs < 2)
      if (a)
        print("")
      a
    }

    // Make a list of all pixels that are adjacent to at least one of the pixels that are are part of the BB.
    val adjacentList = inOut(false).filter(pOut => inOut(true).exists(pIn => adjacentTo(pIn, pOut)))

    // Of the adjacent pixels, grab the largest one, add it to the BB list, and take it off the non-BB list.
    val maxAdjacent = adjacentList.maxBy(p => bbImage.get(p.getX, p.getY))
    val in = inOut(true) :+ maxAdjacent
    val out = inOut(false).diff(Seq(maxAdjacent))

    // Create a new map that contains two lists of points, one for the BB and one list that is not part of the BB.
    val newInOut: Map[Boolean, Seq[Point2i]] = Map((true, in), (false, out))

    // If enough pixels have been acquired to cover the area of the BB then quit, otherwise get the next adjacent
    // pixel that is the brightest..
    if (in.size >= bbCount) newInOut else growBBPixList(bbImage, bbCount, newInOut)
  }

  /**
    * Create a list of points to be used in the first pass to find the brightest spot in the search area.
    *
    * Pixel coordinates are 0-relative to the upper left corner of the image.
    *
    * @param trans Image translator to get pixel sizes.
    * @param bbRadius The radius of the BB.
    * @return List of points in the circle.
    */
  private def listOfPointsWithinBBRadius(trans: IsoImagePlaneTranslator, bbRadius: Double = Config.EPIDBBPenumbra_mm): Seq[Point2i] = {
    val pad = 4 // Expand area for finding pixels so that near misses will be included.
    val radiusX = d2i(trans.iso2PixDistX(bbRadius)) + pad
    val radiusY = d2i(trans.iso2PixDistY(bbRadius)) + pad

    def near(x: Int, y: Int, offset_pix: Double = 0): Boolean = {
      val offsetX_mm = trans.pix2IsoDistX(offset_pix)
      val offsetY_mm = trans.pix2IsoDistY(offset_pix)
      val x_mm = trans.pix2IsoDistX(x) + offsetX_mm
      val y_mm = trans.pix2IsoDistY(y) + offsetY_mm
      val distance = Math.sqrt((x_mm * x_mm) + (y_mm * y_mm))

      bbRadius > distance
    }

    val rawPointList0 = for (x <- -radiusX until radiusX; y <- -radiusY until radiusY; if near(x, y)) yield new Point2i(x, y)
    val rawPointListHalf = for (x <- -radiusX until radiusX; y <- -radiusY until radiusY; if near(x, y, 0.5)) yield new Point2i(x, y)

    val rawPointList = if (rawPointList0.size > rawPointListHalf.size) rawPointList0 else rawPointListHalf

    val minX = rawPointList.map(_.x).min
    val minY = rawPointList.map(_.y).min

    val pointList = rawPointList.map(p => new Point2i(p.x - minX, p.y - minY))
    pointList
  }

  /**
    * Provide a testing entry point.
    * @param trans Translator.
    * @return List of points.
    */
  def testListOfPointsWithinBB(trans: IsoImagePlaneTranslator): Seq[Point2i] = listOfPointsWithinBBRadius(trans)

  /**
    * Find the core pixes of the BB to within 2 pixels.
    *
    * Take a template of the BB and iterate it over every possible position in the given
    * image.  Return the place where the sum of the values is max.
    *
    * @param trans Translate mm to pixels.
    * @param image Search this image for the core.
    * @return A list of BB points relative to the given image.
    */
  private def locateBbCoarsely(trans: IsoImagePlaneTranslator, image: DicomImage): Seq[Point2i] = {
    val bbModelPointList = listOfPointsWithinBBRadius(trans)
    val width = bbModelPointList.map(_.x).max
    val height = bbModelPointList.map(_.y).max

    val allPoints = for (x <- 0 until (image.width - width); y <- 0 until (image.height - height)) yield new Point2i(x, y)

    def sumOf(point: Point2i): Float = {
      bbModelPointList.map(p => image.get(p.x + point.x, p.y + point.y)).sum
    }

    val maxLocation = allPoints.maxBy(sumOf)
    val shiftedPointList = bbModelPointList.map(p => new Point2i(p.x + maxLocation.x, p.y + maxLocation.y))
    shiftedPointList
  }

  /**
    * Find the center of the list of given points.
    * @param list List of points.
    * @return Point in the middle.
    */
  private def pointListCenter(list: Seq[Point2i]) = {
    val x = list.map(_.x).sum / list.size.toDouble
    val y = list.map(_.y).sum / list.size.toDouble
    new Point2i(d2i(x), d2i(y))
  }

  /**
    * Given a list of points in the search image, filter out those that are not part of the BB or the BB's penumbra.
    * @param pointList List of image points.
    * @param preciseLocationWholeImage_pix Precise location of BB in pixels.
    * @param trans For translating between pixel plane and isoplane.
    * @return List of just background pixels.
    */
  private def getBackgroundPoints(pointList: Seq[Point2i], preciseLocationWholeImage_pix: Point2d, trans: IsoImagePlaneTranslator): Seq[Point2i] = {
    val requiredDistance_mm = Config.EPIDBBPenumbra_mm * 3
    def isBackground(p: Point2i): Boolean = {
      val xDist_mm = trans.pix2IsoDistX(p.x - preciseLocationWholeImage_pix.x)
      val yDist_mm = trans.pix2IsoDistY(p.y - preciseLocationWholeImage_pix.y)
      val dist_mm = Math.sqrt(xDist_mm * xDist_mm + yDist_mm * yDist_mm)
      dist_mm > requiredDistance_mm
    }
    pointList.filter(isBackground)
  }

  case class PreciseLocation(precise_pix: Point2d, bbMean_cu: Double, backgroundMean_cu: Double, backgroundStdDev_cu: Double, bbPixelList: Seq[Point2i]) {}

  private def preciseLocationUsingBBGrowth(coarseCenter: Point2i, searchImage: DicomImage, trans: IsoImagePlaneTranslator, searchRect: Rectangle): PreciseLocation = {

    val inOut: Map[Boolean, Seq[Point2i]] = {
      val center = (true, Seq(coarseCenter))
      val empty =
        for (x <- 0 until searchImage.width; y <- 0 until searchImage.height; if !((x == coarseCenter.x) && (y == coarseCenter.y)))
          yield new Point2i(x, y)
      val list = Seq(center, (false, empty)).toMap
      list
    }

    val bbSizeX_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) * 2
    val bbSizeY_pix = trans.iso2PixDistY(Config.EPIDBBPenumbra_mm) * 2
    val bbCount = Math.PI * ((bbSizeX_pix * bbSizeY_pix) / 4)

    val all = growBBPixList(searchImage, bbCount, inOut)
    val inPixXY = all(true)
    val outPixXY = all(false)
    val inValue = inPixXY.map(p => searchImage.get(p.getX, p.getY)) // list of values of pixels that are part of the BB

    // calculate the center of mass for all points in the BB
    val sumMass = inValue.sum
    val xPos_pix = (inPixXY.map(p => p.getX * searchImage.get(p.getX, p.getY)).sum / sumMass) + searchRect.getX
    val yPos_pix = (inPixXY.map(p => p.getY * searchImage.get(p.getX, p.getY)).sum / sumMass) + searchRect.getY

    val preciseLocationWholeImage_pix = new Point2d(xPos_pix, yPos_pix)
    val preciseLocationSearchArea_pix = new Point2d(preciseLocationWholeImage_pix.x - searchRect.getX, preciseLocationWholeImage_pix.y - searchRect.getY)
    val backgroundValueList = getBackgroundPoints(outPixXY, preciseLocationSearchArea_pix, trans).map(p => searchImage.get(p.x, p.y))

    val backgroundMean_cu = backgroundValueList.sum / backgroundValueList.size
    val backgroundStdDev_cu = ImageUtil.stdDev(backgroundValueList)

    val bbMean_cu = sumMass / inValue.size

    PreciseLocation(new Point2d(xPos_pix, yPos_pix), bbMean_cu, backgroundMean_cu, backgroundStdDev_cu, bbPixelList = inPixXY)
  }

  /**
    * Find the BB in the RTIMAGE.  Correct for columnar amplification by normalizing pixel columns.  Use
    * the processed image for locating the BB.
    *
    * Note: This puts statistics from the raw image into the database as a more accurate way of
    * assessing EPID health.
    *
    * @param al RTIMAGE.
    * @param outputPK Output associated with this data.
    * @return Either success with data, or failure with an explanation.
    */
  def findBB_columnCorrectedGrowBB(al: AttributeList, outputPK: Long): Result = {

    val wholeImage = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)

    // Using a sub-area eliminates the need for having to deal with other objects, such as the couch rails.
    val searchRect = searchArea(trans, new Point2d(0, 0), Config.BBbyEPIDSearchDistance_mm)

    // image that contains the area to search
    val searchImageColumnarCorrectedAndMaxMin = correctForColumnarAmplification(wholeImage, searchRect, al)
    val searchImageColumnarCorrected = searchImageColumnarCorrectedAndMaxMin._1
    val minMaxColumnRatio = searchImageColumnarCorrectedAndMaxMin._2
    val coarseColumnarPixelList = locateBbCoarsely(trans, searchImageColumnarCorrected)

    val coarseCenter = pointListCenter(coarseColumnarPixelList)

    val preciseStats = preciseLocationUsingBBGrowth(coarseCenter, searchImageColumnarCorrected, trans, searchRect)

    val backgroundCoOfVar = preciseStats.backgroundStdDev_cu / preciseStats.backgroundMean_cu
    println("Background coefficient of variation: " + backgroundCoOfVar)

    val bbStdDevMultiple = (preciseStats.bbMean_cu - preciseStats.backgroundMean_cu) / preciseStats.backgroundStdDev_cu

    val bbCenter_mm = {
      val p = trans.pix2Iso(preciseStats.precise_pix.getX, preciseStats.precise_pix.getY)
      new Point2d(p.getX, -p.getY)
    }

    val error = {
      0 match {
        case _ if backgroundCoOfVar > Config.EPIDBBMaxBackgroundCoefficientOfVariation => Some("Image is too noisy.  Possibly the table is mis-positioned.")
        case _ if bbStdDevMultiple < Config.EPIDBBMinimumStandardDeviation             => Some("The image is too noisy to confidently locate the BB.")
        case _                                                                         => None
      }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // Statistics of raw (not columnar corrected) image. These are put in the database and are a
    // more realistic view of EPID health.
    val preciseStatsRaw = preciseLocationUsingBBGrowth(coarseCenter, wholeImage.getSubimage(searchRect), trans, searchRect)
    val backgroundCoOfVarRaw = preciseStatsRaw.backgroundStdDev_cu / preciseStatsRaw.backgroundMean_cu
    println("Background coefficient of variation raw: " + backgroundCoOfVarRaw)

    val bbStdDevMultipleRaw = (preciseStatsRaw.bbMean_cu - preciseStatsRaw.backgroundMean_cu) / preciseStatsRaw.backgroundStdDev_cu

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    val result =
      Result(
        error,
        pix = preciseStats.precise_pix,
        al = al,
        iso = bbCenter_mm,
        toBBbyEPID(al, bbCenter_mm, outputPK, bbStdDevMultipleRaw, preciseStatsRaw.backgroundStdDev_cu, preciseStatsRaw.backgroundMean_cu),
        rawSearchArea = Some(wholeImage.getSubimage(searchRect)),
        processedSearchArea = Some(searchImageColumnarCorrected),
        bbPointList = Some(preciseStats.bbPixelList),
        bbMean_cu = preciseStats.bbMean_cu,
        minMaxColumnRatio = Some(minMaxColumnRatio)
      )

    result
  }

  // ------------------------------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------------------------------

  // the old way.  This code is retained for comparison of results.  It does not use columnar amplification correction.
  /**
    * Define a rectangle twice the width and height of the BB centered on the coarse location of the BB
    * @param searchImage Sub-image to search for BB.
    * @param trans Iso <--> pixel
    * @param searchRect Describes location of search area within whole image.
    * @return
    */
  private def makeBBRect(searchImage: DicomImage, trans: IsoImagePlaneTranslator, searchRect: Rectangle): Rectangle = {
    val bbSizeX_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) * 2
    val bbSizeY_pix = trans.iso2PixDistY(Config.EPIDBBPenumbra_mm) * 2
    val bbCorner = searchImage.getMaxRect(d2i(bbSizeX_pix), d2i(bbSizeY_pix)) // search for brightest BB-sized rectangle
    val bbRectX = searchRect.getX + bbCorner.getX - (bbSizeX_pix / 2.0)
    val bbRectY = searchRect.getY + bbCorner.getY - (bbSizeY_pix / 2.0)
    val bbRectW = bbSizeX_pix * 2.0
    val bbRectH = bbSizeY_pix * 2.0
    new Rectangle(d2i(bbRectX), d2i(bbRectY), d2i(bbRectW), d2i(bbRectH))
  }

  def findBBold(al: AttributeList, outputPK: Long): Result = {
    val wholeImage = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)
    // Using a sub-area eliminates the need for having to deal with other objects, such as the couch rails.
    val searchRect = searchArea(trans, new Point2d(0, 0), Config.BBbyEPIDSearchDistance_mm)
    // image that contains the area to search
    val searchImage = wholeImage.getSubimage(searchRect)

    val bbSizeX_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) * 2
    val bbSizeY_pix = trans.iso2PixDistY(Config.EPIDBBPenumbra_mm) * 2

    val bbRect = makeBBRect(searchImage, trans, searchRect) // close fitting rectangle around the BB

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

    // Two lists of points, those that are in the BB and those that are not.
    val inOut = pixList.groupBy(p => bbCorePix.contains(p))

    val all = growBBPixList(bbImage, bbCount, inOut)
    val inPixXY = all(true)
    val outPixXY = all(false)
    val inValue = inPixXY.map(p => bbImage.get(p.getX, p.getY))
    val outValue = outPixXY.map(p => bbImage.get(p.getX, p.getY))
    val outStdDev = ImageUtil.stdDev(outValue)

    // calculate the center of mass for all points in the BB
    val sumMass = inValue.sum
    val xPos_pix = (inPixXY.map(p => p.getX * bbImage.get(p.getX, p.getY)).sum / sumMass) + bbRect.getX
    val yPos_pix = (inPixXY.map(p => p.getY * bbImage.get(p.getX, p.getY)).sum / sumMass) + bbRect.getY

    val bbCenter_pix = new Point2d(xPos_pix, yPos_pix)

    val bbCenter_mm = trans.pix2Iso(xPos_pix, yPos_pix)

    val outMean = outValue.sum / outValue.size // mean of pixels outside the BB
    val bbMean_cu = sumMass / inPixXY.size // mean of pixels inside the BB
    // how many times larger (number of multiples) of the difference of the BB's mean is than the standard deviation
    val bbStdDevMultiple = (bbMean_cu - outMean).abs / outStdDev

    val valid = {
      val ok = bbStdDevMultiple >= Config.EPIDBBMinimumStandardDeviation
      ok
    }

    val error = {

      if (bbStdDevMultiple >= Config.EPIDBBMinimumStandardDeviation)
        None
      else
        Some("Failed to find image of BB in EPID image with sufficient contrast to background. for gantry angle " + Util.gantryAngle(al))
    }

    if (error.isDefined)
      logger.warn("Daily QA EPID Failure: " + error.get)

    logger.info("EPID bbStdDevMultiple: " + bbStdDevMultiple + "    valid: " + valid)

    // calculate values related to image quality
    val pixelStandardDeviation_cu = ImageUtil.stdDev(outValue)
    val pixelMean_cu = outValue.sum / outPixXY.size

    val xOffset = bbRect.x - searchRect.x
    val yOffset = bbRect.y - searchRect.y
    val bbLocation = new Point2d(bbCenter_mm.getX, -bbCenter_mm.getY)
    val result =
      Result(
        error,
        bbCenter_pix,
        al,
        bbLocation,
        toBBbyEPID(al, bbLocation, outputPK, bbStdDevMultiple = bbStdDevMultiple, pixelStandardDeviation_cu, pixelMean_cu),
        rawSearchArea = Some(searchImage),
        bbPointList = Some(inPixXY.map(p => new Point2i(p.x + xOffset, p.y + yOffset))),
        bbMean_cu = bbMean_cu.toDouble,
        minMaxColumnRatio = None
      ) // Convert Y to DICOM gantry coordinate system

    result
  }

  // define the official function
  def findBB(al: AttributeList, outputPK: Long): Result = findBB_columnCorrectedGrowBB(al, outputPK)

}
