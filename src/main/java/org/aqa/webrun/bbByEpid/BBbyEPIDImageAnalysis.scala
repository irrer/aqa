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
  case class Result(pix: Point2d, al: AttributeList, iso: Point2d, bbByEpid: BBbyEPID) {
    def AlOf: AttributeList = al
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

    def getDbl(tag: AttributeTag) = epid.get(tag).getDoubleValues.head

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
      getDbl(TagByName.TableTopLateralPosition), // tableXlateral_mm
      getDbl(TagByName.TableTopVerticalPosition), // tableYvertical_mm
      getDbl(TagByName.TableTopLongitudinalPosition), // tableZlongitudinal_mm
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

    // Two lists of points, those that are in the BB and those that are not.
    val inOut = pixList.groupBy(p => bbCorePix.contains(p))

    /**
      * Get the list of pixels that are in the BB by finding the largest that are adjacent to the core
      * pixels.  Recursively do this until enough pixels have been acquired to cover the area of the BB.
      * In this manner the BB is 'grown', looking for adjacent bright pixels.
      */
    @tailrec
    def getListOfPixInBB(inOut: Map[Boolean, Seq[Point2i]]): Map[Boolean, Seq[Point2i]] = {

      // true if adjacent
      def adjacentTo(p1: Point2i, p2: Point2i) = ((p1.getX - p2.getX).abs < 2) && ((p1.getY - p2.getY).abs < 2)
      // Make a list of all pixels that are adjacent to at least one of the pixels that are are part of the BB.
      val adjacentList = inOut(false).filter(pOut => inOut(true).exists(pIn => adjacentTo(pIn, pOut)))

      // Of the adjacent pixels, grab the largest one, add it to the BB list, and take it off the non-BB list.
      val maxAdjacent = adjacentList.maxBy(p => bbImage.get(p.getX, p.getY))
      val in = inOut(true) :+ maxAdjacent
      val out = inOut(false).diff(Seq(maxAdjacent))

      // Create a new map that contains two lists of points, one for the BB and one list that is not part of the BB.
      val newInOut = Map((true, in), (false, out))

      // If enough pixels have been acquired to cover the area of the BB then quit, otherwise get the next adjacent
      // pixel that is the brightest..
      if (in.size >= bbCount) newInOut else getListOfPixInBB(newInOut)
    }

    val all = getListOfPixInBB(inOut)
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
    val bbMean = sumMass / inPixXY.size // mean of pixels inside the BB
    // how many times larger (number of multiples) of the difference of the BB's mean is than the standard deviation
    val bbStdDevMultiple = (bbMean - outMean).abs / outStdDev

    val valid = {
      val ok = bbStdDevMultiple >= Config.EPIDBBMinimumStandardDeviation
      ok
    }
    logger.info("EPID bbStdDevMultiple: " + bbStdDevMultiple + "    valid: " + valid)

    // calculate values related to image quality
    val pixelStandardDeviation_cu = ImageUtil.stdDev(outValue)
    val pixelMean_cu = outValue.sum / outPixXY.size

    if (valid) {
      val bbLocation = new Point2d(bbCenter_mm.getX, -bbCenter_mm.getY)
      val result =
        Result(
          bbCenter_pix,
          al,
          bbLocation,
          toBBbyEPID(al, bbLocation, outputPK, bbStdDevMultiple = bbStdDevMultiple, pixelStandardDeviation_cu, pixelMean_cu)
        ) // Convert Y to DICOM gantry coordinate system

      Right(result)
    } else {
      val msg = "Failed to find image of BB in EPID image with sufficient contrast to background. for gantry angle " + Util.gantryAngle(al)
      logger.warn(msg)
      Left(FailedResult(msg, al))
    }
  }

}
