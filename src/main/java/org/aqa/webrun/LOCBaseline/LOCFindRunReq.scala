package org.aqa.webrun.LOCBaseline

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util

import java.io.File


/**
 * Provide support for determining which of the incoming RTIMAGE files are OPEN and TRANS.
 *
 * Note that this requires that there are exactly two files.  It might be enhanced in the
 * future to handle a larger number of images and ignore the irrelevant ones, but that
 * would depend on having the RTPLAN available.  For newer cases it is, but not for older ones.
 */
object LOCFindRunReq extends Logging {

  /**
    * Given an RTIMAGE, find the maximum number of distinct leaf jaw positions.
    * <p/>
    * This relies on the fact the the delivery images have intentional variations in them, as in:
    * <p/>
    *   ... 29 \  29 \  29 \  29.15 \  29 \  29 \  29 ...
    * <p/>
    * so they have more distinct positions.
    * @param rtimage DICOM RTIMAGE of LOC.
    * @return
    */
  private def maxSizeOfDistinctLeafJawPositions(rtimage: AttributeList): Int = {
    val ljsList = DicomUtil.findAllSingle(rtimage, TagByName.LeafJawPositions)
    val sizeList = ljsList.map(_.getDoubleValues.distinct.length).distinct
    sizeList.max
  }

  /**
    * Check if the file is a baseline image.
    * @param rtplan DICOM RTPLAN of LOC.
    * @return True if is baseline.
    */
  private def isBaselinePlan(rtplan: AttributeList): Boolean = maxSizeOfDistinctLeafJawPositions(rtplan) == 2

  /**
    * Check if the file is a delivery image.
    * @param rtplan DICOM RTPLAN of LOC.
    * @return True if is delivery.
    */
  private def isDeliveryPlan(rtplan: AttributeList): Boolean = maxSizeOfDistinctLeafJawPositions(rtplan) == 4

  /**
    * Get the beam name by looking up the RTIMAGE in the DB and then looking for its name there.  If
    * the RTPLAN is not available or the beam is not defined, then return None.
    *
    * @param rtimage RTIMAGE
    * @return Name of beam if possible, None if not.
    */
  private def getBeamName(rtimage: AttributeList, rtplan: AttributeList): Option[String] = {
    try {
      val beamName = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage).get
      Some(beamName)
    } catch {
      case _: Throwable => None
    }
  }

  private def isBaselineOpen(rtimage: AttributeList, rtplan: AttributeList): Boolean = {
    val beamName = getBeamName(rtimage, rtplan)
    beamName.isDefined && beamName.get.toLowerCase.contains("open")
  }

  private def isBaselineTrans(rtimage: AttributeList, rtplan: AttributeList): Boolean = {
    val beamName = getBeamName(rtimage, rtplan)
    beamName.isDefined && beamName.get.toLowerCase.contains("trans")
  }

  private def beamNumberOf(rtimage: AttributeList): Int = {
    DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber).head.getIntegerValues.head
  }

  private def exposureTime(rtimage: AttributeList): Double = {
    DicomUtil.findAllSingle(rtimage, TagByName.ExposureTime).head.getIntegerValues.head
  }

  /**
    * For older LOC data sets, the RTPLAN is not in the database, so other, less reliable clues must be used.
    * <p/>
    * Try to determine which image is which by first testing if the beam number
    * is known.  If that fails, the one with the smaller ExposureTime should be
    * the OPEN beam.
    * <p/>
    *  This function assumes that there are at least two RTIMAGE files.
    *
    * @param rtimageList List of RTIMAGE files uploaded.
    * @return Run requirements.
    */
  private def determineBeamIdentityWithoutRTPLAN(rtimageList: Seq[AttributeList]): LOCBaselineRunReq = {
    val aAl = rtimageList.head
    val bAl = rtimageList(1)
    val aExposure = exposureTime(aAl)
    val bExposure = exposureTime(bAl)

    (beamNumberOf(aAl), beamNumberOf(bAl)) match {
      case (1, 16) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain both of the OPEN and TRANS beam identities using beam numbers referencing the RTPLAN.")
        LOCBaselineRunReq(aAl, bAl) // both beams known
      case (16, 1) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain both of the TRANS and OPEN beam identities using beam numbers referencing the RTPLAN.")
        LOCBaselineRunReq(bAl, aAl) // both beams known
      case (1, _) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain the identity of the the OPEN using beam numbers referencing the RTPLAN but not the TRANS beam.")
        LOCBaselineRunReq(aAl, bAl) // open beam known
      case (_, 16) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain the identity of the the TRANS using beam numbers referencing the RTPLAN but not the OPEN beam.")
        LOCBaselineRunReq(aAl, bAl) // trans beam known
      case (_, 1) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain the identity of the the OPEN using beam numbers referencing the RTPLAN but not the TRANS beam.")
        LOCBaselineRunReq(bAl, aAl) // open beam known
      case (16, _) =>
        logger.info("LOC Baseline does not have the RTPLAN, but was able to ascertain the identity of the the TRANS using beam numbers referencing the RTPLAN but not the OPEN beam.")
        LOCBaselineRunReq(bAl, aAl) // trans beam known
      case (_, _) if aExposure < bExposure =>
        logger.info("LOC Baseline does not have the RTPLAN, but is using the ExposureTime to determine which beam is OPEN and which is TRANS.  OPEN: " + aExposure + "    TRANS: " + bExposure)
        LOCBaselineRunReq(aAl, bAl) // first is open due to ExposureTime
      case (_, _) =>
        logger.info("LOC Baseline does not have the RTPLAN, but is using the ExposureTime to determine which beam is TRANS and which is OPEN.  OPEN: " + bExposure + "    TRANS: " + aExposure)
        LOCBaselineRunReq(bAl, aAl) // second is open due to ExposureTime
    }
  }

  /**
    * Construct the run requirements from the list of RTIMAGES.
    * @param rtimageList Uploaded by the user.
    * @return Run requirements.
    */
  def constructRunReq(rtimageList: Seq[AttributeList]): Either[String, LOCBaselineRunReq] = {

    val rtplan = DicomSeries.getRtplan(rtimageList.head)

    if (rtplan.isDefined) {
      val openList = rtimageList.filter(img => isBaselineOpen(img, rtplan.get))
      val transList = rtimageList.filter(img => isBaselineTrans(img, rtplan.get))

      (openList.nonEmpty, transList.nonEmpty) match {
        case (true, true) =>
          logger.info("LOC Baseline has the RTPLAN and was able to determine the identities of both the OPEN and TRANS beams.")
          Right(LOCBaselineRunReq(openList.head, transList.head)) // Going forward, this should always be true

        case _ =>
          val msg = "Unable to find both OPEN and TRANS beams in list of RTIMAGES"
          logger.info("LOC Baseline has the RTPLAN and was NOT able to determine the identities of both the OPEN and TRANS beams. " + msg)
          Left(msg)
      }
    } else {
      // neither beam is known.  May happen for older cases when RTPLAN is unavailable because it was not made with the plan creator.
      Right(determineBeamIdentityWithoutRTPLAN(rtimageList))
    }

  }

  def main(args: Array[String]): Unit = {
    val deli = """C:\Users\irrer\Downloads\RTPLAN_2020-11-06T10-12-47-851_LOC_TB1\LOC-Delivery.dcm"""
    val base = """C:\Users\irrer\Downloads\RTPLAN_2020-11-06T10-12-47-851_LOC_TB1\LOC-Baseline.dcm"""

    Seq(base, deli).foreach(fn => {
      val file = new File(fn)
      val al = new DicomFile(file).attributeList.get
      println(file.getName + " isBaselinePlan: " + isBaselinePlan(al))
      println(file.getName + " isDeliveryPlan: " + isDeliveryPlan(al))
    })
  }

}
