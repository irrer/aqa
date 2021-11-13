package org.aqa.webrun.LOCBaseline

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util

import java.io.File

object LOCFindRunReq {

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
    * @param rtimage DICOM RTIMAGE of LOC.
    * @return True if is baseline.
    */
  def isBaselinePlan(rtimage: AttributeList): Boolean = maxSizeOfDistinctLeafJawPositions(rtimage) == 2

  /**
    * Check if the file is a delivery image.
    * @param rtimage DICOM RTIMAGE of LOC.
    * @return True if is delivery.
    */
  def isDeliveryPlan(rtimage: AttributeList): Boolean = maxSizeOfDistinctLeafJawPositions(rtimage) == 4

  /**
    * Get the SOP of the RTPLAN that this RTIMAGE references.
    * @param rtimage RTIMAGE
    * @return SOP of RTPLAN
    */
  private def getRtplanSop(rtimage: AttributeList): String = {
    val rtplanRef = DicomUtil.seqToAttr(rtimage, TagByName.ReferencedRTPlanSequence)
    val sop = rtplanRef.map(al => al.get(TagByName.ReferencedSOPInstanceUID)).head.getSingleStringValueOrNull
    sop
  }

  private def getBeamName(rtimage: AttributeList): Option[String] = {
    try {
      val rtplanSop = getRtplanSop(rtimage)
      val ds = DicomSeries.getBySopInstanceUID(rtplanSop).headOption
      val rtplanAl = ds.get.attributeListList.filter(al => Util.sopOfAl(al).equals(rtplanSop)).head
      val beamName = Phase2Util.getBeamNameOfRtimage(rtplanAl, rtimage).get
      Some(beamName)
    } catch {
      case _ => None
    }
  }

  private def isBaselineOpen(rtimage: AttributeList): Boolean = {
    val beamName = getBeamName(rtimage)
    beamName.isDefined && beamName.get.toLowerCase.contains("open")
  }

  private def isBaselineTrans(rtimage: AttributeList): Boolean = {
    val beamName = getBeamName(rtimage)
    beamName.isDefined && beamName.get.toLowerCase.contains("trans")
  }

  private def beamNumberOf(rtimage: AttributeList): Int = {
    DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber).head.getIntegerValues.head
  }

  private def exposureTime(rtimage: AttributeList): Double = {
    DicomUtil.findAllSingle(rtimage, TagByName.ExposureTime).head.getDoubleValues.head
  }

  /**
    * For older LOC data sets, the RTPLAN is not in the database, so other, less reliable clues must be used.
    * <p/>
    * Try to determine which image is which by first testing if the beam number
    * is known.  If that fails, the one with the smaller ExposureTime should be
    * the OPEN beam.
    * @param rtimageList
    * @return Run requirements.
    */
  def byBeamNumber(rtimageList: Seq[AttributeList]): LOCBaselineRunReq = {
    val alA = rtimageList.head
    val alB = rtimageList(1)
    val a = beamNumberOf(alA)
    val b = beamNumberOf(alB)
    val aEx = exposureTime(alA)
    val bEx = exposureTime(alB)

    (a, b) match {
      case (1, 16)               => LOCBaselineRunReq(alA, alB) // both beams known
      case (1, _)                => LOCBaselineRunReq(alA, alB) // open beam known
      case (_, 16)               => LOCBaselineRunReq(alA, alB) // trans beam known
      case (16, 1)               => LOCBaselineRunReq(alB, alA) // both beams known
      case (_, 1)                => LOCBaselineRunReq(alB, alA) // open beam known
      case (16, _)               => LOCBaselineRunReq(alB, alA) // trans beam known
      case (_, _) if (aEx < bEx) => LOCBaselineRunReq(alA, alB) // first is open due to ExposureTime
      case (_, _)                => LOCBaselineRunReq(alB, alA) // second is open due to ExposureTime
    }
  }

  /**
   * Construct the run requirements from the list of RTIMAGEs.
   * @param rtimageList Uploaded by the user.
   * @return Run requirements.
   */
  def constructRunReq(rtimageList: Seq[AttributeList]): LOCBaselineRunReq = {

    val openList = rtimageList.filter(isBaselineOpen)
    val transList = rtimageList.filter(isBaselineTrans)

    def other(rtimage: AttributeList): AttributeList = {
      val sop = Util.sopOfAl(rtimage)
      rtimageList.filterNot(al => Util.sopOfAl(al).equals(sop)).head
    }

    // if both or one the the beams is known, then use that.
    (openList.nonEmpty, transList.nonEmpty) match {
      case (true, true)  => LOCBaselineRunReq(openList.head, transList.head) // Going forward, this should always be true
      case (true, false) => LOCBaselineRunReq(openList.head, other(openList.head))
      case (false, true) => LOCBaselineRunReq(other(transList.head), transList.head)
      case _             => byBeamNumber(rtimageList) // neither beam is known.  May happen when RTPLAN is unavailable.
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
