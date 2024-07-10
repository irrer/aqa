package org.aqa.webrun.convertDicomDev

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util

import java.util.Date

/**
 * Required files for making a series.
 *
 * @param rtplan        RTPLAN to use
 * @param rtimageList   List of RTIMAGE files uploaded by user
 * @param prototypeList List of RTIMAGE files whose metadata can be used to make RTIMAGES.
 *
 */

case class SeriesMakerReq(rtplan: AttributeList, rtimageList: Seq[AttributeList], prototypeList: Seq[AttributeList]) {}

object SeriesMakerReq {

  /**
   * Get the content date+time of the given dicom.
   *
   * @param dicom For this DICOM.
   * @return content date+time
   */
  def getContentDateTime(dicom: AttributeList): Date = DicomUtil.getTimeAndDate(dicom, TagByName.ContentDate, TagByName.ContentTime).get


  def extractDistinctRtimageList(dicomList: Seq[AttributeList]): Seq[AttributeList] = {
    // @formatter:off
    dicomList.
      filter(Util.isRtimage).                     // only RTIMAGE modality
      groupBy(Util.sopOfAl).values.map(_.head).   // must be distinct by SOP instance UID
      toSeq.sortBy(getContentDateTime)            // sort by content date+time
    // @formatter:on
  }


  /**
   * Find the RTPLAN to use.  Give first priority to the one uploaded, but if no RTPLAN was
   * uploaded, then use the one referenced by the RTIMAGE file(s).
   *
   * If there was more than one RTPLAN uploaded or the RTIMAGE files reference more than
   * one RTPLAN, then return an error.  Also return an error if an RTPLAN file can not be found.
   *
   * Note that a 'known' RTPLAN file means that it was one that is in the AQA database.
   *
   * @param alList List of DICOM files uploaded.
   * @return The RTPLAN to use, or, an error message.
   */
  private def rtplanToUse(alList: Seq[AttributeList]): Either[String, AttributeList] = {

    val rtplanList = alList.filter(al => Util.isRtplan(al))
    0 match {
      case _ if rtplanList.size == 1 => // User uploaded exactly one plan, so use it.
        Right(rtplanList.head)

      case _ if rtplanList.size > 1 => // User uploaded more than one plan, so it is ambiguous as to which should be used.
        Left(s"More than one RTPLAN was uploaded.  You should either upload one or zero. \\n If zero, then one of the RTIMAGE files should reference a known RTPLAN.")

      case _ if rtplanList.isEmpty => // User did not upload a plan.  Look to see if any of the RTIMAGE files reference a plan that is in the database.
        val rtimageList = extractDistinctRtimageList(alList)
        val referencedRtplanUidList = rtimageList.flatMap(ri => Util.getRtplanSop(ri)).distinct

        def getRtplanOfImage(rtplanUid: String): Option[AttributeList] = {
          DicomSeries.getBySopInstanceUID(rtplanUid).flatMap(_.attributeListList).find(planAl => Util.sopOfAl(planAl).equals(rtplanUid))
        }

        // list of plans from the database that are referenced by the uploaded RTIMAGE files
        val referencedRtplanList = referencedRtplanUidList.flatMap(getRtplanOfImage)

        0 match {
          case _ if referencedRtplanList.isEmpty => // RTIMAGE files did not reference any plans
            Left("No RTPLAN was uploaded and none of the RTIMAGE files reference a known RTPLAN.")

          case _ if referencedRtplanList.size == 1 => // RTIMAGE files referenced exactly one plan that was found in the database, so use it.
            Right(referencedRtplanList.head) // SUCCESS!

          case _ if referencedRtplanList.isEmpty => // RTIMAGE files referenced more than one plan that was found in the database, so this is ambiguous.
            Left(s"No RTPLAN was uploaded and none of the RTIMAGE files reference ${referencedRtplanList.size} known RTPLANS.")
        }
    }
  }

  /**
   * Determine if an RTIMAGE was created in developer (on the treatment machine) mode.
   *
   * @param al RTIMAGE to test.
   * @return True if developer mode.
   */
  def isDevImage(al: AttributeList): Boolean = {
    def hasMachine = {
      val attr = al.get(TagByName.RadiationMachineName)
      val has = (attr != null) && (attr.getSingleStringValueOrNull() != null) && attr.getSingleStringValueOrNull().nonEmpty
      has
    }

    def hasRtplanRef = Phase2Util.referencedPlanUID(al).nonEmpty

    val is = hasMachine && hasRtplanRef
    is
  }

  /**
   * Get the time and date of an RTIMAGE file.  Use the ContentDate and ContentTime.
   *
   * @param rtimage For this file.
   * @return Date.
   */
  def rtimageTimeDate(rtimage: AttributeList): Date = {
    val date = DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get
    date
  }

  /**
   * Make a set of required files for making a new series.  If valid, return the required files, otherwise return an error message.
   *
   * @param alList List of all DICOM files.
   * @return Requirements or error message.
   */
  def makeRequirements(alList: Seq[AttributeList]): Either[String, SeriesMakerReq] = {

    val rtImageList = extractDistinctRtimageList(alList)

    val rtplan = rtplanToUse(alList)

    0 match {
      case _ if rtplan.isLeft =>
        Left(rtplan.left.get)
      case _ if rtImageList.isEmpty =>
        Left("No RTIMAGE files were uploaded.")
      case _ =>
        val prototypeList = rtImageList.filter(ri => Phase2Util.referencedPlanUID(ri).nonEmpty)
        Right(SeriesMakerReq(rtplan.right.get, rtImageList, prototypeList))
    }
  }
}
