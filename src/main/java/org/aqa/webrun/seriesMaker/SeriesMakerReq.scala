package org.aqa.webrun.seriesMaker

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.AnonymizeUtil
import org.aqa.DicomFile

import java.util.Date

/**
 * Required files for making a series.
 *
 * @param rtplan       RTPLAN to use
 * @param rtimageList  List of RTIMAGE files uploaded by user
 * @param templateList List of RTIMAGE files whose metadata can be used to make RTIMAGES.
 *
 */

case class SeriesMakerReq(rtplan: AttributeList, rtimageList: Seq[DicomFile], machine: Machine, templateList: Seq[AttributeList]) extends Logging {


}

object SeriesMakerReq extends Logging {

  /**
   * Get the content date+time of the given dicom.
   *
   * @param dicom For this DICOM.
   * @return content date+time
   */
  def getContentDateTime(dicom: AttributeList): Date = DicomUtil.getTimeAndDate(dicom, TagByName.ContentDate, TagByName.ContentTime).get


  /**
   * Get the anonymized RTPLAN SOPInstanceUID
   *
   * @param institutionPK For this institution.
   * @param rtplan        non-anonymized RTPLAN.
   * @return
   */
  def anonymizedRtplanSop(institutionPK: Long, rtplan: AttributeList): String = {
    val planSopAttr = rtplan.get(TagByName.SOPInstanceUID)
    val al = new AttributeList
    al.put(planSopAttr)
    val alAnon = AnonymizeUtil.anonymizeDicom(institutionPK, al)
    Util.sopOfAl(alAnon)
  }


  private def extractDistinctRtimageList(dicomList: Seq[DicomFile]): Seq[DicomFile] = {
    // @formatter:off
    dicomList.
      filter(df => Util.isRtimage(df.al)).                     // only RTIMAGE modality
      groupBy(df => Util.sopOfAl(df.al)).values.map(_.head).   // must be distinct by SOP instance UID
      toSeq.sortBy(df => getContentDateTime(df.al))            // sort by content date+time
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
  private def rtplanToUse(institutionPK: Long, alList: Seq[DicomFile]): Either[String, AttributeList] = {

    val rtplanList = alList.filter(df => Util.isRtplan(df.al))
    0 match {
      case _ if rtplanList.size == 1 => // User uploaded exactly one plan, so use it.
        Right(rtplanList.head.al)

      case _ if rtplanList.size > 1 => // User uploaded more than one plan, so it is ambiguous as to which should be used.
        Left(s"More than one RTPLAN was uploaded.  You should either upload one or zero. \\n If zero, then one of the RTIMAGE files should reference a known RTPLAN.")

      case _ if rtplanList.isEmpty => // User did not upload a plan.  Look to see if any of the RTIMAGE files reference a plan that is in the database.
        val rtimageList = extractDistinctRtimageList(alList)
        val referencedRtplanUidList = rtimageList.flatMap(ri => Util.getRtplanSop(ri.al)).distinct

        def getRtplanOfImage(rtplanUid: String): Option[AttributeList] = {
          DicomSeries.getBySopInstanceUID(rtplanUid).flatMap(_.attributeListList).find(planAl => Util.sopOfAl(planAl).equals(rtplanUid))
        }

        // list of plans from the database that are referenced by the uploaded RTIMAGE files
        val referencedRtplanList = referencedRtplanUidList.flatMap(getRtplanOfImage)

        0 match {
          case _ if referencedRtplanList.isEmpty => // RTIMAGE files did not reference any plans
            Left("No RTPLAN was uploaded and none of the RTIMAGE files reference a known RTPLAN.")

          case _ if referencedRtplanList.size == 1 => // RTIMAGE files referenced exactly one plan that was found in the database, so use it.
            val rtplanAnon = referencedRtplanList.head // SUCCESS!
            val rtplan = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(rtplanAnon)).head
            Right(rtplan)

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
   * Get the machine referenced by the uploaded DICOM files.  Try getting it from the RTPLAN file first.
   *
   * @param institutionPK For this institution.
   * @param rtplan        RTPLAN being used.
   * @param rtimageList   List of uploaded RTIMAGE files.
   * @return Either a machine or an error message.
   */
  private def machineOf(institutionPK: Long, rtplan: AttributeList, rtimageList: Seq[DicomFile]): Either[String, Machine] = {
    val machineList: Seq[Machine] = Machine.getForInstitution(institutionPK).filter(machine => machine.getRealDeviceSerialNumber.isDefined && machine.getRealTpsId.isDefined)

    // list of machines referenced by RTIMAGE files as RadiationMachineName
    val machineListFromRtimageList: Seq[Machine] = {
      // list of referenced machine names
      val attrList = rtimageList.flatMap(ri => DicomUtil.findAllSingle(ri.al, TagByName.RadiationMachineName))

      val deAnonSet = attrList.flatMap(attr => AnonymizeUtil.deAnonymizeAttribute(institutionPK, attr)).map(_.getSingleStringValueOrEmptyString()).distinct.filter(_.nonEmpty).toSet

      val referencedMachineList = machineList.filter(machine => machine.getRealTpsId.isDefined && deAnonSet.contains(machine.getRealTpsId.get))
      referencedMachineList
    }

    // list of machines referenced by RTPLAN as TreatmentMachineName
    val machineListFromRtplan: Seq[Machine] = {
      val machineNameSet = DicomUtil.findAllSingle(rtplan, TagByName.TreatmentMachineName).map(_.getSingleStringValueOrEmptyString).distinct.filter(_.nonEmpty).toSet


      // val referencedMachineList = machineList.filter(machine => machine.getRealTpsId.isDefined && machineNameSet.contains(machine.getRealTpsId.get))
      val referencedMachineList = machineList.filter(machine => machine.getRealTpsId.isDefined && machineNameSet.contains(machine.getRealTpsId.get))

      referencedMachineList
    }

    val machine = (machineListFromRtplan ++ machineListFromRtimageList).headOption

    if (machine.isDefined)
      Right(machine.get)
    else
      Left("Could not find a reference to a treatment machine in the uploaded DICOM files.")
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
   * Get beams that can be used as template beams (beams that were delivered in production mode (as
   * opposed to dev mode) that have all of their fields filled in.
   *
   * Do this by first looking through the list of images that were uploaded, using any
   * that were delivered in production mode.  If the plan defines other beams, then look
   * for a previous instance of the plan being analyzed, get the beams, and use them.
   *
   * @param rtplan      RTPLAN to use.
   * @param rtimageList Uploaded RTIMAGE list.
   * @return
   */
  private def getTemplateList(institutionPK: Long, rtplan: Either[String, AttributeList], rtimageList: Seq[AttributeList]): Seq[AttributeList] = {
    if (rtplan.isLeft)
      Seq()
    else {
      val plan = rtplan.right.get
      // list of RTIMAGES that point to this plan
      val templateList = rtimageList.filter(ri => Phase2Util.referencedPlanUID(ri).nonEmpty)

      // list of beams that are referenced by images
      val referencedBeamList = templateList.flatMap(ri => DicomUtil.findAllSingle(ri, TagByName.ReferencedBeamNumber)).flatMap(_.getIntegerValues).distinct.sorted

      // list of all beam numbers in plan
      val planBeamList = DicomUtil.findAllSingle(plan, TagByName.BeamNumber).flatMap(_.getIntegerValues).distinct.sorted

      val list: Seq[AttributeList] = {
        if (referencedBeamList == planBeamList)
          templateList // the user uploaded template a image for each beam in the plan.
        else {
          // Get a previously processed series (that references this RTPLAN) from the database. This might be empty.
          val dbRtimageList: Seq[DicomSeries] = DicomSeries.getByReferencedRtplanUID(anonymizedRtplanSop(institutionPK, plan), modality = "RTIMAGE", limit = 1)

          /**
           * Get the beam number of the given RTIMAGE.
           *
           * @param rtimage For this image.
           * @return Beam number, if found.
           */
          def beamNumberOfRtimage(rtimage: AttributeList): Option[Int] = {
            DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber).flatMap(_.getIntegerValues).headOption
          }

          // list of beam numbers from template-eligible files that were uploaded by the user
          val userBeamNumberSet = templateList.flatMap(beamNumberOfRtimage).toSet

          /**
           * Return true if the image both has a ReferencedBeamNumber defined and that beam number is not in the template set uploaded by the user.
           *
           * @param rtimage For this image.
           * @return
           */
          def hasBeamButNotInSet(rtimage: AttributeList): Boolean = {
            val num = beamNumberOfRtimage(rtimage)
            num.isDefined && (!userBeamNumberSet.contains(num.get))
          }

          // List of images from DB that have a beam number defined (should always be the case anyway) and whose beam number is not in the set uploaded by the user.
          val dbList = dbRtimageList.flatMap(_.attributeListList).filter(hasBeamButNotInSet)

          templateList ++ dbList
        }
      }

      def referencedBeamNumber(rtimage: AttributeList): Int = DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber).flatMap(_.getIntegerValues).head

      list.sortBy(referencedBeamNumber)
    }
  }

  /**
   * Make a set of required files for making a new series.  If valid, return the required files, otherwise return an error message.
   *
   * @param alList List of all DICOM files.
   * @return Requirements or error message.
   */
  def makeRequirements(institutionPK: Long, alList: Seq[DicomFile]): Either[String, SeriesMakerReq] = {

    val rtimageList = extractDistinctRtimageList(alList)

    val rtplan = rtplanToUse(institutionPK, alList)

    val machine: Either[String, Machine] = {
      if (rtplan.isRight)
        machineOf(institutionPK, rtplan.right.get, rtimageList)
      else
        Left("No RTPLAN")
    }

    def templateList = getTemplateList(institutionPK, rtplan, rtimageList.map(_.al))

    0 match {
      case _ if rtplan.isLeft =>
        Left(rtplan.left.get)
      case _ if rtimageList.isEmpty =>
        Left("No RTIMAGE files were uploaded.")
      case _ if machine.isLeft =>
        Left(machine.left.get)
      case _ if templateList.isEmpty =>
        Left("Could not find any RTIMAGES to use as templates for making beams.")
      case _ =>
        Right(SeriesMakerReq(rtplan.right.get, rtimageList, machine.right.get, templateList))
    }
  }
}
