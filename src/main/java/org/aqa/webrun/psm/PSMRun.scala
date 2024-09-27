package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.aqa.Util
import org.aqa.webrun.floodField.FloodUtil
import org.aqa.Config
import org.aqa.db.DicomSeries
import org.aqa.db.FloodField
import org.aqa.db.Machine
import org.aqa.web.WebUtil
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

class PSMRun(procedure: Procedure) extends WebRunProcedure with RunTrait[PSMRunReq] {

  private def beamNumberOf(rtimage: AttributeList): Option[Int] = {
    val attr = rtimage.get(TagByName.ReferencedBeamNumber)
    val beamNumber =
      if ((attr != null) && (attr.getIntegerValues.length == 1))
        Some(attr.getIntegerValues.head)
      else
        None

    beamNumber
  }

  private def getPlanBeamNumberList(rtplan: AttributeList): Seq[Int] = DicomUtil.findAllSingle(rtplan, TagByName.BeamNumber).map(_.getIntegerValues.head).sorted

  /**
    * Get the list of RTIMAGE files.  They must:
    *
    * <ul>
    *   <li>be RTIMAGE modality</li>
    *   <li>not be a flood field</li>
    *   <li>must each reference a different beam</li>
    * </ul>
    *
    * @param alList List of all DICOM uploaded files.
    * @return List of conventionally delivered RTIMAGE files.
    */
  private def getRtimageList(alList: Seq[AttributeList]) = {
    // get RTIMAGE files that are not flood fields
    val list1 = Util.sortByDateTime(alList.filter(Util.isRtimage).filterNot(FloodUtil.isFloodField))

    // sort by date+time
    val list2 = Util.sortByDateTime(list1)

    // If there are multiple RTIMAGE files that reference the same beam, then take the most recently captured one. The
    // last one is usually the right one because the user made a mistake delivering the earlier one(s).  This also is
    // a simple thing to tell the user, and it is something that they can control.
    val list3 = list2.groupBy(beamNumberOf).map(beamAl => Util.sortByDateTime(beamAl._2).last)

    list3.toSeq
  }

  override def run(extendedData: ExtendedData, runReq: PSMRunReq, response: Response): ProcedureStatus.Value = {
    Trace.trace()
    new PSMExecute(extendedData, runReq, response)
    Trace.trace()
    ProcedureStatus.done
  }

  private def getRtplan(rtplanList: Seq[AttributeList], planUIDReference: String): Option[AttributeList] = {
    if (Config.ProductionMode || rtplanList.isEmpty) { // Either this is is ProductionMode, or is TestMode and the user did not upload a plan.
      val matchingUploaded = rtplanList.filter(plan => planUIDReference.contains(Util.sopOfAl(plan)))

      def dbPlan: Seq[AttributeList] = DicomSeries.getBySopInstanceUID(planUIDReference).map(_.attributeListList.head)

      val list = matchingUploaded ++ dbPlan
      list.headOption
    } else {
      rtplanList.headOption
    }
  }

  /**
    * Determine of all of the beams referenced in the plan have been uploaded.  If so, return None, else return an error message describing what is missing.
    * @param rtplan For this RTPLAN.
    * @param rtimageList List of RTIMAGE files uploaded by user.
    * @return None if ok, error message if beams are missing.
    */
  private def allBeamsPresent(rtplan: AttributeList, rtimageList: Seq[AttributeList]): Option[String] = {
    val planBeamNumberList = getPlanBeamNumberList(rtplan)
    val rtimageBeamNumberList = rtimageList.flatMap(rtimage => DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber)).map(_.getIntegerValues.head).sorted

    val missingBeamNumberList = planBeamNumberList.diff(rtimageBeamNumberList)

    if (missingBeamNumberList.isEmpty)
      None
    else {
      def nameOf(BeamNumber: Int): Option[String] = {
        val beamAl = Util.getBeamOfRtimage(rtplan, BeamNumber)
        if (beamAl.isDefined) {
          val name = beamAl.get.get(TagByName.BeamName).getSingleStringValueOrEmptyString
          if (name.nonEmpty)
            Some(name)
          else
            None
        } else
          None
      }

      val missingBeamNameList = missingBeamNumberList.flatMap(nameOf)

      val nl = WebUtil.titleNewline

      Some(s"""There are ${missingBeamNameList.size} beam(s) missing:    $nl ${missingBeamNameList.mkString("    " + nl)}""")
    }
  }

  /**
    * Get the flood field to be used with this data set.  Use the latest one, whether it was uploaded by the user, or, was in the database.
    * @param alList List of all uploaded DICOM files.
    * @param rtimageList List of all RTIMAGE files except for flood field.
    * @return
    */
  private def getFloodField(alList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Option[AttributeList] = {

    val machinePK = {
      val anonAttr = rtimageList.head.get(TagByName.DeviceSerialNumber)
      val anonDeviceSerialNumber = anonAttr.getSingleStringValueOrEmptyString
      val machineList = Machine.findMachinesBySerialNumber(anonDeviceSerialNumber)
      machineList.head.machinePK.get
    }

    def getInt(tag: AttributeTag): Int = rtimageList.head.get(tag).getIntegerValues.head

    val Columns = getInt(TagByName.Columns)
    val Rows = getInt(TagByName.Rows)

    def matchingResolution(al: AttributeList): Boolean = {
      val col = al.get(TagByName.Columns).getIntegerValues.head
      val row = al.get(TagByName.Rows).getIntegerValues.head
      (col == Columns) && (row == Rows)
    }

    def timeOf(al: AttributeList) = Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

    val floodField = {
      val uploaded = alList.filter(FloodUtil.isFloodField).filter(matchingResolution)
      val list: Seq[AttributeList] = FloodField.getMostRecent(machinePK, Rows, Columns) match {
        case Some(ff) => uploaded :+ ff.dicom
        case _        => uploaded
      }
      list.sortBy(timeOf).headOption
    }

    floodField
  }

  private def isWholeDetectorBeamName(beamName: String): Boolean = {
    beamName.toLowerCase.contains("whole") // TODO put string in configuration
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    val rtplanList = alList.filter(Util.isRtplan)
    val rtimageList = getRtimageList(alList)

    logger.info(s"Number of RTPLAN files uploaded: ${rtplanList.size}    Number of RTIMAGE files: ${rtimageList.size}")

    def referencedSeriesList = rtimageList.map(Util.serInstOfAl).distinct
    val planUIDReferenceList: Seq[String] = rtimageList.flatMap(Util.getRtplanSop).distinct

    def allBeams: Option[String] = {
      val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get
      allBeamsPresent(rtplan, rtimageList)
    }

    def getWholeDetector: Option[AttributeList] = {
      try {
        val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get
        alList.find(rtimage => isWholeDetectorBeamName(Util.getBeamNameOfRtimage(rtplan, rtimage).get))
      } catch {
        case _: Throwable => None
      }

    }

    val result = 0 match {
      case _ if alList.isEmpty                                           => formError("No DICOM files were uploaded.  There should be exactly one.")
      case _ if planUIDReferenceList.isEmpty                             => formError("RTIMAGES do not reference an RTPLAN")
      case _ if planUIDReferenceList.size > 1                            => formError("RTIMAGES reference more than one RTPLAN")
      case _ if referencedSeriesList.size > 1                            => formError("RTIMAGES are from more than one series")
      case _ if getRtplan(rtplanList, planUIDReferenceList.head).isEmpty => formError("Could not get RTPLAN.  Upload the RTPLAN with the RTIMAGE files.")
      case _ if allBeams.nonEmpty                                        => formError(allBeams.get)
      case _ if getFloodField(alList, rtimageList).isEmpty               => formError("Could not find compatible flood field.  Try adding the latest flood field to this upload set.")
      case _ if getWholeDetector.isEmpty                                 => formError("Can not find whole detector image.")
      case _ =>
        val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get
        val planBeamNumberSet = getPlanBeamNumberList(rtplan).toSet
        val wholeDetector = getWholeDetector.get
        val imgList = {
          val list1 = rtimageList.filter(rtimage => planBeamNumberSet.contains(beamNumberOf(rtimage).get))
          val list2 = Util.sortByDateTime(list1)
          val list3 = list2.filterNot(rtimage => Util.sopOfAl(rtimage).equals(Util.sopOfAl(wholeDetector)))
          list3
        }
        val floodField = getFloodField(alList, rtimageList).get
        val runReq = PSMRunReq(rtplan = rtplan, wholeDetector = wholeDetector, rtimageList = imgList, floodField = floodField)
        Right(runReq)
    }

    if (result.isLeft) {
      val j = result.left.get.values.head.toString
      Trace.trace(s"formError: $result :: $j")
    }

    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = {
    val rtplanList = alList.filter(Util.isRtplan)
    val rtimageList = getRtimageList(alList)
    val planUIDReferenceList: Seq[String] = rtimageList.flatMap(Util.getRtplanSop).distinct

    val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get

    def getWholeDetector: AttributeList = rtimageList.find(rtimage => isWholeDetectorBeamName(Util.getBeamNameOfRtimage(rtplan, rtimage).get)).get

    val planBeamNumberSet = getPlanBeamNumberList(rtplan).toSet
    val wholeDetector = getWholeDetector
    val imgList = {
      val list1 = rtimageList.filter(rtimage => planBeamNumberSet.contains(beamNumberOf(rtimage).get))
      val list2 = Util.sortByDateTime(list1)
      val list3 = list2.filterNot(rtimage => Util.sopOfAl(rtimage).equals(Util.sopOfAl(wholeDetector)))
      list3
    }
    val floodField = getFloodField(alList, rtimageList).get

    val runReq = PSMRunReq(rtplan = rtplan, wholeDetector = wholeDetector, rtimageList = imgList, floodField = floodField)
    runReq
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    getRtimageList(alList).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val min = getRtimageList(alList).map(Util.extractDateTimeAndPatientIdFromDicomAl).flatMap(_._1.headOption).min
    Some(new Timestamp(min.getTime))
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val rtimageList = getRtimageList(alList)
    val list = getMachineDeviceSerialNumberListFromRtimageUtil(rtimageList, xmlList)
    list
  }

}
