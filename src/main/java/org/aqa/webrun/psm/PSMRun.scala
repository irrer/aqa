package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
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
import org.aqa.Config.PSMWholeDetectorBeamNamePattern
import org.aqa.web.WebUtil.emptyValueMap
import org.restlet.Response

import java.sql.Timestamp
import java.util.Date
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

  private def getPlanBeamNumberList(rtplan: AttributeList): Seq[Int] = DicomUtil.findAllTag(rtplan, TagByName.BeamNumber).map(_.getIntegerValues.head).sorted

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
    new PSMExecute(extendedData, runReq)
    ProcedureStatus.done
  }

  private def getRtplan(rtplanList: Seq[AttributeList], planUIDReference: String): Option[AttributeList] = {
    if (Config.ProductionMode || rtplanList.isEmpty) { // Either this is ProductionMode, or is TestMode and the user did not upload a plan.
      val matchingUploaded = rtplanList.filter(plan => planUIDReference.contains(Util.sopOfAl(plan)))

      def dbPlan: Seq[AttributeList] = DicomSeries.getBySopInstanceUID(planUIDReference).map(_.attributeListList.head)

      val list = matchingUploaded ++ dbPlan
      list.headOption
    } else {
      rtplanList.headOption
    }
  }

  /**
    * Determine of all the beams referenced in the plan have been uploaded.  If so, return None, else return an error message describing what is missing.
    * @param rtplan For this RTPLAN.
    * @param rtimageList List of RTIMAGE files uploaded by user.
    * @return None if ok, error message if beams are missing.
    */
  private def allBeamsPresent(rtplan: AttributeList, rtimageList: Seq[AttributeList]): Option[String] = {
    val planBeamNumberList = getPlanBeamNumberList(rtplan)
    val rtimageBeamNumberList = rtimageList.flatMap(rtimage => DicomUtil.findAllTag(rtimage, TagByName.ReferencedBeamNumber)).map(_.getIntegerValues.head).sorted

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
    * Get the flood field to be used with this data set.  If there is more than one that qualify, then the
    * most recent one will be used.  To qualify, the flood field must:
    *   - have been delivered before the PSM was delivered
    *   - have matching image resolution, energy, and FFF mode.
    *   - be from the same machine
    *
    * @param alList List of all uploaded DICOM files.
    * @param rtimageList List of all RTIMAGE files except for flood field.
    * @return
    */
  private def getFloodField(rtplan: AttributeList, alList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Option[FloodField] = {

    val machinePK = {
      val anonAttr = rtimageList.head.get(TagByName.DeviceSerialNumber)
      val anonDeviceSerialNumber = anonAttr.getSingleStringValueOrEmptyString
      val machineList = Machine.findMachinesBySerialNumber(anonDeviceSerialNumber)
      machineList.head.machinePK.get
    }

    def getInt(tag: AttributeTag): Int = rtimageList.head.get(tag).getIntegerValues.head

    val Columns = getInt(TagByName.Columns)
    val Rows = getInt(TagByName.Rows)
    val ImagePlanePixelSpacing = rtimageList.head.get(TagByName.ImagePlanePixelSpacing).getDoubleValues

    val kvp = DicomUtil.findAllTag(rtimageList.head, TagByName.KVP).head.getDoubleValues.head

    val fff = {
      val beam = DicomUtil.getBeamOfRtimage(rtplan, rtimageList.head).get

      val fluenceModeList = DicomUtil.findAllTag(beam, TagByName.FluenceMode).map(_.getSingleStringValueOrEmptyString).filter(_.equalsIgnoreCase("FFF")).flatten
      val isFFF = fluenceModeList.nonEmpty
      isFFF
    }

    /** Try to find a qualifying flood field. */
    val floodField: Option[FloodField] = {
      // must be older than this data set
      val maxDate = getDataDate(valueMap = emptyValueMap, alList = alList, xmlList = Seq()).get

      // can not be too old
      val minDate = new Timestamp(maxDate.getTime - Config.PSMMaxFloodFieldAge_ms)

      FloodField
        .getMatching( //
          machinePK = machinePK,
          Rows = Rows,
          Columns = Columns,
          ImagePlanePixelSpacingX = ImagePlanePixelSpacing.head,
          ImagePlanePixelSpacingY = ImagePlanePixelSpacing(1),
          kvp = kvp,
          fff = fff,
          minDate = minDate,
          maxDate = maxDate
        )
        .lastOption
    }

    floodField
  }

  private def isWholeDetectorBeamName(beamName: String): Boolean = {
    beamName.toLowerCase.matches(PSMWholeDetectorBeamNamePattern)
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
      None // TODO rm
    }

    def getWholeDetector: Option[AttributeList] = {
      try {
        val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get
        rtimageList.find(rtimage => isWholeDetectorBeamName(Util.getBeamNameOfRtimage(rtplan, rtimage).get))
      } catch {
        case _: Throwable => None
      }
    }

    val rtplanOpt = getRtplan(rtplanList, planUIDReferenceList.head)

    val result = 0 match {
      case _ if alList.isEmpty                                            => formError("No DICOM files were uploaded.  There should be exactly one.")
      case _ if planUIDReferenceList.isEmpty                              => formError("RTIMAGES do not reference an RTPLAN")
      case _ if planUIDReferenceList.size > 1                             => formError("RTIMAGES reference more than one RTPLAN")
      case _ if referencedSeriesList.size > 1                             => formError("RTIMAGES are from more than one series")
      case _ if rtplanOpt.isEmpty                                         => formError("Could not get RTPLAN.  Upload the RTPLAN with the RTIMAGE files.")
      case _ if allBeams.nonEmpty                                         => formError(allBeams.get)
      case _ if getFloodField(rtplanOpt.get, alList, rtimageList).isEmpty => formError("Could not find compatible flood field.  Try running the 'FloodField' procedure with the latest flood field.")
      case _ if getWholeDetector.isEmpty                                  => formError("Can not find whole detector image.")
      case _ =>
        val rtplan = getRtplan(rtplanList, planUIDReferenceList.head).get
        val planBeamNumberSet = getPlanBeamNumberList(rtplan).toSet
        val wholeDetector = getWholeDetector.get
        val imgList = {
          val list0 = rtimageList.filterNot(FloodUtil.isFloodField)
          val list1 = list0.filter(rtimage => planBeamNumberSet.contains(beamNumberOf(rtimage).get))
          val list2 = Util.sortByDateTime(list1)
          val list3 = list2.filterNot(rtimage => Util.sopOfAl(rtimage).equals(Util.sopOfAl(wholeDetector)))
          list3
        }
        val floodField = getFloodField(rtplan, alList, rtimageList).get
        val runReq = PSMRunReq(rtplan = rtplan, wholeDetector = wholeDetector, rtimageList = imgList, floodField = floodField)
        Right(runReq)
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
    val floodField = getFloodField(rtplan, alList, rtimageList).get

    val runReq = PSMRunReq(rtplan = rtplan, wholeDetector = wholeDetector, rtimageList = imgList, floodField = floodField)
    runReq
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    getRtimageList(alList).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {

    val min: Date = getRtimageList(alList) // all RTIMAGE files
      .filterNot(FloodUtil.isFloodField) // ignore any flood field that may have been uploaded
      .flatMap(d => Util.extractDateTimeAndPatientIdFromDicomAl(d)._1.headOption) // get the date+time from each DICOM files
      .min // use the earliest date+time
    Some(new Timestamp(min.getTime))
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val rtimageList = getRtimageList(alList)
    val list = getMachineDeviceSerialNumberListFromRtimageUtil(rtimageList, xmlList)
    list
  }

}
