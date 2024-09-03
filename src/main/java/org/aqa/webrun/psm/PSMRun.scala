package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
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
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

class PSMRun(procedure: Procedure) extends WebRunProcedure with RunTrait[PSMRunReq] {

  /**
    * True if the al is a flood field.  Must not reference a beam and all XRayImageReceptorTranslation values must be near zero.
    * @param al For this attribute list.
    * @return True if it's a flood field.
    */
  private def isFloodField(al: AttributeList): Boolean = {

    /** True if al references a beam. */
    def referencesBeam: Boolean = DicomUtil.findAllSingle(al, TagByName.ReferencedBeamNumber).nonEmpty

    /** True if there is an XRayImageReceptorTranslation and all of its values are near zero.  Criteria is that
      *  it be less than one.  Normal RTIMAGES have a large value around 500 or ~1000.
      */
    def XRayImageReceptorTranslationNearZero = {
      val attrList = DicomUtil.findAllSingle(al, TagByName.XRayImageReceptorTranslation)
      attrList.nonEmpty && attrList.flatMap(_.getDoubleValues).map(_.abs < 1).reduce(_ && _)
    }
    val is = (!referencesBeam) && XRayImageReceptorTranslationNearZero
    is
  }

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(Util.isRtimage).filterNot(isFloodField)

  private def validatePSM(rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList], floodFieldList: Seq[AttributeList]): Either[StyleMapT, PSMRunReq] = {
    ???
  }

  override def run(extendedData: ExtendedData, runReq: PSMRunReq, response: Response): ProcedureStatus.Value = ???

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    val rtplanList = alList.filter(Util.isRtplan)
    val rtimageList = getRtimageList(alList)
    val floodFieldList = alList.filter(Util.isRtimage).filter(isFloodField)
    val result = validatePSM(rtplanList, rtimageList, floodFieldList)
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = ???

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    getRtimageList(alList).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val min = getRtimageList(alList).map(Util.extractDateTimeAndPatientIdFromDicomAl).flatMap(_._1.headOption).min
    Some(new Timestamp(min.getTime))
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getMachineDeviceSerialNumberListFromRtimageUtil(alList, xmlList)

}
