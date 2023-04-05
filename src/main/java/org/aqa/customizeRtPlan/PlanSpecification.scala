package org.aqa.customizeRtPlan

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil

/**
  * Encapsulate values specified by the user in the user interface.
  */
case class PlanSpecification(toleranceTable: String, patientID: String, patientName: String, machineName: String, planName: String) {

  /**
    *  Modify all attributes that get a user specified value
    */
  def setOverrides(rtplan: AttributeList): Seq[IndexedSeq[Unit]] = {

    /**
      * When customizing a plan, override this DICOM attribute with the given value.
      */
    case class PlanAttributeOverride(tag: AttributeTag, value: String) {
      override def toString: String = DicomUtil.dictionary.getNameFromTag(tag) + " : " + value
    }

    val overrideList = Seq(
      PlanAttributeOverride(TagByName.ToleranceTableLabel, toleranceTable),
      PlanAttributeOverride(TagFromName.PatientID, patientID),
      PlanAttributeOverride(TagFromName.PatientName, patientName),
      PlanAttributeOverride(TagByName.TreatmentMachineName, machineName),
      PlanAttributeOverride(TagFromName.RTPlanLabel, planName),
      PlanAttributeOverride(TagByName.TableTopVerticalPosition, 0.toString)
    )

    overrideList.map(ov => {
      DicomUtil
        .findAllSingle(rtplan, ov.tag)
        .map(at => {
          at.removeValues()
          at.addValue(ov.value)
        })
    })
  }
}
