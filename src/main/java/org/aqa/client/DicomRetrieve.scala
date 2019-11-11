package org.aqa.client

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import org.aqa.Logging

object DicomRetrieve extends Logging {

  /**
   * Build the C-FIND query.
   */
  private def find(modality: String, patientID: String): Seq[AttributeList] = {
    val query = {
      val q = new AttributeList

      def add(text: String, tag: AttributeTag) = {
        val a = AttributeFactory.newAttribute(tag)
        a.addValue(text)
        q.put(a)
      }
      add(modality, TagFromName.Modality)
      add(patientID, TagFromName.PatientID)
      q
    }

    val resultList = DicomCFind.cfind(
      callingAETitle = ClientConfig.DICOMClient.aeTitle,
      calledPacs = ClientConfig.DICOMSource,
      attributeList = query,
      queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
      limit = None,
      queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

    logger.info("C-FIND query PatientID: " + patientID + "    Modality: " + modality + "    number of results: " + resultList.size)

    resultList
  }

  /**
   * Return true if the series has already been processed.
   */
  private def processed(al: AttributeList): Boolean = {
    val SeriesInstanceUID = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrNull
    ProcessedSeries.get(SeriesInstanceUID).isDefined
  }

  private def getSeries(SeriesInstanceUID: String): Seq[AttributeList] = {
    ???
  }

  /**
   * Look for new files to process.
   */
  private def updatePatient(patientID: String) = {

    val planList = find("RTPLAN", patientID).filterNot(processed _)

    val planAlList = planList.map(plan => getSeries(plan.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrNull))
  }

  def update = {
    ClientConfig.PatientIDList.map(patientID => updatePatient(patientID))
  }

  def init = {
    update

  }
}