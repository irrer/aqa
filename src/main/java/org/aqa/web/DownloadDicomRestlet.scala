package org.aqa.web

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Logging
import org.aqa.approval.ApprovalChangeRestlet
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.web.WebUtil.getUser
import org.aqa.AnonymizeUtil
import org.aqa.db.Procedure
import org.aqa.web.WebUtil.userIsWhitelisted
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import scala.xml.Elem

object DownloadDicomRestlet extends Logging {
  val outputPKTag: String = "outputPK"
  private val path = new String((new DownloadDicomRestlet).pathOf)

  def makeReference(outputPK: Long): Elem = {
    val url = s"$path?outputPK=$outputPK"
    <a href={url}>Download DICOM</a>
  }
}

// TODO: handle MachineLog XML files

/** Allow user to download DICOM files.  If the user is from the same institution, they will be de-anonymized.  */
class DownloadDicomRestlet extends Restlet with SubUrlRoot with Logging {

  /**
    * Get all the attribute lists for this input, including the RTPLAN (if available).
    * @param inputPK For this input.
    * @return List of attribute lists.
    */
  private def getAl(inputPK: Long): Seq[AttributeList] = {
    val seriesList = DicomSeries.getByInputPK(inputPK)

    val seriesAlList = seriesList.flatMap(_.attributeListList)

    val planUIDList = seriesList.flatMap(_.referencedRtplanUID).distinct

    val planSeriesList = planUIDList.flatMap(DicomSeries.getBySopInstanceUID)

    val planAlList = planSeriesList.flatMap(_.attributeListList).filter(planAl => planUIDList.contains(Util.sopOfAl(planAl)))

    val allSeries = seriesAlList ++ planAlList
    allSeries
  }

  private def dateOf(al: AttributeList): Long = {
    val dateTimeTags = Seq(
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.SeriesDate, TagByName.SeriesTime),
      (TagByName.RTPlanDate, TagByName.RTPlanTime),
      (TagByName.StudyDate, TagByName.StudyTime)
    )

    val first = dateTimeTags.flatMap(dt => DicomUtil.getTimeAndDate(al, dt._1, dt._2)).head
    first.getTime
  }

  private case class NamedAl(name: String, al: AttributeList) {}

  private def nameGroup(unsortedList: Seq[AttributeList]): Seq[NamedAl] = {
    val list = unsortedList.sortBy(dateOf)

    val len = (list.size + 1).toString.length
    val fmt = s"%0${len}d"

    def makeName(index: Int): String = {
      val modality = Util.modalityOfAl(list(index))
      val numberText = fmt.format(index + 1)
      s"$modality$numberText.dcm"
    }

    val namedList = list.indices.map(index => NamedAl(makeName(index), list(index)))
    namedList
  }

  private def nameAlList(alList: Seq[AttributeList]): Seq[NamedAl] = {

    val groupList = alList.groupBy(Util.modalityOfAl).values

    val namedList = groupList.flatMap(nameGroup)

    namedList.toSeq
  }

  /**
    * Write the DICOM as a zip file stored in a byte array.
    * @param alList List of DICOM.
    * @return Zipped content.
    */
  private def makeZip(alList: Seq[AttributeList]): Array[Byte] = {

    val namedList = nameAlList(alList)

    val toZipOutputStream = new FileUtil.ToZipOutputStream

    namedList.sortBy(_.name).foreach(namedAl => toZipOutputStream.writeDicom(namedAl.al, path = namedAl.name, "AQA"))

    val byteArray = toZipOutputStream.finish()
    byteArray
  }

  private def makeDownloadFileName(output: Output, machine: Machine, doDeAnonymize: Boolean): String = {
    val dateText = Util.formatDate(Util.standardDateFormat, output.dataDate.get)
    val machineText = {
      if (doDeAnonymize)
        machine.getRealId
      else
        machine.id
    }

    val procedureText = {
      val procedure = Procedure.get(output.procedurePK).get
      procedure.fullName
    }

    val fileName = {
      val t = s"$procedureText $machineText $dateText.zip"
      FileUtil.replaceInvalidFileNameCharacters(t, '_').replace(' ', '_')
    }

    fileName
  }

  override def handle(request: Request, response: Response): Unit = {

    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(ApprovalChangeRestlet.outputPKTag).toLong
      val output = Output.get(outputPK).get
      val user = getUser(valueMap).get
      val userInstitutionPK = user.institutionPK
      val machine = Machine.get(output.machinePK.get).get
      val dataInstitutionPK = machine.institutionPK

      val alList = getAl(output.inputPK)

      val doDeAnonymize: Boolean = userIsWhitelisted(valueMap) || (userInstitutionPK == dataInstitutionPK)

      val finalAlList = {
        if (doDeAnonymize) {
          AnonymizeUtil.deAnonymizeDicom(userInstitutionPK, alList)
        } else
          alList
      }

      val zipContent = makeZip(finalAlList)

      val entity = new ByteArrayRepresentation(zipContent, MediaType.APPLICATION_ZIP)
      response.setEntity(entity)
      response.setStatus(Status.SUCCESS_OK)
      WebUtil.setDownloadName(response, makeDownloadFileName(output, machine, doDeAnonymize))

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }

  }
}
