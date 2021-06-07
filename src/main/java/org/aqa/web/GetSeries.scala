package org.aqa.web

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import org.aqa.AnonymizeUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomAnonymous
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.io.File
import scala.xml.Elem
import scala.xml.PrettyPrinter
import scala.xml.XML

object GetSeries {
  private val sync = "sync"
}

/**
  * Generate XML that lists DICOM series associated with the given patient ID.
  */
class GetSeries extends Restlet with SubUrlRoot with Logging {

  private val PatientIDTag = "PatientID"

  private def generateXml(user: User, realPatientId: String): Elem = {
    val institutionPK = user.institutionPK
    val attribute = AttributeFactory.newAttribute(TagFromName.PatientID)
    attribute.addValue(realPatientId)

    val anonPatientId = {
      val patIdDicomAnonList = AnonymizeUtil.makeDicomAnonymousList(institutionPK, Seq(attribute))
      patIdDicomAnonList.head.value // anonymized patient ID
    }
    val dicomSeriesList: Seq[DicomSeries.DicomSeriesWithoutContent] = DicomSeries.getByPatientID(anonPatientId).sortBy(ds => ds.date.getTime)

    val relatedOutputList = Output.getByInputPKSet(dicomSeriesList.flatMap(ds => ds.inputPK).toSet)

    def outputOfDicomSeries(dicomSeries: DicomSeries.DicomSeriesWithoutContent): Option[Output] = {
      if (dicomSeries.inputPK.isDefined)
        relatedOutputList.find(o => o.inputPK == dicomSeries.inputPK.get)
      else
        None
    }

    def urlOfDicomSeries(dicomSeries: DicomSeries.DicomSeriesWithoutContent): Option[String] = {
      outputOfDicomSeries(dicomSeries) match {
        case Some(output) =>
          val file = new File(output.dir, Output.displayFilePrefix + ".html")
          Some(WebServer.urlOfResultsFile(file))
        case _ => None
      }
    }

    val tagList = Seq(TagFromName.SeriesInstanceUID, TagFromName.FrameOfReferenceUID, TagFromName.PatientID, TagFromName.DeviceSerialNumber)

    val dicomAnonList = DicomAnonymous.getAttributesByTag(institutionPK, tagList)

    def daKey(institutionPK: Long, attributeTag: String, value: String): String = institutionPK + " " + attributeTag + " " + value

    val daList = dicomAnonList.map(da => (daKey(da.institutionPK, da.attributeTag, da.value), da)).toMap

    /**
      * Given a tag and anonymized value, return the non-anonymized value.
      */
    def lookup(tag: AttributeTag, anonValue: String): Option[String] = {

      daList.get(daKey(institutionPK, DicomAnonymous.formatAnonAttributeTag(tag), anonValue)) match {
        case Some(dicomAnonymous) => Some(AnonymizeUtil.decryptWithNonce(institutionPK, dicomAnonymous.value_real))
        case _                    => None
      }
    }

    // get list of all procedures once
    val procedureList = Procedure.list

    /**
      * Lookup the name+version of a procedure
      */
    def nameOfProcedure(procedurePK: Long) = {
      procedureList.find(p => p.procedurePK.get == procedurePK) match {
        case Some(proc) => proc.name + " " + proc.version
        case _          => "unknown"
      }
    }

    def toXml(dicomSeries: DicomSeries.DicomSeriesWithoutContent): Elem = {

      def getOpt(textOpt: Option[String], tag: AttributeTag, xmlTag: String): Option[Elem] = {

        textOpt match {
          case Some(text) =>
            lookup(tag, text) match {
              case Some(text) =>
                val xml = "<" + xmlTag + ">" + edu.umro.util.XML.escapeSpecialChars(text) + "</" + xmlTag + ">"
                Some(XML.loadString(xml))
              case _ => None
            }
          case _ => None
        }
      }

      val output = outputOfDicomSeries(dicomSeries)

      val serInstUid = getOpt(Some(dicomSeries.seriesInstanceUID), TagFromName.SeriesInstanceUID, "SeriesInstanceUID")
      val frmOfRef = getOpt(dicomSeries.frameOfReferenceUID, TagFromName.FrameOfReferenceUID, "FrameOfReferenceUID")
      val mappedFrameOfReferenceUID = getOpt(dicomSeries.mappedFrameOfReferenceUID, TagFromName.FrameOfReferenceUID, "mappedFrameOfReferenceUID")
      val modality = Some(<Modality>{dicomSeries.modality}</Modality>)
      val sopClassUid = Some(<SOPClassUID>{dicomSeries.sopClassUID}</SOPClassUID>)
      val devSerNo = getOpt(dicomSeries.deviceSerialNumber, TagFromName.DeviceSerialNumber, "DeviceSerialNumber")
      val patId = getOpt(dicomSeries.patientID, TagFromName.PatientID, "PatientID")
      val referencedRtplanUID = getOpt(dicomSeries.referencedRtplanUID, TagFromName.ReferencedSOPInstanceUID, "referencedRtplanUID")
      val startDate = {
        if (output.isDefined) Some(<AnalysisStartDate>{Util.standardDateFormat.format(output.get.startDate)}</AnalysisStartDate>)
        else None
      }
      val procedure = {
        if (output.isDefined) Some(<Procedure>{nameOfProcedure(output.get.procedurePK)}</Procedure>)
        else None
      }
      val status = {
        if (output.isDefined) Some(<Status>{output.get.status}</Status>)
        else None
      }
      val url = {
        urlOfDicomSeries(dicomSeries) match {
          case Some(u) => Some(<URL>{u}</URL>)
          case _       => None
        }
      }

      val elemList: Seq[Elem] = Seq(serInstUid, devSerNo, frmOfRef, mappedFrameOfReferenceUID, modality, sopClassUid, patId, referencedRtplanUID, url, procedure, status, startDate).flatten

      val seriesXml = {
        <Series>
          {elemList}
          <DataDate>{Util.standardDateFormat.format(dicomSeries.date)}</DataDate>
          <NumberOfSlices>{dicomSeries.size}</NumberOfSlices>
        </Series>
      }

      seriesXml
    }

    val seriesListXml = {
      <SeriesList>
        {
        val groupSize = Math.min(5, dicomSeriesList.size / 8)
        val partitioned = edu.umro.ScalaUtil.Util.sizedGroups(dicomSeriesList, groupSize)
        partitioned.par.map(sub => sub.map(ds => toXml(ds))).toList.flatten
      }
      </SeriesList>
    }

    val patId = {
      val unknown = "unknown"
      if (dicomSeriesList.nonEmpty && dicomSeriesList.head.patientID.isDefined) {
        val pi = lookup(TagFromName.PatientID, dicomSeriesList.head.patientID.get)
        if (pi.isDefined) pi.get else unknown
      } else unknown
    }
    logger.info("Generated XML list of " + dicomSeriesList.size + " series for patient " + patId)
    seriesListXml
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(request)
      val realPatientId = valueMap.get(PatientIDTag)

      0 match {
        case _ if user.isEmpty          => badRequest(response, "User not logged in or user can not be identified", Status.CLIENT_ERROR_BAD_REQUEST)
        case _ if realPatientId.isEmpty => badRequest(response, "No " + PatientIDTag + " value given", Status.CLIENT_ERROR_BAD_REQUEST)
        case _                          =>
          // only allow one request at a time to avoid overloading the server
          GetSeries.sync.synchronized {
            logger.info("Getting list of series for PatientID " + realPatientId.get)
            val xml = generateXml(user.get, realPatientId.get)
            val xmlText = new PrettyPrinter(1024, 2).format(xml)
            response.setEntity(xmlText, MediaType.TEXT_XML)
            logger.info("Got list of series for PatientID " + realPatientId.get)
          }
      }

    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }

}
