package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import org.aqa.db.EPID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.AQA
import org.aqa.Config
import edu.umro.util.OpSys
import org.aqa.db.Institution
import org.aqa.AnonymizeUtil
import org.aqa.db.Machine
import org.aqa.db.User
import com.pixelmed.dicom.TagFromName
import org.aqa.db.DicomAnonymous
import com.pixelmed.dicom.AttributeFactory
import org.aqa.db.DicomSeries
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.Attribute
import scala.xml.XML
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace

object GetSeries {}

/**
 * Generate XML that lists DICOM series associated with the given patient ID.
 */
class GetSeries extends Restlet with SubUrlRoot with Logging {

  private val PatientIDTag = "PatientID"

  private def generateXml(user: User, realPatientId: String): Elem = {
    val institutionPK = user.institutionPK
    val institutionKey = AnonymizeUtil.getInstitutionKey(institutionPK)
    val attribute = AttributeFactory.newAttribute(TagFromName.PatientID)
    attribute.addValue(realPatientId)

    val anonPatientId = {
      val patIdDicomAnonList = AnonymizeUtil.makeDicomAnonymousList(institutionPK, Seq(attribute))
      patIdDicomAnonList.head.value // anonymized patient ID
    }
    val dicomSeriesList = DicomSeries.getByPatientID(anonPatientId)

    //    def seriesToAttrList(series: DicomSeries): Seq[Attribute] = {
    //      def makeAttr(tag: AttributeTag, value: String) = {
    //        val attr = AttributeFactory.newAttribute(tag)
    //        attr.addValue(value)
    //        attr
    //      }
    //
    //      val pairList: Seq[(AttributeTag, Option[String])] = Seq(
    //        (TagFromName.SeriesInstanceUID, Some(series.seriesInstanceUID)),
    //        (TagFromName.FrameOfReferenceUID, series.frameOfReferenceUID),
    //        (TagFromName.DeviceSerialNumber, series.deviceSerialNumber),
    //        (TagFromName.PatientID, series.patientID))
    //
    //      val attrList = pairList.filter(p => p._2.isDefined).map(p => makeAttr(p._1, p._2.get))
    //      attrList
    //    }

    val tagList = Seq(TagFromName.SeriesInstanceUID, TagFromName.FrameOfReferenceUID, TagFromName.PatientID, TagFromName.DeviceSerialNumber)

    val dicomAnonList = DicomAnonymous.getAttributesByTag(institutionPK, tagList)

    Trace.trace

    def lookup(tag: AttributeTag, anonValue: String): Option[String] = {
      val tagText = DicomAnonymous.formatAnonAttributeTag(tag)
      dicomAnonList.find(an => an.attributeTag.equals(tagText) && anonValue.equals(an.value)) match {
        case Some(dicomAnonymous) => Some(AnonymizeUtil.decryptWithNonce(institutionPK, dicomAnonymous.value_real))
        case _ => None
      }
    }

    def toXml(dicomSeries: DicomSeries): Elem = {

      def getOpt(textOpt: Option[String], tag: AttributeTag): Option[Elem] = {
        val xmlTag = DicomUtil.dictionary.getNameFromTag(tag)
        textOpt match {
          case Some(text) => {
            lookup(tag, text) match {
              case Some(text) => Some(XML.loadString("<" + xmlTag + ">" + text + "</" + xmlTag + ">"))
              case _ => None
            }
          }
          case _ => None
        }
      }

      val serInstUid = getOpt(Some(dicomSeries.seriesInstanceUID), TagFromName.SeriesInstanceUID)
      val frmOfRef = getOpt(dicomSeries.frameOfReferenceUID, TagFromName.FrameOfReferenceUID)
      val modality = Some(<Modality>{ dicomSeries.modality }</Modality>)
      val sopClassUid = Some(<SOPClassUID>{ dicomSeries.sopClassUID }</SOPClassUID>)
      val devSerNo = getOpt(dicomSeries.deviceSerialNumber, TagFromName.DeviceSerialNumber)
      val patId = getOpt(dicomSeries.patientID, TagFromName.PatientID)

      val elemList = Seq(
        serInstUid,
        devSerNo,
        frmOfRef,
        modality,
        sopClassUid,
        patId).flatten

      val seriesXml = {
        <Series>
          { elemList }
          <dataDate>{ Util.standardDateFormat.format(dicomSeries.date) }</dataDate>
          <numberOfSlices>{ dicomSeries.size }</numberOfSlices>
        </Series>
      }

      seriesXml
    }

    val seriesListXml = {
      <SeriesList>
        { dicomSeriesList.map(dicomSeries => toXml(dicomSeries)) }
      </SeriesList>
    }
    seriesListXml
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(request)
      val realPatientId = valueMap.get(PatientIDTag)

      0 match {
        case _ if (user.isEmpty) => badRequest(response, "User not logged in or user can not be identified", Status.CLIENT_ERROR_BAD_REQUEST)
        case _ if (realPatientId.isEmpty) => badRequest(response, "No " + PatientIDTag + " value given", Status.CLIENT_ERROR_BAD_REQUEST)
        case _ => {
          val xml = generateXml(user.get, realPatientId.get)
          val xmlText = new PrettyPrinter(1024, 2).format(xml)
          response.setEntity(xmlText, MediaType.TEXT_XML)
        }
      }

    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }

}