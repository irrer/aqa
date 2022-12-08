/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.web

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
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
import scala.xml.XML

object GetSeries extends Logging {
  private val sync = "sync"

  /**
    * Local cache for series to reduce the processing load and improve response times.
    *
    * Key: institutionPK:patientID
    * Value: XML text
    */
  private val cache = scala.collection.mutable.HashMap[String, String]()

  /**
    * Build a hash-worthy key from the institution PK and the patient ID so
    * that the key is guaranteed to be unique across institutions.
    *
    * @param institutionPK Identifies institution.
    * @param realPatientId real (non-anonymized) patient ID.
    * @return
    */
  private def makeKey(institutionPK: Long, realPatientId: String): String = {
    institutionPK + ":" + realPatientId
  }

  private def get(institutionPK: Long, realPatientId: String): Option[String] =
    cache.synchronized {
      cache.get(makeKey(institutionPK, realPatientId))
    }

  private def put(institutionPK: Long, realPatientId: String, xmlText: String): Option[String] =
    cache.synchronized {
      cache.put(makeKey(institutionPK, realPatientId), xmlText)
    }

  /**
    * Remove the given entry from the cache.  This is how cache entries are invalidated.  The
    * contents will be re-generated next time it is fetched.  If the cache entry to be removed
    * is not in the cache, then no action will be taken.
    *
    * @param userPK Identifies institution via user.
    * @param anonymizedPatientId real (non-anonymized) patient ID.
    */
  def remove(userPK: Long, anonymizedPatientId: Option[String]): Unit = {
    try {
      val institutionPK = User.get(userPK).get.institutionPK
      // jump through hoops to get the real patient ID using the user ID and anonymizedPatientId
      val realPatientId = {
        val at = AttributeFactory.newAttribute(TagByName.PatientID)
        at.addValue(anonymizedPatientId.get)
        val al = new AttributeList
        al.put(at)
        val realAlList = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(al))
        val rpi = realAlList.head.get(TagByName.PatientID).getSingleStringValueOrNull
        rpi
      }
      cache.synchronized {
        cache.remove(makeKey(institutionPK, realPatientId))
        logger.info("Removed " + realPatientId + " from GetSeries cache.")
      }
    } catch {
      case t: Throwable => logger.warn("Unable to remove entry from cache.  userPK: " + userPK + "    anonymizedPatientId: " + anonymizedPatientId + " : " + fmtEx(t))
    }
  }

  /**
    * Remove all entries in the given institution.
    *
    * @param institutionPK Institution of interest.
    */
  def remove(institutionPK: Long): Unit = {
    cache.synchronized {
      val prefix = institutionPK + ":"
      val list = cache.keys.filter(_.startsWith(prefix))
      list.map(cache.remove)
    }
  }
}

/**
  * Generate XML that lists DICOM series associated with the given patient ID.
  */
class GetSeries extends Restlet with SubUrlRoot with Logging {

  private val PatientIDTag = "PatientID"

  private def generateXml(institutionPK: Long, realPatientId: String): Elem = {
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

      def makeSopList(): Option[Elem] = {
        val realUidList = dicomSeries.sopUidSeq.flatMap(uid => lookup(TagByName.SOPInstanceUID, uid))
        val elem = <SOPInstanceUIDList>
            {realUidList.map(uid => <SOPInstanceUID>{uid.trim}</SOPInstanceUID>)}
          </SOPInstanceUIDList>
        Some(elem)
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
      val sopList = makeSopList()

      val startDate = {
        if (output.isDefined) Some(<AnalysisStartDate>{Util.standardDateFormat.format(output.get.startDate)}</AnalysisStartDate>)
        else None
      }

      /**
        * Get the procedure associated with this series.  First try getting it via the
        * DicomSeries.procedurePK field, and if that fails, get it from the output it
        * is associated with.
        *
        * Ideally the DicomSeries.procedurePK should always work, but the case for the database not being
        * transitioned to the required state must be handled.
        */
      val procedurePK: Option[Long] = {

        def getFromOutput = if (output.isDefined) Some(output.get.procedurePK) else None

        0 match {
          case _ if dicomSeries.procedurePK.isEmpty                                              => getFromOutput
          case _ if !procedureList.exists(p => p.procedurePK.get == dicomSeries.procedurePK.get) => getFromOutput
          case _                                                                                 => Some(dicomSeries.procedurePK.get)
        }
      }

      val procedure = {
        if (procedurePK.isDefined) Some(<Procedure>{nameOfProcedure(procedurePK.get)}</Procedure>)
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

      val elemList: Seq[Elem] = Seq(serInstUid, devSerNo, sopList, frmOfRef, mappedFrameOfReferenceUID, modality, sopClassUid, patId, referencedRtplanUID, url, procedure, status, startDate).flatten

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

  /**
    * Get the series list for the given patient.  First try getting it from cache, but if that
    * fails, generate it and put the new content in cache.
    *
    * @param institutionPK Identify institution.
    * @param realPatientId Real (non-anonymized) patient ID.
    * @return XML representation of all series processed for the given patient ID.
    */
  private def getXml(institutionPK: Long, realPatientId: String): String = {
    GetSeries.get(institutionPK, realPatientId) match {
      case Some(text) => text
      case _          =>
        // only allow one request at a time to avoid overloading the server
        GetSeries.sync.synchronized {
          logger.info("Generating the list of series for PatientID " + realPatientId)
          val start = System.currentTimeMillis
          val xml = generateXml(institutionPK, realPatientId)
          val text = Util.prettyPrint(xml)
          GetSeries.put(institutionPK, realPatientId, text)
          val elapsed = System.currentTimeMillis - start
          logger.info("Generated series list for " + realPatientId + "    text length: " + text.length + "    Elapsed ms: " + elapsed + "\n" + text)
          text
        }
    }
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
        case _ =>
          val xmlText = getXml(user.get.institutionPK, realPatientId.get)
          response.setEntity(xmlText, MediaType.TEXT_XML)
          logger.info("Got list of series for PatientID " + realPatientId.get + "    text length: " + xmlText.length)
      }

    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }

}
