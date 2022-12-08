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

package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.BBbyEPID
import org.aqa.run.CacheCSV
import org.aqa.web.ViewOutput

import java.sql.Timestamp

class DailyQACSVCacheEPID(hostRef: String, institutionPK: Long) extends CacheCSV {

  private def patientIdOf(dataSet: BBbyEPID.DailyDataSetEPID): String = {
    val anonPatId = dataSet.al.get(TagByName.PatientID).getSingleStringValueOrEmptyString
    anonPatId
  }

  private def OperatorsName(dataSet: BBbyEPID.DailyDataSetEPID): String = {
    val anonPatId = dataSet.al.get(TagByName.OperatorsName).getSingleStringValueOrEmptyString
    anonPatId
  }

  private def textFromAl(dataSet: BBbyEPID.DailyDataSetEPID, tag: AttributeTag): String = {
    val attr = dataSet.al.get(tag)
    if (attr == null) "NA"
    else attr.getSingleStringValueOrEmptyString
  }

  private def getEpidValues(dataSet: BBbyEPID.DailyDataSetEPID, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    val al = dataSet.al
    DailyQAUtil.getValues(al, tag, scale)
  }

  /**
    * Get the date+time of the image.  Try for content date+time first, and if not available, use acquisition.
    *
    * @param dataSet Get from here.
    * @return A date+time formatted as a string, or "unknown"
    */
  private def dateTimeFromEPID(dataSet: BBbyEPID.DailyDataSetEPID) = {
    val c = DicomUtil.getTimeAndDate(dataSet.al, TagByName.ContentDate, TagByName.ContentTime)
    val a = DicomUtil.getTimeAndDate(dataSet.al, TagByName.AcquisitionDate, TagByName.AcquisitionTime)

    (c, a) match {
      case (Some(dt), _) => Util.spreadsheetDateFormat.format(dt)
      case (_, Some(dt)) => Util.spreadsheetDateFormat.format(dt)
      case _             => "unknown"
    }
  }

  /**
    * Get one of the device (jaw or collimator) limiting values.
    * @param dataSet From EPID.
    * @param axisNameList List of axis names we are looking for.
    * @param index Which value of the jaw/collimator values.
    * @return Text version of a single jaw/collimator value.
    */
  private def beamLimiting(dataSet: BBbyEPID.DailyDataSetEPID, axisNameList: Seq[String], index: Int): String = {
    try {
      val ExposureSequence = DicomUtil.seqToAttr(dataSet.al, TagByName.ExposureSequence).head
      val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(ExposureSequence, TagByName.BeamLimitingDeviceSequence)

      def correctAxis(al: AttributeList): Boolean = {
        val axisName = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString()
        axisNameList.contains(axisName)
      }

      val beamLimitingSeq = BeamLimitingDeviceSequence.find(correctAxis).get
      val text = beamLimitingSeq.get(TagByName.LeafJawPositions).getDoubleValues()(index).toString
      text
    } catch {
      case _: Throwable =>
        "NA"
    }
  }

  private case class Col(header: String, toText: BBbyEPID.DailyDataSetEPID => String) {}

  private val MachineColHeader = "Machine"
  private val PatientIDColHeader = "PatientID"
  private val OperatorsNameColHeader = "OperatorsName"

  /**
    * Extract a value from a BBbyEPID if it is available.
    *
    * @param dataSet  Potentially contains BBbyEPID
    * @param toDouble Gets value from BBbyEPID
    * @return Either a text version of the value or "NA".
    */
  private def epidVal(dataSet: BBbyEPID.DailyDataSetEPID, toDouble: BBbyEPID => Double): String = {
    if (dataSet.data.isRight) toDouble(dataSet.data.right.get).toString
    else "NA"
  }

  private val colList = Seq[Col](
    Col(MachineColHeader, dataSet => dataSet.machine.id),
    Col("Series Time", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.dataDate.get)),
    Col("Image Time", dataSet => dateTimeFromEPID(dataSet)),
    Col("Analysis", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.startDate)),
    Col(PatientIDColHeader, dataSet => patientIdOf(dataSet)),
    //
    Col("BB Image Posn X mm", dataSet => epidVal(dataSet, e => e.epidImageX_mm)),
    Col("BB Image Posn Y mm", dataSet => epidVal(dataSet, e => e.epidImageY_mm)),
    //
    Col("BB Plan Posn X mm", dataSet => epidVal(dataSet, e => e.epid3DX_mm)),
    Col("BB Plan Posn Y mm", dataSet => epidVal(dataSet, e => e.epid3DY_mm)),
    Col("BB Plan Posn Z mm", dataSet => epidVal(dataSet, e => e.epid3DZ_mm)),
    //
    Col("Table X latr mm", dataSet => epidVal(dataSet, e => e.tableXlateral_mm)),
    Col("Table Y vert mm", dataSet => epidVal(dataSet, e => e.tableYvertical_mm)),
    Col("Table Z long mm", dataSet => epidVal(dataSet, e => e.tableZlongitudinal_mm)),
    //
    Col("Image Coeff of Variation", dataSet => epidVal(dataSet, e => e.pixelStandardDeviation_cu / e.pixelMean_cu)),
    Col("Multiple of Std Dev", dataSet => epidVal(dataSet, e => e.bbStdDevMultiple)),
    Col("Pixel Mean CU", dataSet => epidVal(dataSet, e => e.pixelMean_cu)),
    Col("Pixel Std Dev CU", dataSet => epidVal(dataSet, e => e.pixelStandardDeviation_cu)),
    //
    Col("EPID XRay Offset X mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation).head),
    Col("EPID XRay Offset Y mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation)(1)),
    Col("EPID XRay Offset Z mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation)(2)),
    //
    Col("EPID pixel spacing X mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing).head),
    Col("EPID pixel spacing Y mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing)(1)),
    //
    Col("EPID image size X", dataSet => getEpidValues(dataSet, TagByName.Columns).head),
    Col("EPID image size Y", dataSet => getEpidValues(dataSet, TagByName.Rows).head),
    //
    Col("RadiationMachineSAD", dataSet => getEpidValues(dataSet, TagByName.RadiationMachineSAD).head),
    Col("RTImageSID", dataSet => getEpidValues(dataSet, TagByName.RTImageSID).head),
    //
    Col("Gantry Angle", dataSet => getEpidValues(dataSet, TagByName.GantryAngle).head),
    Col("Collimator Angle", dataSet => getEpidValues(dataSet, TagByName.BeamLimitingDeviceAngle).head),
    Col("KVP", dataSet => getEpidValues(dataSet, TagByName.KVP).head),
    Col("Exposure Time ms", dataSet => getEpidValues(dataSet, TagByName.ExposureTime).head),
    Col("Meterset Exposure", dataSet => getEpidValues(dataSet, TagByName.MetersetExposure).head),
    //
    Col("Leaf Jaw Position X Lo", dataSet => beamLimiting(dataSet, Seq("X", "ASYMX"), 0)),
    Col("Leaf Jaw Position X Hi", dataSet => beamLimiting(dataSet, Seq("X", "ASYMX"), 1)),
    Col("Leaf Jaw Position Y Lo", dataSet => beamLimiting(dataSet, Seq("Y", "ASYMY"), 0)),
    Col("Leaf Jaw Position Y Hi", dataSet => beamLimiting(dataSet, Seq("Y", "ASYMY"), 1)),
    //
    Col("RT Image Label", dataSet => textFromAl(dataSet, TagByName.RTImageLabel)),
    //
    Col("Table Top Latl Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopLateralPosition).head),
    Col("Table Top Vert Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopVerticalPosition).head),
    Col("Table Top Long Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopLongitudinalPosition).head),
    //
    Col(OperatorsNameColHeader, dataSet => OperatorsName(dataSet)),
    Col("Reviewer Name", dataSet => textFromAl(dataSet, TagByName.ReviewerName)),
    Col("Approval Status", dataSet => textFromAl(dataSet, TagByName.ApprovalStatus)),
    Col("Software Versions", dataSet => textFromAl(dataSet, TagByName.SoftwareVersions)),
    //
    Col("EPID Details", dataSet => hostRef + ViewOutput.viewOutputUrl(dataSet.output.outputPK.get))
  )

  private def makeRow(dataSet: BBbyEPID.DailyDataSetEPID) =
    colList
      .map(col => {
        col.toText(dataSet)
      })
      .mkString(",")

  override protected def constructHeaders: String = colList.map(col => '"' + col.header + '"').mkString(",")

  override protected def cacheDirName(): String = "DailyQAEpidCSV"

  override protected def firstDataDate(institutionPK: Long): Option[Timestamp] = BBbyEPID.getEarliestDate(institutionPK)

  override protected def fetchData(date: Timestamp, hostRef: String, institutionPK: Long): String = {
    val dataList = BBbyEPID.getForOneDay(date, institutionPK)

    val csvText = dataList.map(dataSet => makeRow(dataSet)).mkString("\n")

    csvText
  }

  override protected def postProcessing(csvText: Seq[String]): Seq[String] = {

    // Look up indexes this way in case they are moved to a different position.
    val machineColIndex = colList.indexWhere(c => c.header.equals(MachineColHeader))
    val patientIdColIndex = colList.indexWhere(c => c.header.equals(PatientIDColHeader))
    val operatorsNameColIndex = colList.indexWhere(c => c.header.equals(OperatorsNameColHeader))

    val deAnon = new DailyQADeAnonymize(institutionPK, machineColIndex, patientIdColIndex, operatorsNameColIndex)

    val processed = csvText.filter(line => line.nonEmpty).map(line => deAnon.deAnonymize(line))

    processed
  }

  override protected def getInstitutionPK: Long = institutionPK
}
