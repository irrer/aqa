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
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.run.CacheCSV
import org.aqa.web.ViewOutput

import java.sql.Timestamp

class DailyQACSVCacheCBCT(hostRef: String, institutionPK: Long) extends CacheCSV {

  private def patientIdOf(dataSet: BBbyCBCT.DailyDataSetCBCT): String = {
    dataSet.dicomSeries.attributeListList.head
    val anonPatId = dataSet.al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
    anonPatId
  }

  private def textFromAl(dataSet: BBbyCBCT.DailyDataSetCBCT, tag: AttributeTag): String = {
    val attr = dataSet.al.get(tag)
    if (attr == null) "NA"
    else attr.getSingleStringValueOrEmptyString
  }

  private def getCbctValues(dataSet: BBbyCBCT.DailyDataSetCBCT, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    DailyQAUtil.getValues(dataSet.al, tag, scale)
  }

  private case class Col(header: String, toText: BBbyCBCT.DailyDataSetCBCT => String) {}

  private val MachineColHeader = "Machine"
  private val PatientIDColHeader = "PatientID"
  private val OperatorsNameColHeader = "OperatorsName"

  private def formatImageOrientationPatient(al: AttributeList): String = {
    al.get(TagByName.ImageOrientationPatient).getDoubleValues.map(d => if (d.round == d) d.toLong.toString else d.toString).mkString("  ")
  }

  private val colList = Seq[Col](
    Col(MachineColHeader, dataSet => dataSet.machine.id),
    Col("Acquired", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.dataDate.get)),
    Col("Analysis", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.startDate)),
    Col(PatientIDColHeader, dataSet => patientIdOf(dataSet)),
    Col("Status", dataSet => dataSet.output.status),
    Col(
      "X CBCT mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.cbctX_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "Y CBCT mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.cbctY_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "Z CBCT mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.cbctZ_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "X ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.rtplanX_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "Y ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.rtplanY_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "Z ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => c.rtplanZ_mm.toString;
          case _       => "NA"
        }
    ),
    Col(
      "X CBCT - ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.cbctX_mm - c.rtplanX_mm).toString;
          case _       => "NA"
        }
    ),
    Col(
      "Y CBCT - ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.cbctY_mm - c.rtplanY_mm).toString;
          case _       => "NA"
        }
    ),
    Col(
      "Z CBCT - ISO mm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.cbctZ_mm - c.rtplanZ_mm).toString;
          case _       => "NA"
        }
    ),
    Col(
      "X/lat Table Posn CBCT cm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.tableXlateral_mm / 10).toString;
          case _       => "NA"
        }
    ),
    Col(
      "Y/vrt Table Posn CBCT cm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.tableYvertical_mm / 10).toString;
          case _       => "NA"
        }
    ),
    Col(
      "Z/lng Table Posn CBCT cm",
      dataSet =>
        dataSet.cbct match {
          case Some(c) => (c.tableZlongitudinal_mm / 10).toString;
          case _       => "NA"
        }
    ),
    Col("pixel spacing X mm", dataSet => getCbctValues(dataSet, TagFromName.PixelSpacing).head),
    Col("pixel spacing Y mm", dataSet => getCbctValues(dataSet, TagFromName.PixelSpacing)(1)),
    Col("Slice Thickness Z mm", dataSet => getCbctValues(dataSet, TagFromName.SliceThickness).head),
    Col("num X pix", dataSet => getCbctValues(dataSet, TagFromName.Columns).head),
    Col("num Y pix", dataSet => getCbctValues(dataSet, TagFromName.Rows).head),
    Col("num Z pix (slices)", dataSet => dataSet.dicomSeries.size.toString),
    Col("KVP Peak kilo voltage", dataSet => getCbctValues(dataSet, TagByName.KVP).head),
    Col("Exposure Time ms", dataSet => getCbctValues(dataSet, TagByName.ExposureTime).head),
    Col("X-Ray Tube Current mA", dataSet => getCbctValues(dataSet, TagByName.XRayTubeCurrent).head),
    Col("Collection Diam mm", dataSet => getCbctValues(dataSet, TagByName.DataCollectionDiameter).head),
    Col("Table Longitudinal Position mm", dataSet => getCbctValues(dataSet, TagByName.TableTopLongitudinalPosition).head),
    Col("Table Lateral Position", dataSet => getCbctValues(dataSet, TagByName.TableTopLateralPosition).head),
    Col("Patient Support Angle", dataSet => getCbctValues(dataSet, TagByName.PatientSupportAngle).head),
    Col("TableTopPitchAngle", dataSet => getCbctValues(dataSet, TagByName.TableTopPitchAngle).head),
    Col("TableTopRollAngle", dataSet => getCbctValues(dataSet, TagByName.TableTopRollAngle).head),
    Col("ImageOrientationPatient", dataSet => formatImageOrientationPatient(dataSet.al)),
    Col("Rotation Direction", dataSet => textFromAl(dataSet, TagByName.RotationDirection)),
    Col("Software Versions", dataSet => textFromAl(dataSet, TagByName.SoftwareVersions)),
    Col("CBCT Details", dataSet => hostRef + ViewOutput.viewOutputUrl(dataSet.output.outputPK.get))
  )

  private def makeRow(dataSet: BBbyCBCT.DailyDataSetCBCT) =
    colList
      .map(col => {
        col.toText(dataSet)
      })
      .mkString(",")

  // Trace.trace(TagByName.dict.getTagFromName("ApprovalStatus"))

  override protected def constructHeaders: String = colList.map(col => '"' + col.header + '"').mkString(",")

  override protected def cacheDirName(): String = "DailyQACbctCSV"

  override protected def firstDataDate(institutionPK: Long): Option[Timestamp] = BBbyCBCT.getEarliestDate(institutionPK)

  override protected def fetchData(date: Timestamp, hostRef: String, institutionPK: Long): String = {
    val dataList = BBbyCBCT.getForOneDay(date, institutionPK)

    val csvText = dataList.map(dataSet => makeRow(dataSet)).mkString("\n")

    // response.setEntity(csv, MediaType.TEXT_CSV)
    // response.setStatus(Status.SUCCESS_OK)
    csvText
  }

  override protected def postProcessing(csvText: Seq[String]): Seq[String] = {

    // Look up indexes this way in case they are moved to a different position.
    val machineColIndex = colList.indexWhere(c => c.header.equals(MachineColHeader))
    val patientIdColIndex = colList.indexWhere(c => c.header.equals(PatientIDColHeader))
    val operatorsNameColIndex = colList.indexWhere(c => c.header.equals(OperatorsNameColHeader))

    val deAnon = new DailyQADeAnonymize(institutionPK, machineColIndex = machineColIndex, patientIdColIndex = patientIdColIndex, operatorsNameColIndex = operatorsNameColIndex)

    val processed = csvText.filter(line => line.nonEmpty).map(line => deAnon.deAnonymize(line))

    processed
  }

  override protected def getInstitutionPK: Long = institutionPK
}
