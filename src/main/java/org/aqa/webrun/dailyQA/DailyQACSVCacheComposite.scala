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
import org.aqa.AngleType
import org.aqa.Util
import org.aqa.db.BBbyEPIDComposite
import org.aqa.run.CacheCSV
import org.aqa.web.ViewOutput

import java.sql.Timestamp
import javax.vecmath.Point3d

class DailyQACSVCacheComposite(hostRef: String, institutionPK: Long) extends CacheCSV {

  private def patientIdOf(dataSet: DailyDataSetComposite): String = {
    val anonPatId = dataSet.cbct.attributeList.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
    anonPatId
  }

  private def dblOptToString10(d: Option[Double]) = if (d.isDefined) (d.get / 10).toString else "NA"

  /**
    * Get the list of X,Y,Z table positions for the EPID that is listed first with the requested angle type.  Return the values in cm.
    */
  private def epidTablePosition_cm(dataSet: DailyDataSetComposite, angleType: AngleType.Value): Seq[String] = {
    val vert = AngleType.isVert(angleType)
    val list = dataSet.bbByEpid.find(epid => epid.isVert == vert) match {
      case Some(epid) => Seq(epid.tableXlateral_mm, epid.tableYvertical_mm, epid.tableZlongitudinal_mm).map(t => (t / 10.0).toString)
      case _          => Seq("NA", "NA", "NA")
    }
    list
  }

  /**
    * Get the maximum distance that that table traveled between EPID images (has nothing to do with CBCT) in cm.
    */
  private def epidMaxTravel_cm(dataSet: DailyDataSetComposite): String = {
    val distList = for (a <- dataSet.bbByEpid; b <- dataSet.bbByEpid) yield {
      val aa = new Point3d(a.tableXlateral_mm, a.tableYvertical_mm, a.tableZlongitudinal_mm)
      val bb = new Point3d(b.tableXlateral_mm, b.tableYvertical_mm, b.tableZlongitudinal_mm)
      aa.distance(bb)
    }
    (distList.max / 10).toString
  }

  private def getEpidValues(dataSet: DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    val al = dataSet.bbByEpid.head.attributeList
    DailyQAUtil.getValues(al, tag, scale)
  }

  private def getEpidVertValues(dataSet: DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    val al = dataSet.bbByEpid.filter(e => e.isVert).head.attributeList
    DailyQAUtil.getValues(al, tag, scale)
  }

  private def getEpidHorzValues(dataSet: DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    val al = dataSet.bbByEpid.filter(e => e.isHorz).head.attributeList
    DailyQAUtil.getValues(al, tag, scale)
  }

  private def getCbctValues(dataSet: DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = DailyQAUtil.getValues(dataSet.cbct.attributeList, tag, scale)

  private def formatImageOrientationPatient(al: AttributeList): String = {
    al.get(TagByName.ImageOrientationPatient).getDoubleValues.map(d => if (d.round == d) d.toLong.toString else d.toString).mkString("  ")
  }

  private case class Col(header: String, toText: DailyDataSetComposite => String) {}

  private val MachineColHeader = "Machine"
  private val PatientIDColHeader = "PatientID"

  private def xyzDistance(dataSet: DailyDataSetComposite): String = {
    val x = dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm
    val y = dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm
    val z = dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm
    Math.sqrt((x*x) + (y*y) + (z*z)).toString
  }

  private val colList = Seq[Col](
    Col(MachineColHeader, dataSet => dataSet.machine.id),
    Col("Acquired", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.dataDate.get)),
    Col("Analysis", dataSet => Util.spreadsheetDateFormat.format(dataSet.output.startDate)),
    Col(PatientIDColHeader, dataSet => patientIdOf(dataSet)),
    Col("Status", dataSet => dataSet.output.status),
    Col("X CBCT - ISO mm", dataSet => (dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm).toString),
    Col("Y CBCT - ISO mm", dataSet => (dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm).toString),
    Col("Z CBCT - ISO mm", dataSet => (dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm).toString),
    Col("XYZ total Distance CBCT to ISO mm", xyzDistance),
    Col("X/lat Table Movement cm", dataSet => dblOptToString10(dataSet.composite.tableXlateral_mm)),
    Col("Y/vert Table Movement cm", dataSet => dblOptToString10(dataSet.composite.tableYvertical_mm)),
    Col("Z/lng Table Movement cm", dataSet => dblOptToString10(dataSet.composite.tableZlongitudinal_mm)),
    Col("X/lat Table Posn CBCT cm", dataSet => (dataSet.cbct.tableXlateral_mm / 10).toString),
    Col("Y/vert Table Posn CBCT cm", dataSet => (dataSet.cbct.tableYvertical_mm / 10).toString),
    Col("Z/lng Table Posn CBCT cm", dataSet => (dataSet.cbct.tableZlongitudinal_mm / 10).toString),
    Col("Vert X/lat Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.vertical).head),
    Col("Vert X/lat Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.vertical).head),
    Col("Vert Y/vert Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.vertical)(1)),
    Col("Vert Z/lng Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.vertical)(2)),
    Col("Horz X/lat Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.horizontal).head),
    Col("Horz Y/vert Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.horizontal)(1)),
    Col("Horz Z/lng Table Posn EPID cm", dataSet => epidTablePosition_cm(dataSet, AngleType.horizontal)(2)),
    Col("EPID max Table Travel cm", dataSet => epidMaxTravel_cm(dataSet)),
    Col("Gantry Angle for XZ (vert) deg", dataSet => dataSet.vertList.head.gantryAngle_deg.toString),
    Col("Vert (EPID-ISO) X mm", dataSet => dataSet.vertList.head.epid3DX_mm.toString),
    Col("Vert (EPID-ISO) Z mm", dataSet => dataSet.vertList.head.epid3DZ_mm.toString),
    Col("Vert (EPID-ISO) - (CBCT-ISO) X mm", dataSet => dataSet.composite.xAdjusted_mm.get.toString),
    Col("Vert (EPID-ISO) - (CBCT-ISO) Z mm", dataSet => (dataSet.vertList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),
    Col("Gantry Angle for YZ (horz) deg", dataSet => dataSet.horzList.head.gantryAngle_deg.toString),
    Col("Horz (EPID-ISO) Y mm", dataSet => dataSet.horzList.head.epid3DY_mm.toString),
    Col("Horz (EPID-ISO) Z mm", dataSet => dataSet.horzList.head.epid3DZ_mm.toString),
    Col("Horz (EPID-ISO) - (CBCT-ISO) Y mm", dataSet => dataSet.composite.yAdjusted_mm.get.toString),
    Col("Horz (EPID-ISO) - (CBCT-ISO) Z mm", dataSet => (dataSet.horzList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),
    Col("EPID Vert XRay Offset X mm", dataSet => getEpidVertValues(dataSet, TagByName.XRayImageReceptorTranslation).head),
    Col("EPID Vert XRay Offset Y mm", dataSet => getEpidVertValues(dataSet, TagByName.XRayImageReceptorTranslation)(1)),
    Col("EPID Vert XRay Offset Z mm", dataSet => getEpidVertValues(dataSet, TagByName.XRayImageReceptorTranslation)(2)),
    Col("EPID Horz XRay Offset X mm", dataSet => getEpidHorzValues(dataSet, TagByName.XRayImageReceptorTranslation).head),
    Col("EPID Horz XRay Offset Y mm", dataSet => getEpidHorzValues(dataSet, TagByName.XRayImageReceptorTranslation)(1)),
    Col("EPID Horz XRay Offset Z mm", dataSet => getEpidHorzValues(dataSet, TagByName.XRayImageReceptorTranslation)(2)),
    Col("Avg (EPID-ISO) - (CBCT-ISO) Z mm", dataSet => dataSet.composite.zAdjusted_mm.get.toString),
    Col("No. of EPID images", dataSet => dataSet.bbByEpid.size.toString),
    Col("EPID pixel spacing X mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing).head),
    Col("EPID pixel spacing Y mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing)(1)),
    Col("CBCT pixel spacing X mm", dataSet => getCbctValues(dataSet, TagFromName.PixelSpacing).head),
    Col("CBCT pixel spacing Y mm", dataSet => getCbctValues(dataSet, TagFromName.PixelSpacing)(1)),
    Col("CBCT Slice Thickness Z mm", dataSet => getCbctValues(dataSet, TagFromName.SliceThickness).head),
    Col("CBCT num X pix", dataSet => getCbctValues(dataSet, TagFromName.Columns).head),
    Col("CBCT num Y pix", dataSet => getCbctValues(dataSet, TagFromName.Rows).head),
    Col("CBCT num Z pix (slices)", dataSet => dataSet.cbctDicomSeries.size.toString),
    Col("CBCT KVP Peak kilo voltage", dataSet => getCbctValues(dataSet, TagByName.KVP).head),
    Col("CBCT Exposure Time ms", dataSet => getCbctValues(dataSet, TagByName.ExposureTime).head),
    Col("CBCT X-Ray Tube Current mA", dataSet => getCbctValues(dataSet, TagByName.XRayTubeCurrent).head),
    Col("X/lat Table Posn CBCT cm", dataSet => (dataSet.cbct.tableXlateral_mm / 10).toString),
    Col("Y/vert Table Posn CBCT cm", dataSet => (dataSet.cbct.tableYvertical_mm / 10).toString),
    Col("Z/lng Table Posn CBCT cm", dataSet => (dataSet.cbct.tableZlongitudinal_mm / 10).toString),
    Col("Patient Support Angle", dataSet => getCbctValues(dataSet, TagByName.PatientSupportAngle).head),
    Col("TableTopPitchAngle", dataSet => getCbctValues(dataSet, TagByName.TableTopPitchAngle).head),
    Col("TableTopRollAngle", dataSet => getCbctValues(dataSet, TagByName.TableTopRollAngle).head),
    Col("ImageOrientationPatient", dataSet => formatImageOrientationPatient(dataSet.cbct.attributeList)),
    Col("CBCT Details", dataSet => hostRef + ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)),
    Col("EPID Details", dataSet => hostRef + ViewOutput.viewOutputUrl(dataSet.composite.outputPK))
  )

  private def makeRow(dataSet: DailyDataSetComposite) = colList.map(col => col.toText(dataSet)).mkString(",")

  override protected def constructHeaders: String = colList.map(col => '"' + col.header + '"').mkString(",")

  override protected def cacheDirName(): String = "DailyQACompositeCSV"

  override protected def firstDataDate(institutionPK: Long): Option[Timestamp] = BBbyEPIDComposite.getEarliestDate(institutionPK)

  override protected def fetchData(date: Timestamp, hostRef: String, institutionPK: Long): String = {
    val dataList = BBbyEPIDComposite.getForOneDay(date, institutionPK)

    val csvText = dataList.map(dataSet => makeRow(dataSet)).mkString("\n")

    // response.setEntity(csv, MediaType.TEXT_CSV)
    // response.setStatus(Status.SUCCESS_OK)
    csvText
  }

  override protected def postProcessing(csvText: Seq[String]): Seq[String] = {

    // Look up indexes this way in case they are moved to a different position.
    val machineColIndex = colList.indexWhere(c => c.header.equals(MachineColHeader))
    val patientIdColIndex = colList.indexWhere(c => c.header.equals(PatientIDColHeader))

    val deAnon = new DailyQADeAnonymize(institutionPK, machineColIndex = machineColIndex, patientIdColIndex = patientIdColIndex)

    val processed = csvText.filter(line => line.nonEmpty).map(line => deAnon.deAnonymize(line))
    processed
  }

  override protected def getInstitutionPK: Long = institutionPK
}
