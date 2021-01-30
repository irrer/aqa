package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.AnonymizeUtil
import org.aqa.Util
import org.aqa.db.BBbyEPID
import org.aqa.db.DicomAnonymous
import org.aqa.db.Machine
import org.aqa.run.CacheCSV
import org.aqa.web.ViewOutput

import java.sql.Timestamp


class DailyQACSVCacheEPID(hostRef: String, institutionPK: Long) extends CacheCSV {


  private def patientIdOf(dataSet: BBbyEPID.DailyDataSetEPID): String = {
    val anonPatId = dataSet.bbByEPID.attributeList.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
    anonPatId
  }

  private def OperatorsName(dataSet: BBbyEPID.DailyDataSetEPID): String = {
    val anonPatId = dataSet.bbByEPID.attributeList.get(TagFromName.OperatorsName).getSingleStringValueOrEmptyString
    anonPatId
  }


  private def textFromAl(dataSet: BBbyEPID.DailyDataSetEPID, tag: AttributeTag): String = {
    val attr = dataSet.bbByEPID.attributeList.get(tag)
    if (attr == null) "NA"
    else attr.getSingleStringValueOrEmptyString
  }

  private def dblOptToString10(d: Option[Double]) = if (d.isDefined) (d.get / 10).toString else "NA"

  /**
   * Get the list of X,Y,Z table positions for the EPID that is listed first with the requested angle type.  Return the values in cm.
   */
  /*
  private def epidTablePosition_cm(dataSet: BBbyEPID.DailyDataSetEPID, angleType: AngleType.Value): Seq[String] = {
    val vert = AngleType.isVert(angleType)
    val list = dataSet.bbByEpid.find(epid => epid.isVert == vert) match {
      case Some(epid) => Seq(epid.tableXlateral_mm, epid.tableYvertical_mm, epid.tableZlongitudinal_mm).map(t => (t / 10.0).toString)
      case _ => Seq("NA", "NA", "NA")
    }
    list
  }
  */

  /**
   * Get the maximum distance that that table traveled between EPID images (has nothing to do with CBCT) in cm.
   */
  /*
  private def epidMaxTravel_cm(dataSet: BBbyEPID.DailyDataSetEPID): String = {
    val distList = for (a <- dataSet.bbByEpid; b <- dataSet.bbByEpid) yield {
      val aa = new Point3d(a.tableXlateral_mm, a.tableYvertical_mm, a.tableZlongitudinal_mm)
      val bb = new Point3d(b.tableXlateral_mm, b.tableYvertical_mm, b.tableZlongitudinal_mm)
      aa.distance(bb)
    }
    (distList.max / 10).toString
  }
  */

  private def getValues(al: AttributeList, tag: AttributeTag, scale: Double): Seq[String] = {
    val list = DicomUtil.findAllSingle(al, tag)

    if (list.isEmpty)
      Seq("NA", "NA", "NA")
    else {
      val at = list.head
      val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
      val numList = vr match {
        case _ if ValueRepresentation.isIntegerStringVR(vr) => at.getIntegerValues
        case _ if ValueRepresentation.isLongStringVR(vr) => at.getLongValues

        case _ if ValueRepresentation.isSignedLongVR(vr) => at.getLongValues
        case _ if ValueRepresentation.isSignedShortVR(vr) => at.getLongValues

        case _ if ValueRepresentation.isUnsignedLongVR(vr) => at.getLongValues
        case _ if ValueRepresentation.isUnsignedShortVR(vr) => at.getShortValues

        case _ if ValueRepresentation.isFloatDoubleVR(vr) => at.getDoubleValues.map(n => n * scale)
        case _ if ValueRepresentation.isFloatSingleVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ if ValueRepresentation.isDecimalStringVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ => throw new RuntimeException("Unrecognized value representation: " + new String(vr))
      }
      numList.map(n => n.toString)
    }
  }

  private def getEpidValues(dataSet: BBbyEPID.DailyDataSetEPID, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    val al = dataSet.bbByEPID.attributeList
    getValues(al, tag, scale)
  }

  private def dateTimeFromEPID(dataSet: BBbyEPID.DailyDataSetEPID, dateTag: AttributeTag, timeTag: AttributeTag): String = {
    val date = DicomUtil.getTimeAndDate(dataSet.bbByEPID.attributeList, dateTag, timeTag)
    if (date.isDefined) Util.standardDateFormat.format(date.get) else "unknown"
  }

  /**
   * Format a number that is approximately zero.
   *
   * @param d Number to format.
   * @return Number formatted as human friendly string.
   */
  private def fmtSmall(d: Double): String = d.formatted("%20.12f").trim.replaceAll("00*$", "")

  // private def getCbctValues(dataSet: BBbyEPID.DailyDataSetEPID, tag: AttributeTag, scale: Double = 1.0): Seq[String] = getValues(dataSet.cbct.attributeList, tag, scale)

  private case class Col(header: String, toText: BBbyEPID.DailyDataSetEPID => String) {}

  private val MachineColHeader = "Machine"
  private val PatientIDColHeader = "PatientID"
  private val OperatorsNameColHeader = "OperatorsName"

  private val colList = Seq[Col](
    Col(MachineColHeader, dataSet => dataSet.machine.id),
    Col("SeriesAcquired", dataSet => Util.standardDateFormat.format(dataSet.output.dataDate.get)),
    Col("ImageAcquisition", dataSet => dateTimeFromEPID(dataSet, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)),
    Col("Analysis", dataSet => Util.standardDateFormat.format(dataSet.output.startDate)),
    Col(PatientIDColHeader, dataSet => patientIdOf(dataSet)),

    Col("BB Image Posn X mm", dataSet => fmtSmall(dataSet.bbByEPID.epidImageX_mm)),
    Col("BB Image Posn Y mm", dataSet => fmtSmall(dataSet.bbByEPID.epidImageY_mm)),

    Col("BB Plan Posn X mm", dataSet => fmtSmall(dataSet.bbByEPID.epid3DX_mm)),
    Col("BB Plan Posn Y mm", dataSet => fmtSmall(dataSet.bbByEPID.epid3DY_mm)),
    Col("BB Plan Posn Z mm", dataSet => fmtSmall(dataSet.bbByEPID.epid3DZ_mm)),

    Col("Table X latr mm", dataSet => fmtSmall(dataSet.bbByEPID.tableXlateral_mm)),
    Col("Table Y vert mm", dataSet => fmtSmall(dataSet.bbByEPID.tableYvertical_mm)),
    Col("Table Z long mm", dataSet => fmtSmall(dataSet.bbByEPID.tableZlongitudinal_mm)),

    Col("EPID XRay Offset X mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation).head),
    Col("EPID XRay Offset Y mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation)(1)),
    Col("EPID XRay Offset Z mm", dataSet => getEpidValues(dataSet, TagByName.XRayImageReceptorTranslation)(2)),

    Col("EPID pixel spacing X mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing).head),
    Col("EPID pixel spacing Y mm", dataSet => getEpidValues(dataSet, TagByName.ImagePlanePixelSpacing)(1)),

    Col("EPID image size X", dataSet => getEpidValues(dataSet, TagFromName.Columns).head),
    Col("EPID image size Y", dataSet => getEpidValues(dataSet, TagFromName.Rows).head),

    Col("RadiationMachineSAD", dataSet => getEpidValues(dataSet, TagByName.RadiationMachineSAD).head),
    Col("RTImageSID", dataSet => getEpidValues(dataSet, TagByName.RTImageSID).head),

    Col("Gantry Angle", dataSet => getEpidValues(dataSet, TagByName.GantryAngle).head),
    Col("Collimator Angle", dataSet => getEpidValues(dataSet, TagByName.BeamLimitingDeviceAngle).head),
    Col("KVP", dataSet => getEpidValues(dataSet, TagByName.KVP).head),
    Col("Meterset Exposure", dataSet => textFromAl(dataSet, TagByName.dict.getTagFromName("MetersetExposure"))),
    Col("RT Image Label", dataSet => textFromAl(dataSet, TagByName.RTImageLabel)),

    Col("Table Top Latl Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopLateralPosition).head),
    Col("Table Top Vert Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopVerticalPosition).head),
    Col("Table Top Long Posn mm", dataSet => getEpidValues(dataSet, TagByName.TableTopLongitudinalPosition).head),

    Col(OperatorsNameColHeader, dataSet => OperatorsName(dataSet)),
    Col("Reviewer Name", dataSet => textFromAl(dataSet, TagFromName.ReviewerName)),
    Col("Approval Status", dataSet => textFromAl(dataSet, TagByName.dict.getTagFromName("ApprovalStatus"))),
    Col("Software Versions", dataSet => textFromAl(dataSet, TagFromName.SoftwareVersions)),

    Col("EPID Details", dataSet => hostRef + ViewOutput.viewOutputUrl(dataSet.output.outputPK.get)))

  private def makeRow(dataSet: BBbyEPID.DailyDataSetEPID) = colList.map(col => col.toText(dataSet)).mkString(",")

  // Trace.trace(TagByName.dict.getTagFromName("ApprovalStatus"))

  override protected def constructHeaders: String = colList.map(col => '"' + col.header + '"').mkString(",")

  override protected def cacheDirName(): String = "DailyQAEpidCSV"

  override protected def firstDataDate(institutionPK: Long): Option[Timestamp] = BBbyEPID.getEarliestDate(institutionPK)

  override protected def fetchData(date: Timestamp, hostRef: String, institutionPK: Long): String = {
    val dataList = BBbyEPID.getForOneDay(date, institutionPK)

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


    // Map id --> de-anonymized real id
    val machineMap = {
      val machineList = Machine.listMachinesFromInstitution(institutionPK)
      val mm = machineList.map(machine => (machine.id, AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)))
      mm.toMap
    }

    val patIdMap = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagFromName.PatientID)).
      map(da => (da.value, AnonymizeUtil.decryptWithNonce(institutionPK, da.value_real))).toMap

    val operatorsNameMap = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagFromName.OperatorsName)).
      map(da => (da.value, AnonymizeUtil.decryptWithNonce(institutionPK, da.value_real))).toMap

    /**
     * Replace anonymized values with real values.
     *
     * @param line Entire text of one line.
     * @return Same line with fields de-anonymized.
     */
    def deAnonymize(line: String): String = {
      val columnList = line.split(",")
      val mach = {
        if (columnList.size < (machineColIndex + 1))
          "unknown machine"
        else {
          val anon = columnList(machineColIndex)
          if (machineMap.contains(anon)) machineMap(anon) else anon
        }
      }

      val pat = {
        if (columnList.size < (patientIdColIndex + 1))
          "unknown Patient ID"
        else {
          val anon = columnList(patientIdColIndex)
          if (patIdMap.contains(anon)) patIdMap(anon) else anon
        }
      }

      val operator = {
        if (columnList.size < (operatorsNameColIndex + 1))
          "unknown Op Name"
        else {
          val anon = columnList(operatorsNameColIndex)
          if (operatorsNameMap.contains(anon)) operatorsNameMap(anon) else anon
        }
      }

      val fixed = columnList.
        patch(machineColIndex, Seq(mach), 1).
        patch(patientIdColIndex, Seq(pat), 1).
        patch(operatorsNameColIndex, Seq(operator), 1)

      fixed.mkString(",")
    }

    val processed = csvText.filter(line => line.nonEmpty).map(line => deAnonymize(line))
    processed
  }

  override protected def getInstitutionPK: Long = institutionPK
}
