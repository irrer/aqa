package org.aqa.webrun.dailyQA

import org.restlet.Response
import java.util.Date
import org.aqa.db.BBbyEPIDComposite
import org.aqa.Util
import org.aqa.web.ViewOutput
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.aqa.web.WebUtil
import org.aqa.AnonymizeUtil
import org.aqa.db.Input
import org.aqa.db.DicomAnonymous
import com.pixelmed.dicom.TagFromName
import org.aqa.AngleType
import javax.vecmath.Point3d
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.ValueRepresentation
import com.pixelmed.dicom.AttributeList

object DailyQACSV {

  def getCsv(dataSetList: Seq[BBbyEPIDComposite.DailyDataSetComposite], response: Response): Unit = {

    def sorter(a: BBbyEPIDComposite.DailyDataSetComposite, b: BBbyEPIDComposite.DailyDataSetComposite): Boolean = {
      if (a.machine.machinePK.get != b.machine.machinePK.get) (a.machine.machinePK.get < b.machine.machinePK.get)
      else {
        if (a.output.dataDate.get.getTime != b.output.dataDate.get.getTime) (a.output.dataDate.get.getTime < b.output.dataDate.get.getTime)
        else {
          a.output.startDate.getTime < b.output.startDate.getTime
        }
      }
    }

    val institutionPK = WebUtil.getUser(response.getRequest).get.institutionPK
    val patIdMap = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagFromName.PatientID)).
      map(da => (da.value, AnonymizeUtil.decryptWithNonce(institutionPK, da.value_real))).toMap

    /**
     * Build map of machine id --> real id.
     */
    val machineNameSet: Map[String, String] = {
      val machList = dataSetList.groupBy(_.machine.machinePK.get).map(group => group._2.head.machine)
      machList.map(mach => (mach.id, AnonymizeUtil.decryptWithNonce(institutionPK, mach.id_real.get))).toMap
    }

    def patientIdOf(dataSet: BBbyEPIDComposite.DailyDataSetComposite): String = {
      val unknown = "unknown"
      val patId = try {
        val anonPatId = Input.get(dataSet.output.inputPK).get.patientId.get
        val p = patIdMap.get(anonPatId) match {
          case Some(text) => text
          case _ => unknown
        }
        p
      } catch {
        case t: Throwable => unknown
      }

      patId
    }

    val urlPrefix = response.getRequest.getHostRef

    def dblOptToString10(d: Option[Double]) = if (d.isDefined) (d.get / 10).toString else "NA"

    /**
     * Get the list of X,Y,Z table positions for the EPID that is listed first with the requested angle type.  Return the values in cm.
     */
    def epidTablePosition_cm(dataSet: BBbyEPIDComposite.DailyDataSetComposite, angleType: AngleType.Value): Seq[String] = {
      val vert = AngleType.isVert(angleType)
      val list = dataSet.bbByEpid.find(epid => epid.isVert == vert) match {
        case Some(epid) => Seq(epid.tableXlateral_mm, epid.tableYvertical_mm, epid.tableZlongitudinal_mm).map(t => (t / 10.0).toString)
        case _ => Seq("NA", "NA", "NA")
      }
      list
    }

    /**
     * Get the maximum distance that that table traveled between EPID images (has nothing to do with CBCT) in cm.
     */
    def epidMaxTravel_cm(dataSet: BBbyEPIDComposite.DailyDataSetComposite): String = {
      val distList = for (a <- dataSet.bbByEpid; b <- dataSet.bbByEpid) yield {
        val aa = new Point3d(a.tableXlateral_mm, a.tableYvertical_mm, a.tableZlongitudinal_mm)
        val bb = new Point3d(b.tableXlateral_mm, b.tableYvertical_mm, b.tableZlongitudinal_mm)
        aa.distance(bb)
      }
      (distList.max / 10).toString
    }

    //    def getEpidPixelSpacingXY(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Seq[String] = {
    //      val al = dataSet.bbByEpid.head.attributeList
    //      val pixSp = al.get(TagFromName.ImagePlanePixelSpacing)
    //      if (pixSp == null)
    //        Seq("NA", "NA")
    //      else {
    //        val dbl = pixSp.getDoubleValues
    //        Seq(dbl(0).toString, dbl(1).toString)
    //      }
    //    }
    //
    //    def getCbctPixelSpacingXY(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Seq[String] = {
    //      val pixSp = dataSet.cbct.attributeList.get(TagFromName.PixelSpacing)
    //      if (pixSp == null)
    //        Seq("NA", "NA")
    //      else {
    //        val dbl = pixSp.getDoubleValues
    //        Seq(dbl(0).toString, dbl(1).toString)
    //      }
    //    }
    //
    //    def getCbctDouble(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag): String = {
    //      val at = dataSet.cbct.attributeList.get(tag)
    //      if (at == null)
    //        "NA"
    //      else {
    //        at.getDoubleValues()(0).toString
    //      }
    //    }
    //
    //    def getCbctInt(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag): String = {
    //      val at = dataSet.cbct.attributeList.get(tag)
    //      if (at == null)
    //        "NA"
    //      else {
    //        at.getIntegerValues()(0).toString
    //      }
    //    }

    def getNums(al: AttributeList, tag: AttributeTag, scale: Double): Seq[String] = {
      val at = al.get(tag)
      if (at == null) {
        Seq("NA", "NA", "NA")
      }
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

    def getEpidNums(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
      val al = dataSet.bbByEpid.head.attributeList
      getNums(al, tag, scale)
    }

    def getEpidVertNums(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
      val al = dataSet.bbByEpid.filter(e => e.isVert).head.attributeList
      getNums(al, tag, scale)
    }

    def getEpidHorzNums(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
      val al = dataSet.bbByEpid.filter(e => e.isHorz).head.attributeList
      getNums(al, tag, scale)
    }

    def getCbctNums(dataSet: BBbyEPIDComposite.DailyDataSetComposite, tag: AttributeTag, scale: Double = 1.0): Seq[String] = getNums(dataSet.cbct.attributeList, tag, scale)

    case class Col(header: String, toText: (BBbyEPIDComposite.DailyDataSetComposite) => String);

    val colList = Seq[Col](
      new Col("Machine", (dataSet) => machineNameSet(dataSet.machine.id)),
      new Col("Acquired", (dataSet) => Util.standardDateFormat.format(dataSet.output.dataDate.get)),
      new Col("Analysis", (dataSet) => Util.standardDateFormat.format(dataSet.output.startDate)),
      new Col("PatientID", (dataSet) => patientIdOf(dataSet)),
      new Col("Status", (dataSet) => dataSet.output.status),

      new Col("X CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm).toString),
      new Col("Y CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm).toString),
      new Col("Z CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm).toString),

      new Col("X/lat Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableXlateral_mm)),
      new Col("Y/vert Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableYvertical_mm)),
      new Col("Z/lng Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableZlongitudinal_mm)),

      new Col("X/lat Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableXlateral_mm / 10).toString),
      new Col("Y/vert Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableYvertical_mm / 10).toString),
      new Col("Z/lng Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableZlongitudinal_mm / 10).toString),

      new Col("Vert X/lat Table Posn EPID cm", (dataSet) => {
        val j = epidTablePosition_cm(dataSet, AngleType.vertical)
        j(0)
      }),

      new Col("Vert X/lat Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.vertical))(0)),
      new Col("Vert Y/vert Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.vertical))(1)),
      new Col("Vert Z/lng Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.vertical))(2)),

      new Col("Horz X/lat Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.horizontal))(0)),
      new Col("Horz Y/vert Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.horizontal))(1)),
      new Col("Horz Z/lng Table Posn EPID cm", (dataSet) => (epidTablePosition_cm(dataSet, AngleType.horizontal))(2)),

      new Col("EPID max Table Travel cm", (dataSet) => epidMaxTravel_cm(dataSet)),

      new Col("Gantry Angle for XZ (vert) deg", (dataSet) => dataSet.vertList.head.gantryAngle_deg.toString),
      new Col("Vert (EPID-ISO) X mm", (dataSet) => dataSet.vertList.head.epid3DX_mm.toString),
      new Col("Vert (EPID-ISO) Z mm", (dataSet) => dataSet.vertList.head.epid3DZ_mm.toString),
      new Col("Vert (EPID-ISO) - (CBCT-ISO) X mm", (dataSet) => dataSet.composite.xAdjusted_mm.get.toString),
      new Col("Vert (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.vertList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),

      new Col("Gantry Angle for YZ (horz) deg", (dataSet) => dataSet.horzList.head.gantryAngle_deg.toString),
      new Col("Horz (EPID-ISO) Y mm", (dataSet) => dataSet.horzList.head.epid3DY_mm.toString),
      new Col("Horz (EPID-ISO) Z mm", (dataSet) => dataSet.horzList.head.epid3DZ_mm.toString),
      new Col("Horz (EPID-ISO) - (CBCT-ISO) Y mm", (dataSet) => dataSet.composite.yAdjusted_mm.get.toString),
      new Col("Horz (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.horzList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),

      new Col("EPID Vert XRay Offset X mm", (dataSet) => (getEpidVertNums(dataSet, TagFromName.XRayImageReceptorTranslation))(0)),
      new Col("EPID Vert XRay Offset Y mm", (dataSet) => (getEpidVertNums(dataSet, TagFromName.XRayImageReceptorTranslation))(1)),
      new Col("EPID Vert XRay Offset Z mm", (dataSet) => (getEpidVertNums(dataSet, TagFromName.XRayImageReceptorTranslation))(2)),

      new Col("EPID Horz XRay Offset X mm", (dataSet) => (getEpidHorzNums(dataSet, TagFromName.XRayImageReceptorTranslation))(0)),
      new Col("EPID Horz XRay Offset Y mm", (dataSet) => (getEpidHorzNums(dataSet, TagFromName.XRayImageReceptorTranslation))(1)),
      new Col("EPID Horz XRay Offset Z mm", (dataSet) => (getEpidHorzNums(dataSet, TagFromName.XRayImageReceptorTranslation))(2)),

      new Col("Avg (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.composite.zAdjusted_mm.get).toString),

      new Col("No. of EPID images", (dataSet) => dataSet.bbByEpid.size.toString),

      new Col("EPID pixel spacing X mm", (dataSet) => (getEpidNums(dataSet, TagFromName.ImagePlanePixelSpacing))(0)),
      new Col("EPID pixel spacing Y mm", (dataSet) => (getEpidNums(dataSet, TagFromName.ImagePlanePixelSpacing))(1)),

      new Col("CBCT pixel spacing X mm", (dataSet) => (getCbctNums(dataSet, TagFromName.PixelSpacing))(0)),
      new Col("CBCT pixel spacing Y mm", (dataSet) => (getCbctNums(dataSet, TagFromName.PixelSpacing))(1)),
      new Col("CBCT Slice Thickness Z mm", (dataSet) => (getCbctNums(dataSet, TagFromName.SliceThickness))(0)),
      new Col("CBCT num X pix", (dataSet) => (getCbctNums(dataSet, TagFromName.Columns))(0)),
      new Col("CBCT num Y pix", (dataSet) => (getCbctNums(dataSet, TagFromName.Rows))(0)),
      new Col("CBCT num Z pix (slices)", (dataSet) => dataSet.cbctDicomSeries.size.toString),

      new Col("CBCT KVP Peak kilo voltage", (dataSet) => (getCbctNums(dataSet, TagFromName.KVP))(0)),
      new Col("CBCT Exposure Time msec", (dataSet) => (getCbctNums(dataSet, TagFromName.ExposureTime))(0)),
      new Col("CBCT X-Ray Tube Current mA", (dataSet) => (getCbctNums(dataSet, TagFromName.XRayTubeCurrent))(0)),

      new Col("CBCT Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)),
      new Col("EPID Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.composite.outputPK)))

    val headerList = colList.map(col => '"' + col.header + '"').mkString(",")

    def makeRow(dataSet: BBbyEPIDComposite.DailyDataSetComposite) = colList.map(col => col.toText(dataSet)).mkString(",")

    val rowList = dataSetList.sortWith(sorter _).map(dataSet => makeRow(dataSet)).mkString("\n")

    val csv = headerList + "\n" + rowList

    response.setEntity(csv, MediaType.TEXT_CSV)
    response.setStatus(Status.SUCCESS_OK)
  }
}