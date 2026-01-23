package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import org.aqa.webrun.ExtendedData
import org.aqa.AnonymizeUtil
import org.aqa.Logging
import org.aqa.Util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.vecmath.Point2d

class WLCsv(resultList: Seq[WLResult], extendedData: ExtendedData) extends Logging {

  private def generateCsvText: String = {

    def noc(text: String): String = text.replace(',', ' ') // no commas

    case class Dp(v: String, n: String, ok: Boolean = true) {

      def this(v: Double, n: String, ok: Boolean) = this(v.toString, n, ok)
      def this(v: Option[Double], n: String, ok: Boolean) = this(if (v.isDefined) v.get.toString else "NA", n, ok)

      val value: String = if (ok) noc(v) else "NA"
      val name: String = noc(n)

      def this(v: Double, n: String) = this("%16.12f".format(v).trim, n)
    }

    def listToCsv(textList: Seq[String]): String = textList.foldLeft("")((l, t) => if (l.isEmpty) t else l + ',' + t) + "\n"

    def ir2csv(ir: WLResult): Seq[Dp] = {

      val tongueAndGrooveOffset = new Point2d(0, 0)

      val fieldName = ir.gantryRounded_txt + " " + ir.collimatorRounded_txt + " " + ir.elapsedTime_txt

      /**
        * Given a tag, get the string version of the non-anonymized (decrypted) attribute.
        *
        * @param tag For this attribute
        * @return
        */
      def deAnon(tag: AttributeTag): String = {
        val attr = ir.attrList.get(tag)
        val clear = AnonymizeUtil.deAnonymizeAttribute(extendedData.institution.institutionPK.get, attr)
        if (clear.isDefined)
          clear.get.getSingleStringValueOrEmptyString
        else
          "NA"
      }

      val ok = ir.offsetXY_mm >= 0

      Seq(
        Dp(extendedData.machine.getRealId, "machine id"),
        Dp(fieldName, "field name"),
        Dp(ir.getImageStatus.toString, "status"),
        Dp(ir.attr(TagByName.PatientSupportAngle), "table angle"),
        Dp(ir.attr(TagByName.GantryAngle), "gantry angle"),
        Dp(ir.attr(TagByName.BeamLimitingDeviceAngle), "coll angle"),
        new Dp(ir.offsetX_mm, "X offset corrected box-ball", ok),
        new Dp(ir.offsetY_mm, "Y offset corrected box-ball", ok),
        new Dp(ir.offsetXY_mm, "XY offset corrected", ok),
        new Dp(ir.boxCenter_mm.x, "X box center corrected", ok),
        new Dp(ir.boxCenter_mm.y, "Y box center corrected", ok),
        new Dp(tongueAndGrooveOffset.x, "X tongue and groove correction"),
        new Dp(tongueAndGrooveOffset.y, "Y tongue and groove correction"),
        new Dp(ir.ballCenter_mm.map(_.x), "X ball center", ok),
        new Dp(ir.ballCenter_mm.map(_.y), "Y ball center", ok),
        new Dp(ir.OffsetLeft_mm, "box left uncorrected", ok),
        new Dp(ir.OffsetRight_mm, "box right uncorrected", ok),
        new Dp(ir.OffsetTop_mm, "box top uncorrected", ok),
        new Dp(ir.OffsetBottom_mm, "box bottom uncorrected", ok),
        new Dp(ir.OffsetX1_mm, "X1 distance to center", ok),
        new Dp(ir.OffsetX2_mm, "X2 distance to center", ok),
        new Dp(ir.OffsetY1_mm, "Y1 distance to center", ok),
        new Dp(ir.OffsetY2_mm, "Y2 distance to center", ok),
        // new Dp((ir.boxEdges.right + ir.boxEdges.left) / 2, "X box center uncorrected", ok),
        // new Dp((ir.boxEdges.bottom + ir.boxEdges.top) / 2, "Y box center uncorrected", ok),
        Dp(deAnon(TagByName.PatientID), "Patient ID"),
        Dp(deAnon(TagByName.PatientName), "Patient Name"),
        Dp(deAnon(TagByName.SOPInstanceUID), "Instance (slice) UID"),
        Dp(deAnon(TagByName.SeriesInstanceUID), "Series UID")
      )
    }

    val title: String = {
      val title = "Winston-Lutz Field Data"
      val data = "Data Date: " + noc(Util.spreadsheetDateFormat.format(extendedData.output.dataDate.get))
      val analysis = "Analysis Date: " + noc(Util.spreadsheetDateFormat.format(new Date))

      Seq(title, data, analysis).mkString(",") + "\n"
    }
    val header: String = listToCsv(ir2csv(resultList.head).map(dp => dp.name))
    val content = resultList.sortBy(ir => ir.elapsedTime_ms).map(ir => ir2csv(ir).map(_.value).mkString(",")).mkString("\n")

    val text = title + header + content

    text
  }

  def writeCsvFile: String = {
    //noinspection SpellCheckingInspection
    val fileDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")
    val fileName = fileDateFormat.format(extendedData.output.dataDate.get) + ".csv"
    val text = generateCsvText
    val file = new File(extendedData.output.dir, fileName)
    Util.writeFile(file, text)
    logger.info("Wrote WL CSV to " + file.getAbsolutePath)
    fileName
  }

}
