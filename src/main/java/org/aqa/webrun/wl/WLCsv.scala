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

class WLCsv(resultList: Seq[WLImageResult], extendedData: ExtendedData) extends Logging {

  private def generateCsvText: String = {

    def noc(text: String): String = text.replace(',', ' ') // no commas

    case class Dp(v: String, n: String, ok: Boolean = true) {

      def this(v: Double, n: String, ok: Boolean) = this(v.toString, n, ok)

      val value: String = if (ok) noc(v) else "NA"
      val name: String = noc(n)

      def this(v: Double, n: String) = this(v.formatted("%16.12f").trim, n)
    }

    def listToCsv(textList: Seq[String]): String = textList.foldLeft("")((l, t) => if (l.isEmpty) t else l + ',' + t) + "\n"

    def ir2csv(ir: WLImageResult): Seq[Dp] = {

      val tongueAndGrooveOffset = new Point(0, 0)

      val fieldName = ir.gantryRounded_txt + " " + ir.collimatorRounded_txt + " " + ir.elapsedTime_txt

      /**
        * Given a tag, get the string version of the non-anonymized (decrypted) attribute.
        *
        * @param tag For this attribute
        * @return
        */
      def deAnon(tag: AttributeTag): String = {
        val attr = ir.rtimage.get(tag)
        val clear = AnonymizeUtil.deAnonymizeAttribute(extendedData.institution.institutionPK.get, attr)
        if (clear.isDefined)
          clear.get.getSingleStringValueOrEmptyString
        else
          "NA"
      }

      val ok = ir.offXY >= 0

      Seq(
        Dp(extendedData.machine.getRealId, "machine id"),
        Dp(fieldName, "field name"),
        Dp(ir.imageStatus.toString, "status"),
        Dp(ir.attr(TagByName.PatientSupportAngle), "table yaw"),
        Dp(ir.attr(TagByName.GantryAngle), "gantry angle"),
        Dp(ir.attr(TagByName.BeamLimitingDeviceAngle), "coll angle"),
        new Dp(ir.offX, "X offset corrected box-ball", ok),
        new Dp(ir.offY, "Y offset corrected box-ball", ok),
        new Dp(ir.offXY, "XY offset corrected", ok),
        new Dp(ir.box.x, "X box center corrected", ok),
        new Dp(ir.box.y, "Y box center corrected", ok),
        new Dp(tongueAndGrooveOffset.x, "X tongue and groove correction"),
        new Dp(tongueAndGrooveOffset.y, "Y tongue and groove correction"),
        new Dp(ir.ball.x, "X ball center", ok),
        new Dp(ir.ball.y, "Y ball center", ok),
        new Dp(ir.boxEdges.left, "box left uncorrected", ok),
        new Dp(ir.boxEdges.right, "box right uncorrected", ok),
        new Dp(ir.boxEdges.top, "box top uncorrected", ok),
        new Dp(ir.boxEdges.bottom, "box bottom uncorrected", ok),
        new Dp((ir.boxEdges.right + ir.boxEdges.left) / 2, "X box center uncorrected", ok),
        new Dp((ir.boxEdges.bottom + ir.boxEdges.top) / 2, "Y box center uncorrected", ok),
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
    val fileDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")
    val fileName = fileDateFormat.format(extendedData.output.dataDate.get) + ".csv"
    val text = generateCsvText
    val file = new File(extendedData.output.dir, fileName)
    Util.writeFile(file, text)
    logger.info("Wrote WL CSV to " + file.getAbsolutePath)
    fileName
  }

}
