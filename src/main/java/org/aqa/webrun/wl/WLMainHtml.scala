package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.MachineWL
import org.aqa.web.C3ChartHistory
import org.aqa.web.WebUtil
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.webrun.winLutz360.WinLutz360Chart
import org.aqa.webrun.winLutz360.WLFailure

import java.awt.Color
import java.io.File
import scala.xml.Elem

object WLMainHtml extends Logging {

  def generateGroupHtml(extendedData: ExtendedData, resultList: Seq[Either[WLFailure, WLResult]], runReq: WLRunReq, monthly: Elem): String = {
    val dataList = resultList.filter(_.isRight).map(_.right.get)

    /**
     * Get the image status of an analysis result.
     *
     * @param wlResult Either failure or data.
     * @return Image status.
     */
    def statusOf(wlResult: Either[WLFailure, WLResult]): WLImageStatus.Value = {
      if (wlResult.isLeft)
        WLImageStatus.UnexpectedError
      else
        wlResult.right.get.getImageStatus
    }

    val wlParameters = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get)

    def subDirOf(ir: Either[WLFailure, WLResult]): File = {
      if (ir.isLeft) {
        val subDirName = ir.left.get.wlMessage.runReq.subDirName(ir.left.get.wlMessage.rtimage)
        new File(extendedData.output.dir, subDirName)
      } else
        ir.right.get.getDirectory
    }

    def beamNameOf(ir: Either[WLFailure, WLResult]): Option[String] = {
      if (ir.isRight && ir.right.get.beamName.isDefined)
        ir.right.get.beamName
      else
        None
    }

    val passStyle = s"color: #000000; background: #${Config.WLPassColor};"
    val cautionStyle = s"color: #000000; background: yellow;"

    def canRead(name: String, ir: Either[WLFailure, WLResult]): Boolean = {
      new File(subDirOf(ir), name).canRead
    }

    def fmtTime(al: AttributeList): String = {
      val totalSeconds = (WLImageUtil.timeOf(al).getTime - extendedData.output.dataDate.get.getTime) / 1000
      val secondsText = "%02d".format(totalSeconds % 60)
      s"""${totalSeconds / 60}:$secondsText"""
    }

    def csvFileName = {
      // val list = resultList // .filter(_.isInstanceOf[WLImageResult]).map(_.asInstanceOf[WLImageResult])
      val wlCsv = new WLCsv(dataList, extendedData)
      wlCsv.writeCsvFile
    }

    // val timeOf = new ImageMetaDataGroup(resultList.map(r => r.imageMetaData)).timeOf

    val laserCorrectionList = WLLaserCorrection.setList(dataList)

    // val readyForEvaluation = if (jobStatus(resultList) == JobStatus.ReadyForEvaluation) "*" else ""

    def irTextHtml(ir: Either[WLFailure, WLResult]): Seq[Elem] = {

      val dir = subDirOf(ir)

      val attrList: AttributeList = {
        if (ir.isLeft)
          ir.left.get.wlMessage.rtimage
        else
          ir.right.get.attrList
      }

      dir.mkdirs()

      def hiFmtDbl(d: Double): String = "%9.6f".format(d).trim

      // val wl: Option[WinstonLutz] = if (WLImageStatus.hasResult(ir.imageStatus)) Some(ir.toWinstonLutz) else None

      val diagnostics: Elem = {
        val elem =
          if (canRead(WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME, ir))
            <a href={dir.getName + "/" + WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME}>Diagnostics</a>
          else {
            <a href={s"${dir.getName}/${Util.sopOfAl(attrList)}.txt"}>View DICOM Metadata</a>
          }
        elem
      }

      val badPixels: Elem = {
        if (ir.isRight) {
          val r = ir.right.get
          if ((r.getBadPixelList == null) || r.getBadPixelList.isEmpty)
            <span></span>
          else {
            <span>
              {r.getBadPixelList.size}
            </span>
          }
        } else
          <span>Bad Pixel List Not Available</span>
      }

      val laserIsDefined = {
        ir.isRight && WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir.right.get).isDefined
      }

      val laserHtml: Option[Elem] = if (laserIsDefined) Some(<td/>) else None

      def getNameHtml(ir: Either[WLFailure, WLResult]): Elem = {
        def tableAngle_deg: Double = attrList.get(TagByName.PatientSupportAngle).getDoubleValues.head

        val tableAngleText = {
          val ta = Util.angleRoundedToTenthExceptCardinal(tableAngle_deg)
          if (ta.round == ta)
            "%d".format(ta.round)
          else
            "%5.1f".format(ta).trim
        }

        <b>
          {s"G${Util.angleRoundedTo1(Util.gantryAngle(attrList))} C${Util.angleRoundedTo1(Util.collimatorAngle(attrList))} T$tableAngleText ${fmtTime(attrList)}"}
        </b>
      }

      def passedText(ir: Either[WLFailure, WLResult]): Elem = {
        val status = statusOf(ir)
        if (status == WLImageStatus.Passed)
          <span style={"color:black; background:" + Config.WLPassColor}>PASSED</span>
        else
          <span style={"color:black; background:" + Config.WLFailColor}>
            {status}
          </span>
      }

      val elem: Elem = {

        val offsetX_mm: Double = if (ir.isRight) ir.right.get.offsetX_mm else Double.NaN
        val offsetY_mm: Double = if (ir.isRight) ir.right.get.offsetY_mm else Double.NaN
        val offsetXY_mm: Double = if (ir.isRight) ir.right.get.offsetXY_mm else Double.NaN

        <td style='background: #eeeeee'>
          <center>
            <h3 title={s"Gantry angle, collimator angle,${WebUtil.titleNewline}Table angle, and time since start"}>
              <b>
                {getNameHtml(ir)}
              </b>
            </h3>
            <p>
              {if (beamNameOf(ir).isDefined) {
              "Beam " + beamNameOf(ir).get
            } else
              ""}
            </p>
            <p title={hiFmtDbl(offsetX_mm) + ", " + hiFmtDbl(offsetY_mm)}>
              Offset in mm X =
              {WebUtil.setPrecisionAttr(<span></span>, offsetX_mm)}
              Y =
              {WebUtil.setPrecisionAttr(<span></span>, offsetY_mm)}
            </p>
            <p title={hiFmtDbl(offsetXY_mm)}>
              R =
              {WebUtil.setPrecisionAttr(<span></span>, offsetXY_mm)}{passedText(ir)}
            </p>
            <p>
              {diagnostics}
            </p>
            <p>
              {badPixels}
            </p>
          </center>
        </td>
      }

      Seq(laserHtml, Some(elem)).flatten
    }

    def toHtml(color: Color): String = {
      "#" + "%06x".format(color.getRGB & 0xffffff)
    }

    def irThumbImageHtml(sts: WLImageStatus.ImageStatus): Elem = {
      val color = if (sts == WLImageStatus.Passed) toHtml(Config.WLPassColor) else toHtml(Config.WLFailColor)
      <td height="20" width="20" style={s"background:$color;foreground:$color;"}></td>
    }

    def irThumbImageListHtml: Elem = {
      <table border="0" style="border-collapse:separate; border-spacing:0.5em;">
        <tr>
          {resultList.map(wl => irThumbImageHtml(statusOf(wl)))}
        </tr>
      </table>
    }

    def irImageHtml(ir: Either[WLFailure, WLResult]): Seq[Elem] = {

      val subDir = subDirOf(ir)

      def img(name: String): Elem = {
        val title = name match {
          case WLgenHtml.NORMAL_SUMMARY_FILE_NAME => "Summary Image"
          case WLgenHtml.BRIGHT_SUMMARY_FILE_NAME => ""
          case WLgenHtml.ORIGINAL_FILE_NAME => "Entire Image"
          case _ => "Image"
        }

        val id: String = C3Chart.makeUniqueChartIdTag
        val url = subDir.getName + "/" + name
        val script = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

        <div>
          <span>
            {title}
          </span>
          <script>
            {script}
          </script>
          <a href={url}>
            <div class='zoom' id={id}>
              <img width={Config.WLSummarySize.toString} src={url}/>
            </div>
          </a>
        </div>
      }

      val laserHtml: Option[Elem] = {
        val laserCor: Option[WLLaserCorrection] = {
          if (ir.isRight)
            WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir.right.get)
          else
            None
        }
        if (laserCor.isDefined)
          Some(<td>
            {laserCorrectionToHtml(laserCor.get)}
          </td>)
        else
          None
      }

      val imageHtml = {
        <td>
          <center>
            {0 match {
            case _ if canRead(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.NORMAL_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.NORMAL_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.ORIGINAL_FILE_NAME, ir) => img(WLgenHtml.ORIGINAL_FILE_NAME)

            case _ => <span>No Image Available</span>
          }}
          </center>
        </td>
      }
      Seq(laserHtml, Some(imageHtml)).flatten
    }

    def laserCorrectionToHtml(correction: WLLaserCorrection): Elem = {
      class Instruction(val pos: String, val neg: String) {}

      val style = "padding:8px;"

      def row(name: String, value: Double, instruction: Instruction): Elem = {
        val instr = if (value >= 0) instruction.pos else instruction.neg
        val corrStatus: Elem = {
          if (value.abs < Config.WLLaserCorrectionLimit)
            <span style={passStyle}>
              {correction.passedText}
            </span>
          else
            <span style={cautionStyle}>
              {correction.failedText}
            </span>
        }

        <tr>
          <td style={style}>
            {name}
          </td>
          <td style={style}>
            {("%6.2f".format(value) + "mm").trim}
          </td>
          <td style={style}>
            {instr}
          </td>
          <td style={style}>
            {corrStatus}
          </td>
        </tr>
      }

      <center>
        Laser Corrections
        <br/>
        Tolerance:
        {Config.WLLaserCorrectionLimit}<table border="1" width="350">
        <tr bgcolor='dddddd'>
          <td style={style}>Axis</td>
          <td style={style}>Offset</td>
          <td style={style}>Correct by moving</td>
          <td style={style}>Status</td>
        </tr>{row("Longitudinal", correction.longitudinal, new Instruction("away from gantry", "toward gantry"))}{row("Lateral", correction.lateral, new Instruction("towards left when facing gantry", "towards right when facing gantry"))}{row("Vertical", correction.vertical, new Instruction("towards floor", "towards ceiling"))}
      </table>
      </center>
    }

    def csvLink(): Elem = {
      if (!resultList.exists(r => WLImageStatus.hasResult(statusOf(r)))) {
        <span>No Results</span>
      } else {
        <a title="Results as spreadsheet/CSV" href={csvFileName}>Results</a>
      }
    }

    def html(resultList: Seq[Either[WLFailure, WLResult]]): String = {

      val offsets: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr>
            <td>Tongue
              &amp;
              Groove Offsets dX = 0.0 dY = 0.0
            </td>
          </tr>
          <tr>
            <td>Radial Offset Tolerance, Rtol =
              {wlParameters.passLimit_mm}
              mm
            </td>
          </tr>
        </table>
      }

      val imageHtml: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr style='background: #eeeeee'>
            {resultList.flatMap(irTextHtml)}
          </tr>
          <tr>
            {resultList.flatMap(irImageHtml)}
          </tr>
        </table>
      }

      val headTable1: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr>
            <td>
              {WebUtil.showPrecision}
            </td>
            <td>
              {offsets}
            </td>
            <td></td>
          </tr>
        </table>
      }

      val readyForEvaluationNote: String = {
        ""
      }

      val rtplanView: Elem = {
        if (runReq.rtplan.isDefined) {
          val fileName = "RTPLAN.txt"
          val rtplanText = DicomUtil.attributeListToString(runReq.rtplan.get)
          val file = new File(extendedData.output.dir, fileName)
          Util.writeFile(file, rtplanText)
          <a href="RTPLAN.txt" style="margin-left: 24px;margin-right: 24px;">RTPLAN as text</a>
        } else <span style="margin-left: 24px;margin-right: 24px;">RTPLAN Not Available</span>
      }

      val passFailBanner: Elem = {

        def makeElem(text: String, color: Color) = {
          val style = s"color: #000000; background: ${toHtml(color)};"
          <h1 style={style}>
            <b style="margin-left:12px;margin-right:12px;">
              {text}
            </b>
          </h1>
        }

        resultList.find(r => statusOf(r) != WLImageStatus.Passed) match {
          case Some(result) =>
            makeElem(statusOf(result).toString, Config.WLFailColor)
          case _ =>
            if (resultList.isEmpty)
              makeElem("FAILED", Config.WLFailColor)
            else
              makeElem("PASSED", Config.WLPassColor)
        }
      }

      val headTable2: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr>
            <td>
              {csvLink()}
            </td>
            <td>
              {rtplanView}
            </td>
            <td>
              {monthly}
            </td>
            <td>
              {passFailBanner}
            </td>
            <td>
              {resultList.size}
              images</td>
            <td>
              {irThumbImageListHtml}
            </td>
          </tr>
        </table>
      }

      val wlChart = {
        if (extendedData.procedure.isWinLutz360)
          new WinLutz360Chart(extendedData.output.outputPK.get)
        else
          new WLChart(extendedData.output.outputPK.get)
      }

      val chartHtml: Seq[Elem] = {
        def toElem(beamName: String, chart: C3ChartHistory) = {
          <div style="border:solid grey 1px; margin-top:16px;">
            <center>
              <h3>
                {beamName}
              </h3>
            </center>{chart.html}
          </div>
        }

        val list = wlChart.beamNameList.zip(wlChart.chartList)

        val help = <div style="margin-top:20px;margin-bottom:8px;">
          {C3ChartHistory.htmlHelp()}
        </div>
        help +: list.map(nameChart => toElem(nameChart._1, nameChart._2))
      }

      // val javaScript = wlChart.chartList.map(_.javascript).mkString("\n")
      val runScript = s"""<script src='/WLHistoryRestlet?outputPK=${extendedData.output.outputPK.get.toString}'></script>"""

      val content: Elem = {
        <div>
          {headTable2}{headTable1}{readyForEvaluationNote}{imageHtml}{chartHtml}
        </div>
      }

      // @formatter:off
      val text = WebUtil.wrapBody(
        content = ExtendedData.wrapExtendedData(extendedData, content),
        pageTitle = extendedData.procedure.name,
        c3 = true,
        runScript = Some(runScript) )
      // @formatter:on
      text
    }

    //Log.get.finest("Generated html: \n\n" + html + "\n\n")
    val htmlText = html(resultList)
    htmlText
  }

}
