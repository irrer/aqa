package org.aqa.webrun.wl

import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Logging
import org.aqa.web.WebUtil

import java.awt.Color
import java.io.File
import scala.xml.Elem

object WLMainHtml extends Logging {

  def generateGroupHtml(extendedData: ExtendedData, resultList: Seq[WLImageResult]): String = {
    // val relativeUrl = jobDir.getAbsolutePath.substring(Config.DataDirectory.getAbsolutePath.length + 1).replace('\\', '/')

    val groupStartTime = resultList.map(_.contentTime).minBy(_.getTime)

    val passStyle = s"color: #000000; background: #${Config.WLPassColor};"
    val cautionStyle = s"color: #000000; background: yellow;"

    def relUrl(ir: WLImageResult) = ir.directory.getName

    def canRead(name: String, ir: WLImageResult): Boolean = new File(ir.directory, name).canRead

    def fmtTime(ir: WLImageResult): String = {
      val totalSeconds = (ir.contentTime.getTime - groupStartTime.getTime) / 1000
      "" + (totalSeconds / 60) + ":" + (totalSeconds % 60).formatted("%02d")
    }

    val csvFileName = {
      val wlCsv = new WLCsv(resultList, extendedData)
      wlCsv.writeCsvFile
    }

    // val timeOf = new ImageMetaDataGroup(resultList.map(r => r.imageMetaData)).timeOf

    val laserCorrectionList = WLLaserCorrection.setList(resultList)

    // val readyForEvaluation = if (jobStatus(resultList) == JobStatus.ReadyForEvaluation) "*" else ""

    def irTextHtml(ir: WLImageResult): Seq[Elem] = {
      def fmtDbl(value: Double): String = value.formatted("%6.2f").trim

      val diagnostics: Elem = {
        val elem =
          if (canRead(WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME, ir))
            <a href={relUrl(ir) + "/" + WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME}>Diagnostics</a>
          else {
            <a href={relUrl(ir)}>Generated Files</a>
          }
        elem
      }

      val badPixels: Elem = {
        if ((ir.badPixelList == null) || ir.badPixelList.isEmpty)
          <span></span>
        else {
          <span>{ir.badPixelList.size}</span>
        }
      }

      val laserIsDefined = WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir).isDefined
      val laserHtml: Option[Elem] = if (laserIsDefined) Some(<td/>) else None

      def getNameHtml(ir: WLImageResult): Elem = {
        <b>
          <span>G{ir.gantryRounded_deg}</span>
          <span>C{ir.collimatorRounded_deg.round}</span>
          <span>{fmtTime(ir)}</span>
        </b>
      }

      def passedText(ir: WLImageResult): Elem = {
        if (ir.imageStatus == ImageStatus.Passed)
          <span style={"color:black; background:" + Config.WLPassColor}> PASSED </span>
        else
          <span style={"color:black; background:" + Config.WLFailColor}> {ir.imageStatus} </span>
      }

      val elem: Elem = {
        <td style='background: #eeeeee'>
          <center>
            <h3 title='Gantry angle, collimator angle, and time since start'><b>{getNameHtml(ir)}</b></h3>
            <p>
            Offset in mm X = {fmtDbl(ir.offX)} Y = {fmtDbl(ir.offY)}
            </p>
            <p>
              R = {fmtDbl(ir.offXY)}  {passedText(ir)}
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
      "#" + (color.getRGB & 0xffffff).formatted("%06x")
    }

    def irThumbImageHtml(ir: WLImageResult): Elem = {
      val color = if (ir.imageStatus == ImageStatus.Passed) toHtml(Config.WLPassColor) else toHtml(Config.WLFailColor)
      <td height="20" width="20" style={s"background:$color;foreground:$color;"}> </td>
    }

    def irThumbImageListHtml: Elem = {
      <table border="0" style="border-collapse:separate; border-spacing:0.5em;"><tr>
        {resultList.map(irThumbImageHtml)}
        </tr>
      </table>
    }

    def irImageHtml(ir: WLImageResult): Seq[Elem] = {
      def img(name: String): Elem = {
        val title = name match {
          case WLgenHtml.NORMAL_SUMMARY_FILE_NAME => "Summary Image"
          case WLgenHtml.BRIGHT_SUMMARY_FILE_NAME => "Summary Image Brightened"
          case WLgenHtml.ORIGINAL_FILE_NAME       => "Entire Image"
          case _                                  => "Image"
        }
        val id: String = ir.subDirName
        val url = relUrl(ir) + "/" + name
        val script = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

        <div>
          <span> {title} </span>
          <script> {script} </script>
          <a href={url}>
            <div class='zoom' id={id}>
              <img width={Config.WLSummarySize.toString} src={url}/>
            </div>
         </a>
        </div>
      }

      val laserHtml: Option[Elem] = {
        val laserCor = WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir)
        if (laserCor.isDefined)
          Some(<td>{laserCorrectionToHtml(laserCor.get)}</td>)
        else
          None
      }

      val imageHtml = {
        <td>
          <center> {
          0 match {
            case _ if canRead(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.NORMAL_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.NORMAL_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.ORIGINAL_FILE_NAME, ir) => img(WLgenHtml.ORIGINAL_FILE_NAME)

            case _ => <span>No Image Available</span>
          }
        }
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
          <td style={style}> {name} </td>
          <td style={style}> {(value.formatted("%6.2f") + "mm").trim} </td>
          <td style={style}> {instr} </td>
          <td style={style}> {corrStatus} </td>
        </tr>
      }

      <center>
        Laser Corrections
        <br/>
        Tolerance: {Config.WLLaserCorrectionLimit}
        <table border="1" width="350">
          <tr bgcolor='dddddd'>
            <td style={style}>Axis</td>
            <td style={style}>Offset</td>
            <td style={style}>Correct by moving</td>
            <td style={style}>Status</td>
          </tr>
          {row("Longitudinal", correction.longitudinal, new Instruction("away from gantry", "toward gantry"))}
          {row("Lateral", correction.lateral, new Instruction("towards left when facing gantry", "towards right when facing gantry"))}
          {row("Vertical", correction.vertical, new Instruction("towards floor", "towards ceiling"))}
        </table>
      </center>
    }

    def csvLink(): Elem = {
      <a title="Results as spreadsheet/CSV" href={csvFileName}>Results</a>
    }

    def html(resultList: Seq[WLImageResult]): String = {

      val offsets: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr>
            <td> Tongue &amp; Groove Offsets dX = 0.0 dY = 0.0 </td>
          </tr>
          <tr>
            <td> Radial Offset Tolerance, Rtol = {Config.WLPassLimit} mm </td>
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
          <td></td>
          <td>{offsets}</td>
          <td></td>
          </tr>
        </table>
      }

      val readyForEvaluationNote: String = {
        ""
      }

      val badPixelsToHtml: Elem = {
        <div title='Number of unique bad pixels in portal imager'>Bad Pixels: 0</div>
      }

      val headTable2: Elem = {
        <table border='0' style="border-collapse:separate; border-spacing:0.5em;">
          <tr>
            <td> {csvLink()} </td>
            <td> {badPixelsToHtml} </td>
            <td> {resultList.size}  images</td>
            <td> {irThumbImageListHtml} </td>
          </tr>
        </table>
      }

      val content: Elem = {
        <div>
          {headTable2}
          {headTable1}
          {readyForEvaluationNote}
          {imageHtml}
        </div>
      }

      val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "Winston Lutz")
      text
    }

    //Log.get.finest("Generated html: \n\n" + html + "\n\n")
    val htmlText = html(resultList)
    htmlText
  }

}
