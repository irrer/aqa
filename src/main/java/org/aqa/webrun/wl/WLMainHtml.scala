package org.aqa.webrun.wl

import org.aqa.web.WebUtil.timeAgo
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.awt.Color
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

object WLMainHtml extends Logging {

  private def spaces(count: Int): String = (0 to (count / 2)).foldLeft(" ")((t, _) => t + "&nbsp; ")

  private val fileDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")
  private val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  private val DEFAULT_DATE = standardDateFormat.parse("2000-01-01T00:00:00")
  private val CSV_FILE_SUFFIX = ".csv"
  private val MAIN_HTML_FILE_NAME = "/WinstonLutz.html"

  // Format date in human friendly HTML way
  private def fmtDate(date: Date): String = {
    val format = new SimpleDateFormat("EEE dd MMM YYYY  h:mm aa")
    format.format(date)
  }

  val HTML_PREFIX: String = {
    """
          <link rel="icon" href="/static/images/favicon.ico?" type="image/x-icon"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap-theme.min.css"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/min/dropzone.min.css"/>
          <link rel="stylesheet" href="/static/AQA.css"/>
          <link href="/static/bootstrap/datetime/bootstrap-datetimepicker.min.css" rel="stylesheet" media="screen"/>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/js/bootstrap.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/min/dropzone.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-timeago/1.5.4/jquery.timeago.min.js"></script>
          <script src="/static/tooltip/tooltip.js"></script>
          <script type="text/x-mathjax-config">MathJax.Hub.Config({ tex2jax: { inlineMath: [['$','$'], ['\\(','\\)']] } });</script>
          <script type="text/x-mathjax-config">MathJax.Hub.Config({ TeX: { extensions: ['autobold.js', 'AMSsymbols.js'] } });</script>
          <script type="text/javascript" src="/static/mathjax.js?config=TeX-AMS-MML_HTMLorMML"></script>
          <script src="/static/zoom/jquery.zoom.js"></script>
          <script src="/static/AQA.js"></script>
          <script>
              $(document).ready(function(){ $('#ex1').zoom(); });
              jQuery(document).ready(function() {jQuery('abbr.timeago').timeago();})
          </script>
          <link rel='stylesheet' href='/static/WLQA.css'/>
      """
  }

  def generateGroupHtml(extendedData: ExtendedData, resultList: Seq[WLImageResult]): String = {
    val jobDir = resultList.head.directory.getParentFile
    // val relativeUrl = jobDir.getAbsolutePath.substring(Config.DataDirectory.getAbsolutePath.length + 1).replace('\\', '/')

    val groupStartTime = resultList.map(_.contentTime).minBy(_.getTime)

    def relUrl(ir: WLImageResult) = ir.directory.getName

    def canRead(name: String, ir: WLImageResult): Boolean = new File(ir.directory, name).canRead

    def angleRoundedTo22_5(angle: Double): Double = (((angle + 3600) / 22.5).round.toInt % 16) * 22.5 // convert to nearest multiple of 22.5 degrees

    def fmtGantryAngle(angle: Double) = angleRoundedTo22_5(angle)

    def fmtCollAngle(angle: Double): Int = Util.angleRoundedTo90(angle)

    def fmtTime(ir: WLImageResult): String = {
      val totalSeconds = (ir.contentTime.getTime - groupStartTime.getTime) / 1000
      "" + (totalSeconds / 60) + ":" + (totalSeconds % 60).formatted("%02d")
    }

    def getNameHtml(ir: WLImageResult): String = {
      "G" + fmtGantryAngle(ir.gantryAngle) + spaces(4) + " C" + fmtCollAngle(Util.collimatorAngle(ir.rtimage)) + spaces(4) + fmtTime(ir)
    }

    def getName(ir: WLImageResult): String = {
      "G" + fmtGantryAngle(ir.gantryAngle) + "   C" + fmtCollAngle(ir.collimatorRounded_deg) + "    " + fmtTime(ir)
    }

    def getCareEventStartDate(dir: File): Date = {
      try {
        fileDateFormat.parse(dir.getName)
      } catch {
        case e: Exception =>
          logger.error("Unable to get treatment date for directory " + dir.getAbsolutePath + ", using current date instead: " + fmtEx(e))
          DEFAULT_DATE
      }
    }

    // val timeOf = new ImageMetaDataGroup(resultList.map(r => r.imageMetaData)).timeOf

    val laserCorrectionList = WLLaserCorrection.setList(resultList)

    // val readyForEvaluation = if (jobStatus(resultList) == JobStatus.ReadyForEvaluation) "*" else ""

    def irTextHtml(ir: WLImageResult): String = {
      def fmt(value: Double): String = value.formatted("%6.2f").replaceAll(" ", "")

      val diagnostics: String = {
        if (canRead(WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME, ir))
          "<a href='" + relUrl(ir) + "/" + WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME + "'>Diagnostics</a>\n"
        else
          "<a href='" + relUrl(ir) + "'>Generated Files</a>\n"
      }

      val badPixels: String = {
        if ((ir.badPixelList == null) || ir.badPixelList.isEmpty) ""
        else {
          " &nbsp; &nbsp; &nbsp; Bad Pixels: " + ir.badPixelList.size
        }
      }

      val laserHtml = if (WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir).isDefined) "<td/>" else ""

      def getNameHtml(ir: WLImageResult): String = {
        ir.gantryRounded_txt + spaces(4) + ir.collimatorRounded_txt + spaces(4) + fmtTime(ir)
      }

      def passedText(ir: WLImageResult) = {
        if (ir.imageStatus == ImageStatus.Passed)
          "<passed>&nbsp; PASSED &nbsp;</passed>"
        else "<failed>&nbsp;" + ir.imageStatus + " &nbsp;</failed>"
      }

      laserHtml +
        "<td style='background: #eeeeee'><center>\n" +
        "<h2 title='Gantry angle, collimator angle, and time since start'>" + getNameHtml(ir) + "</h2><p> </p>\n" +
        "Offset in mm &nbsp; X = " + fmt(ir.offX) + " &nbsp; &nbsp; &nbsp; Y = " + fmt(ir.offY) + "<p> </p>\n" +
        "R = " + fmt(ir.offXY) + " &nbsp; &nbsp; &nbsp; " + passedText(ir) + "<p> </p>\n" +
        diagnostics +
        badPixels +
        "</center></td>\n"
    }

    def toHtml(color: Color): String = {
      "#" + (color.getRGB & 0xffffff).formatted("%06x")
    }

    def irThumbImageHtml(ir: WLImageResult): String = {
      val color = if (ir.imageStatus == ImageStatus.Passed) toHtml(Config.WLPassColor) else toHtml(Config.WLFailColor)
      "<td height='20' width='20' style='background:" + color + ";forground:#" + color + ";'> </td>\n"
    }

    def irThumbImageListHtml: String = {
      "<table border='0' cellspacing='4'   cellpadding='0'><tr>\n" + {
        resultList.map(ir => irThumbImageHtml(ir)).foldLeft("")((a, b) => a + b)
      } + "\n" +
        "</tr>\n" +
        "</table>\n"
    }

    def irImageHtml(ir: WLImageResult): String = {
      def img(name: String): String = {
        val title = name match {
          case WLgenHtml.NORMAL_SUMMARY_FILE_NAME => "Summary Image"
          case WLgenHtml.BRIGHT_SUMMARY_FILE_NAME => "Summary Image Brightened"
          case WLgenHtml.ORIGINAL_FILE_NAME       => "Entire Image"
          case _                                  => "Image"
        }
        val norm = name == WLgenHtml.NORMAL_SUMMARY_FILE_NAME
        val id = ir.subDirName
        val url = relUrl(ir) + "/" + name
        title +
          "    <script>\n" +
          "        $(document).ready(function(){ $('#" + id + "').zoom(); });\n" +
          "    </script>\n" +
          "    <a href='" + url + "'>\n" +
          "        <div class='zoom' id='" + id + "'>\n" +
          "            <img width='" + Config.WLSummarySize + "' src='" + url + "'/>\n" +
          "        </div>\n" +
          "    </a>\n"
      }

      val laserHtml = {
        val laserCor = WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir)
        if (laserCor.isDefined) {
          "<td>" + laserCorrectionToHtml(laserCor.get) + "</td>\n"
        } else ""
      }

      val imageHtml = {
        "<td> <center>\n" + {
          0 match {
            case _ if canRead(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.NORMAL_SUMMARY_FILE_NAME, ir) => img(WLgenHtml.NORMAL_SUMMARY_FILE_NAME)

            case _ if canRead(WLgenHtml.ORIGINAL_FILE_NAME, ir) => img(WLgenHtml.ORIGINAL_FILE_NAME)

            case _ => "No Image Available"
          }

        } + "\n" +
          "    </center>\n    </td>\n"
      }
      laserHtml + imageHtml
    }

    def laserCorrectionToHtml(correction: WLLaserCorrection): String = {
      class Instruction(val pos: String, val neg: String)

      def row(name: String, value: Double, instruction: Instruction): String = {
        val instr = if (value >= 0) instruction.pos else instruction.neg
        val corrStatus = if (value.abs < Config.WLLaserCorrectionLimit) "<passed>&nbsp;" + correction.passedText + "&nbsp;</passed>" else "<caution>&nbsp;" + correction.failedText + "&nbsp;</caution>"
        "<tr><td>" + name + "</td><td>" + value.formatted("%6.2f mm").trim + "</td><td>" + instr + "</td><td>" + corrStatus + "</td></tr>\n"
      }

      "<center>\n" +
        "Laser Corrections<br/>\n" +
        "Tolerance: " + Config.WLLaserCorrectionLimit + "\n" +
        "<table frame=hsides rules=rows cellpadding='8' cellspacing='20' border='0' width='350'>\n" +
        "<tr bgcolor='dddddd'><td>Axis</td><td>Offset</td><td>Correct by moving</td><td>Status</td></tr>\n" +
        row("Longitudinal", correction.longitudinal, new Instruction("away from gantry", "toward gantry")) +
        row("Lateral", correction.lateral, new Instruction("towards left when facing gantry", "towards right when facing gantry")) +
        row("Vertical", correction.vertical, new Instruction("towards floor", "towards ceiling")) +
        "</table>\n" +
        "</center>\n"
    }

    def irCorrectionHtml(ir: WLImageResult): String = {
      "<td>" + {
        WLLaserCorrection.getCorrectionOfImage(laserCorrectionList, ir) match {
          case Some(laserCorr) =>
            "<center>\n" +
              "Laser Corrections<br/>" + "tolerance: " + Config.WLLaserCorrectionLimit.formatted("%4.2f mm") + "\n" +
              laserCorrectionToHtml(laserCorr) + "\n" +
              "</center>\n"
          case _ => ""
        }
      } +
        "</td>\n"
    }

    def csvLink(ir: WLImageResult): String = {
      "<a title='Results as spreadsheet friendly CSV (comma separated values)' href='" + ir.directory.getParentFile.getParentFile.getName + CSV_FILE_SUFFIX + "'>Results</a>\n"
    }

    def html(resultList: Seq[WLImageResult]): String = {
      val machId = extendedData.machine.id

      val allPassed = {
        val statusTypeList = resultList.map(_.imageStatus).distinct
        (statusTypeList.size == 1) && statusTypeList.head.equals(ImageStatus.Passed)
      }

      def allImagesPassed(): ImageStatus.ImageStatus =
        resultList.foldLeft(ImageStatus.Passed)((s, i) => {
          if (allPassed)
            if (i.imageStatus == ImageStatus.Passed) ImageStatus.Passed
            else i.imageStatus
          else s
        })

      def jobStatusDescription(): String = {
        val statusTypeList = resultList.map(_.imageStatus).distinct
        if ((statusTypeList.size == 1) && statusTypeList.head.equals(ImageStatus.Passed)) {
          "<passed> &nbsp; " + allImagesPassed().toString.toUpperCase + " &nbsp; </passed>"
        } else {
          "<failed> &nbsp; " + allImagesPassed().toString + " &nbsp; </failed>"
        }
      }

      val head = {
        "<!DOCTYPE html>\n" +
          "<html>\n" +
          "<head>\n" +
          HTML_PREFIX + "\n" +
          "<title>" + machId + " " + fmtDate(getCareEventStartDate(jobDir.getParentFile)) + "</title>\n" +
          "</head>\n" +
          "<body>\n" +
          "<a href='/" + MAIN_HTML_FILE_NAME + "'>Home</a>\n" +
          "<center>\n" +
          "Stereotactic RadioSurgery Winston-Lutz Test Quality Assurance Report<p> </p>\n" +
          "<table  border='0' cellspacing='4' cellpadding='0'>\n" +
          "<tr valign='center'>\n" +
          "<td><h1>Machine " + machId + " &nbsp; </h1></td>\n" +
          "<td><h1>" + jobStatusDescription() + "</h1></td>\n" +
          "</tr>\n" +
          "</table>\n"
      }

      val times: String = {
        def treatment: String = {
          val date = getCareEventStartDate(jobDir.getParentFile)
          "<tr>\n" +
            "<td>Treatment</td>" +
            "<td>" + timeAgo(date) + "</td>\n" +
            "<td></td>\n" +
            "</tr>\n"
        }

        def analysis: String = {
          val date = extendedData.output.analysisDate.get
          val elapsed = System.currentTimeMillis - date.getTime
          val elapsedDateFormat = new SimpleDateFormat("m:ss")

          "<tr>\n" +
            "<td title='Start time of analysis'>Analysis</td>" +
            "<td>" + timeAgo(date) + "</td>\n" +
            "<td title='Calculation time'>" + elapsedDateFormat.format(elapsed) + "</td>\n" +
            "</tr>\n"
        }

        "<table border='0'  cellspacing='4' cellpadding='0'>\n" +
          treatment +
          analysis +
          "</table>\n"
      }

      val offsets: String = {
        "<table border='0'   cellspacing='4' cellpadding='0'>\n" +
          "<tr><td>Tongue &amp; Groove Offsets &nbsp; dX = 0.0 &nbsp; &nbsp; &nbsp; dY = 0.0 " +
          "</td></tr>\n" +
          "<tr><td>" +
          "Radial Offset Tolerance, Rtol = " + Config.WLPassLimit + " mm\n" +
          "</td></tr>\n" +
          "</table>\n"
      }

      val imageHtml: String = {
        "<table border='0' cellspacing='20' cellpadding='20'>\n" +
          "<tr style='background: #eeeeee'>\n" + {
          resultList.map(ir => irTextHtml(ir)).foldLeft("")((a, b) => a + b)
        } + "\n" +
          "</tr>\n" +
          "<tr>\n" + {
          resultList.map(ir => irImageHtml(ir)).foldLeft("")((a, b) => a + b)
        } + "\n" +
          "</tr>\n" +
          "</table>\n"
      }

      val headTable1: String = {
        "<table   border='0' cellspacing='4' cellpadding='0'>\n" +
          "<tr>\n" +
          "<td>" + times + "</td>\n" +
          "<td>" + spaces(18) + "</td>\n" +
          "<td>" + offsets + "</td>\n" +
          "<td>" + spaces(18) + "</td>\n" +
          "</tr>\n" +
          "</table>\n"
      }

      val readyForEvaluationNote: String = {
        ""
      }

      val badPixelsToHtml: String = {
        "<div title='Number of unique bad pixels in portal imager'>Bad Pixels: 0</div>"
      }

      val headTable2: String = {
        val SP = 12
        "<table border='0' cellspacing='4'  cellpadding='0'>\n" +
          "<tr>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + csvLink(resultList.head) + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + badPixelsToHtml + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + resultList.size + " images</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + irThumbImageListHtml + "</td>\n" +
          "</tr>\n" +
          "</table>\n"
      }

      head +
        headTable2 +
        headTable1 +
        readyForEvaluationNote +
        imageHtml +
        "</center>\n" +
        "</body>\n" +
        "</html>"
    }

    //Log.get.finest("Generated html: \n\n" + html + "\n\n")
    val htmlText = html(resultList)
    htmlText
  }

}
