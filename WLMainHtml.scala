package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.display.ConsumerFormatImageMaker
import edu.umro.util.Exec
import edu.umro.util.OpSys
import edu.umro.util.Utility
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import javafx.print.PrinterJob.JobStatus
import org.aqa.Config
import resource.managed

import java.awt.Color
import java.io.Closeable
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.StringWriter
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import javax.imageio.ImageIO
import scala.collection.mutable.HashMap
import scala.xml.Elem
import scala.xml.PrettyPrinter
object WLMainHtml {
  private def spaces(count: Int): String = (0 to (count / 2)).foldLeft(" ")((t, c) => t + "&nbsp; ")

   def generateJobHtml(resultList: List[WLImageResult], startTime: Long, imageMetaDataGroup: ImageMetaDataGroup): String = {
    val jobDir = resultList.head.directory.getParentFile
    val relativeUrl = jobDir.getAbsolutePath.substring(Config.DataDirectory.getAbsolutePath.length + 1).replace('\\', '/')
    val start = <p></p>

    def relUrl(ir: WLImageResult) = ir.directory.getName

    def canRead(name: String, ir: WLImageResult): Boolean = new File(ir.directory, name).canRead

    val timeOf = new ImageMetaDataGroup(resultList.map(r => r.imageMetaData)).timeOf

    val laserCorrectionList = LaserCorrection.setList(resultList)

    val readyForEvaluation = if (jobStatus(resultList) == JobStatus.ReadyForEvaluation) "*" else ""

    def irTextHtml(ir: WLImageResult): String = {
      def fmt(value: Double): String = value.formatted("%6.2f").replaceAll(" ", "")

      val diagnostics: String = {
        if (canRead(WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME, ir))
          "<a href='" + relUrl(ir) + "/" + WLgenHtml. DIAGNOSTICS_HTML_FILE_NAME + "'>Diagnostics</a>\n"
        else
          "<a href='" + relUrl(ir) + "'>Generated Files</a>\n"
      }

      val badPixels: String = {
        if (ir.badPixelList.isEmpty) ""
        else {
          " &nbsp; &nbsp; &nbsp; Bad Pixels: " + ir.badPixelList.size
        }
      }

      val laserHtml = if (LaserCorrection.getCorrectionOfImage(laserCorrectionList, ir).isDefined) "<td/>" else "";

      laserHtml +
        "<td style='background: #eeeeee'><center>\n" +
        "<h2 title='Gantry angle, collimator angle, and time since start'>" + ir.imageMetaData.getNameHtml(timeOf) + "</h2><p> </p>\n" +
        "Offset in mm &nbsp; X = " + fmt(ir.offX) + " &nbsp; &nbsp; &nbsp; Y = " + fmt(ir.offY) + "<p> </p>\n" +
        "R = " + fmt(ir.offXY) + " &nbsp; &nbsp; &nbsp; " + passedText(ir, imageMetaDataGroup) + "<p> </p>\n" +
        diagnostics +
        badPixels +
        "</center></td>\n"
    }

    def irThumbImageHtml(ir: WLImageResult): String = {
      val color = if (ir.imageStatus == ImageStatus.Passed) Config.WLPassColor.toHtml else Config.WLFailColor.toHtml
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
          case WLgenHtml.ORIGINAL_FILE_NAME => "Entire Image"
          case _ => "Image"
        }
        val norm = name == WLgenHtml.NORMAL_SUMMARY_FILE_NAME
        val id = ir.imageMetaData.getDirName(timeOf) + name.replaceAll("[^A-Z0-9a-z]", "_")
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
        val laserCor = LaserCorrection.getCorrectionOfImage(laserCorrectionList, ir)
        if (laserCor.isDefined) {
          "<td>" + laserCorrectionToHtml(laserCor.get) + "</td>\n"
        } else ""
      }

      val imageHtml = {
        "<td> <center>\n" + {
          0 match {
            case _ if canRead(BRIGHT_SUMMARY_FILE_NAME, ir) => img(BRIGHT_SUMMARY_FILE_NAME)

            /*
                        case _ if canRead(NORMAL_SUMMARY_FILE_NAME, ir) && canRead(BRIGHT_SUMMARY_FILE_NAME, ir) => {
                            img(NORMAL_SUMMARY_FILE_NAME) + "\n<p></p>\n" + img(BRIGHT_SUMMARY_FILE_NAME)
                        }
             */

            case _ if canRead(NORMAL_SUMMARY_FILE_NAME, ir) => img(NORMAL_SUMMARY_FILE_NAME)

            case _ if canRead(ORIGINAL_FILE_NAME, ir) => img(ORIGINAL_FILE_NAME)

            case _ => "No Image Available"
          }

        } + "\n" +
          "    </center>\n    </td>\n"
      }
      laserHtml + imageHtml
    }

    def laserCorrectionToHtml(correction: LaserCorrection): String = {
      class Instruction(val pos: String, val neg: String);

      def row(name: String, value: Double, instruction: Instruction): String = {
        val instr = if (value >= 0) instruction.pos else instruction.neg
        val corrStatus = if (value.abs < Config.WLLaserCorrectionLimit) "<passed>&nbsp;" + correction.passedText + "&nbsp;</passed>" else "<caution>&nbsp;" + correction.failedText + "&nbsp;</caution>"
        "<tr><td>" + name + "</td><td>" + value.formatted("%6.2f mm").trim + "</td><td>" + instr + "</td><td>" + corrStatus + "</td></tr>\n";
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
        LaserCorrection.getCorrectionOfImage(laserCorrectionList, ir) match {
          case Some(laserCorr) => {
            "<center>\n" +
              "Laser Corrections<br/>" + "tolerance: " + Config.WLLaserCorrectionLimit.formatted("%4.2f mm") + "\n" +
              laserCorrectionToHtml(laserCorr) + "\n" +
              "</center>\n"

          }
          case _ => ""
        }
      } +
        "</td>\n"
    }

    def dicomLink(passed: Boolean): String = {
      val name = if (passed) "DICOM" else "DICOM (not sent)"
      "<p><a title='Download generated DICOM file' href='" + DICOM_FILE_NAME + "'>" + name + "</a>\n"
    }

    def dicomImage: String = {
      "<a title='View generated DICOM file as image' href='" + DICOM_IMAGE_FILE_NAME + "'>Image</a>\n"
    }

    def csvLink(ir: WLImageResult): String = {
      "<a title='Results as spreadsheet friendly CSV (comma separated values)' href='" + ir.directory.getParentFile.getParentFile.getName + CSV_FILE_SUFFIX + "'>Results</a>\n"
    }

    def html(imageMetaDataGroup: ImageMetaDataGroup): String = {
      val machId = if (imageMetaDataGroup.treatmentMachine.isDefined) imageMetaDataGroup.treatmentMachine.get.MachineId else "unknown"
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
          "<td><h1>" + jobStatusDescription(resultList) + "</h1></td>\n" +
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
          val date = new Date(startTime)
          val elapsed = System.currentTimeMillis - startTime
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
          "<tr><td>Tongue &amp; Groove Offsets &nbsp; dX = " + {
          imageMetaDataGroup.treatmentMachine match {
            case Some(tm: TreatmentMachine) => tm.XCorrected + " mm &nbsp; dY = " + tm.YCorrected + " mm\n"
            case None => {
              "<br/><failed>Failure: Unknown treatment machine!</failed>" +
                "<p> </p>Known machines are:<br> &nbsp; &nbsp; " + Config.treatmentMachineNameList + "<p> </p>\n" +
                "<br>Note: this can happen when machine is run in QA mode instead" +
                "<br/>of treatment mode, or a machine is not in the configuration" +
                "<br/>file for this service.  This also means that there were no tongue" +
                "<br/>and groove offset calculations.\n"
            }
          }
        } +
          "</td></tr>\n" +
          "<tr><td>" +
          "Radial Offset Tolerance, Rtol = " + Config.PassLimit + " mm\n" +
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
        if (jobStatus(resultList) == JobStatus.ReadyForEvaluation)
          "<h3><font color='red'>based on collimator angles from DICOM image headers / GgggCccc field name encoding</font></h3>\n"
        else ""
      }

      val badPixelsToHtml: String = {
        "<div title='Number of unique bad pixels in portal imager'>Bad Pixels: " + numBadPixles(resultList) + "</div>"
      }

      val headTable2: String = {
        val SP = 12
        "<table border='0' cellspacing='4'  cellpadding='0'>\n" +
          "<tr>\n" +
          "<td>" + dicomLink(jobStatus(resultList) != JobStatus.Failed) + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + dicomImage + "</td>\n" +
          "<td>" + spaces(SP) + "</td>\n" +
          "<td>" + csvLink(resultList(0)) + "</td>\n" +
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
    html(imageMetaDataGroup)
  }


}
