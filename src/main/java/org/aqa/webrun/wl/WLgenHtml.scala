package org.aqa.webrun.wl

import org.aqa.webrun.ExtendedData
import org.aqa.Config

import java.io.File
import java.io.FileOutputStream
import java.util.Date
import scala.xml.Elem
import scala.xml.PrettyPrinter

object WLgenHtml {

  val IMAGE_FILE_SUFFIX = ".png"
  private val HTML_PREFIX_TAG = "%%%%HTML_PREFIX%%%%"
  private val BAD_PIXEL_FILE_NAME = "badPixels" + IMAGE_FILE_SUFFIX
  val NORMAL_SUMMARY_FILE_NAME = "normalSummary" + IMAGE_FILE_SUFFIX
  val BRIGHT_SUMMARY_FILE_NAME = "brightSummary" + IMAGE_FILE_SUFFIX
  val ORIGINAL_FILE_NAME = "original" + IMAGE_FILE_SUFFIX
  val BAD_PIXEL_FILE_NAME = "badPixels" + IMAGE_FILE_SUFFIX
  val DIAGNOSTICS_HTML_FILE_NAME = "diagnostics.html"
  val MAIN_HTML_FILE_NAME = "index.html"

  def prettyPrint(elem: Elem): String = new PrettyPrinter(1024, 2).format(elem)

  def code2Html(src: String): String = {
    val NL = "@@NL@@"
    val textNewLine = src.replaceAll("""[\012\015][\012\015]*""", NL)
    val elem: Elem = <code>
      {textNewLine}
    </code>
    val escapedText = prettyPrint(elem)
    escapedText.replaceAll(NL, "<br/>\n")
  }

  def generateHtml(extendedData: ExtendedData, imageResult: WLImageResult): Unit = {
    val vs = "<p/><br/>" // vertical space




    val HTML_PREFIX = {
      """
            <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
            <!--[if lt IE 9]>
                <script src="/static/bootstrap/html5shiv/3.7.0/html5shiv.js"></script>
                <script src="/static/bootstrap/libs/respond/1.4.2/respond.min.js"></script>
            <![endif]-->
            <script src='/static/jquery.min.js'></script>
            <script src='/static/jquery.zoom.js'></script>
            <script src='/static/jquery.timeago.js'></script>
            <script src='/static/tooltip.js'></script>
            <script>
                $(document).ready(function(){ $('#ex1').zoom(); });
                jQuery(document).ready(function() {jQuery('abbr.timeago').timeago();})
            </script>
            <link rel='stylesheet' href='/static/WLQA.css'/>
        """
    }

    //val relativeUrl = subDir.getAbsolutePath.substring(Config.DataDirectory.length + 1).replace('\\', '/')

    def img(name: String): String = {
      val shortName = if (name.endsWith(IMAGE_FILE_SUFFIX)) name.substring(0, name.length - IMAGE_FILE_SUFFIX.length) else name
      val longName = shortName + IMAGE_FILE_SUFFIX
      if (new File(extendedData.output.dir, longName).exists) {
        "<a href='" + longName + "'><img title='" + shortName + "' src='" + longName + "'></a>\n"
      } else {
        "Image for " + longName + " does not exist"
      }
    }

    val diagnosticsText: String = {
      val diagFile = new File(extendedData.output.dir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME)
      if (diagFile.exists) {
        val text = scala.io.Source.fromFile(diagFile).mkString
        val textHtml = code2Html(text)
        "<pre style='background: #eeeeee'; font-size: small>\n" + textHtml + "</pre><p/>\n"
      } else "Diagnostics file " + WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME + " does not exist"
    }

    val badPixelImage: String = {
      val badPixelImgFile = new File(extendedData.output.dir, BAD_PIXEL_FILE_NAME)
      if (!badPixelImgFile.exists)
        ""
      else {
        vs + vs + "Entire image with bad pixels highlighted and circled<br/>in red and marginal ones in yellow<br/>\n" + img("badPixels") + "<p/>\n"
      }
    }

    val imageTitle = "Entire image" + (if (imageResult.badPixelList.isEmpty) "" else " with bad pixels corrected")
    val originalImage: String = vs + vs + imageTitle + "<br/>\n" + img("original") + "<p/>\n"

    val summaryWithEdges: String = {
      val w = Config.WLSummarySize
      vs + vs + "Summary with Edges<p/>\n" +
        "<table>\n" +
        "<tr>\n" +
        "<td>\n" +
        "</td>\n" +
        "<td align='center'>\n" +
        img("edge_top") +
        "</td>\n" +
        "<td>\n" +
        "</td>\n" +
        "</tr>\n" +
        "<tr>\n" +
        "<td align='center'>\n" +
        img("edge_left") +
        "</td>\n" +
        "<td align='center'>\n" +
        img("normalSummary") +
        "</td>\n" +
        "<td align='center'>\n" +
        img("edge_right") +
        "</td>\n" +
        "</tr>\n" +
        "<tr>\n" +
        "<td>\n" +
        "</td>\n" +
        "<td align='center'>\n" +
        img("edge_bottom") +
        "</td>\n" +
        "<td>\n" +
        "</td>\n" +
        "</tr>\n" +
        "</table>\n" +
        "<p/>\n"
    }

    val background: String = {
      val w = Config.WLSummarySize
      vs + vs + "Background Surrounding Ball<p/>\n" +
        "<table cellpadding='10'>\n" +
        "<tr>\n" +
        "<td align='center'>\n" +
        "Before Normalization<p/>\n" +
        img("ball_background") +
        "</td>\n" +
        "<td align='center'>\n" +
        "After Normalization<p/>\n" +
        img("normalized_ball_background") +
        "</td>\n" +
        "</tr>\n" +
        "</table>\n" +
        "<p/>\n"
    }

    val ballStages: String = {
      val w = Config.WLSummarySize
      vs + vs + "Location of Ball<p/>\n" +
        "<table cellpadding='10'>\n" +
        "<tr>\n" +
        "<td align='center'>\n" +
        "Before Normalization<p/>\n" +
        img("ball_before_normalization") +
        "</td>\n" +
        "<td align='center'>\n" +
        "After Normalization Coarse Location<p/>\n" +
        img("ball_coarse") +
        "</td>\n" +
        "<td align='center'>\n" +
        "Fine Location<p/>\n" +
        img("ball_fine") +
        "</td>\n" +
        "</tr>\n" +
        "</table>\n" +
        "<p/>\n"
    }

    val brightSummary: String = {
      "<p>Summary brightened to better show ball<p>\n" +
        img("brightSummary")
    }

    val html: String = "<!DOCTYPE html>\n" +
      "<html>\n" +
      "<head>\n" +
      HTML_PREFIX + "\n" +
      "</head>\n" +
      "<body>\n" +
      "<a href='/" + MAIN_HTML_FILE_NAME + "'>Home</a><p/>\n" +
      "<center>\n" +
      "<h2>Diagnostics<p/>" + imageMetaData.getNameHtml(groupStartTime) + "</h2><p/>\n" +
      "<h2><p>" + JobList.passedText(imageResult, imageMetaDataGroup) + "</p></h2>\n" +
      "<a title='Go back to report' href='../" + JobList.REPORT_FILE_NAME + "'>Report</a>" +
      " &nbsp; &nbsp; &nbsp; &nbsp; " +
      "<a title='Download original DICOM image' href='../../" + imageMetaData.SliceUID + JobList.DICOM_SUFFIX + "'>DICOM</a>" +
      " &nbsp; &nbsp; &nbsp; &nbsp; " +
      "<a title='View original DICOM formatted as text' href='" + imageMetaData.SliceUID + ".txt'>DICOM as text</a>" +
      "<p/>\n" +
      "Treatment " + JobList.timeAndTimeAgo(imageMetaData.CreationDate) +
      " &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;\n" +
      "Analysis " + JobList.timeAndTimeAgo(new Date) + "<p/>\n" +
      "</center>\n" +
      diagnosticsText +
      "<center>\n" +
      badPixelImage +
      originalImage +
      summaryWithEdges +
      brightSummary +
      background +
      ballStages +
      "</center>\n" +
      "</body>\n" +
      "</html>"

    val fos = new FileOutputStream(new File(subDir, JobList.DIAGNOSTICS_HTML_FILE_NAME))
    fos.write(html.getBytes)
    fos.close()
  }

}
