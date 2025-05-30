package org.aqa.webrun.wl

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.FileUtil
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.web.WebUtil

import java.io.File
import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object WLgenHtml {

  val IMAGE_FILE_SUFFIX = ".png"
  private val BAD_PIXEL_FILE_NAME = "badPixels" + IMAGE_FILE_SUFFIX
  val NORMAL_SUMMARY_FILE_NAME: String = "normalSummary" + IMAGE_FILE_SUFFIX
  val BRIGHT_SUMMARY_FILE_NAME: String = "brightSummary" + IMAGE_FILE_SUFFIX
  val DIAGNOSTICS_HTML_FILE_NAME = "diagnostics.html"
  private val MAIN_HTML_FILE_NAME = "index.html"
  val RESULTS_DIRECTORY = "results"
  private val DICOM_SUFFIX = ".dcm"
  private val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  val ORIGINAL_FILE_NAME: String = "original" + IMAGE_FILE_SUFFIX

  // Format date in human friendly HTML way
  private def fmtDate(date: Date): String = {
    val format = new SimpleDateFormat("EEE dd MMM YYYY  h:mm aa")
    format.format(date)
  }

  private def timeAgo(date: Date): String = "<abbr class='timeago' title='" + standardDateFormat.format(date) + "'>" + fmtDate(date) + "</abbr>"

  // Format date and elapsed time in human friendly HTML way
  private def timeAndTimeAgo(date: Date): String = {
    fmtDate(date) + "\n" + " &nbsp; &nbsp; " + timeAgo(date)
  }

  def generateHtml(extendedData: ExtendedData, subDir: File, imageResult: WLImageResult, wlMsg: WLMessage): Unit = {

    def img2(name: String, cssStyle: Option[String] = None): Elem = {
      val shortName = if (name.endsWith(IMAGE_FILE_SUFFIX)) name.substring(0, name.length - IMAGE_FILE_SUFFIX.length) else name
      val longName = shortName + IMAGE_FILE_SUFFIX
      if (new File(subDir, longName).exists) {
        if (cssStyle.isDefined)
          <a href={longName} style={cssStyle.get}>
            <img title={shortName} src={longName}/>
          </a>
        else
          <a href={longName}>
            <img title={shortName} src={longName}/>
          </a>
      } else {
        <span>Image for {longName} does not exist</span>
      }
    }

    val diagnosticsText2: Elem = {
      val diagnosticFile = new File(subDir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME)
      if (diagnosticFile.exists) {
        val text = FileUtil.readTextFile(diagnosticFile).right.get
        <pre style='background: #eeeeee; font-size: small'>
          {text}
        </pre>
      } else
        <span>Diagnostics file {WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME}  does not exist</span>
    }

    val badPixelImage2: Elem = {
      val badPixelImgFile = new File(extendedData.output.dir, BAD_PIXEL_FILE_NAME)
      if (!badPixelImgFile.exists)
        <span> </span>
      else {
        <div>
          Entire image with bad pixels highlighted and circled
          <br>in red and marginal ones in yellow</br>
          {img2("badPixels")}
          <p></p>
        </div>
      }
    }

    val imageTitle = "Entire image" + (if ((imageResult.badPixelList == null) || imageResult.badPixelList.isEmpty) "" else " with bad pixels corrected")

    val originalImage2: Elem = {
      <div>
        <p/>
        {imageTitle}
        <br/>
        {img2("original")}
        <p/>
      </div>
    }

    val summaryWithEdges2: Elem = {
      <div>
          Summary with Edges<p/>
          <table>
            <tr>
              <td>
              </td>
              <td align='center'>
                {img2("edge_top")}
              </td>
              <td>
              </td>
            </tr>
            <tr>
              <td align='center'>
                {img2("edge_left")}
              </td>
              <td align='center'>
                {img2("normalSummary")}
              </td>
              <td align='center'>
                {img2("edge_right")}
              </td>
            </tr>
            <tr>
              <td>
              </td>
              <td align='center'>
                {img2("edge_bottom")}
              </td>
              <td>
              </td>
            </tr>
          </table>
          <p/>
        </div>

    }

    val background2: Elem = {
      val cssStyle = Some("margin:20px;")
      <div>
          Background Surrounding Ball
          <p/>
          <table cellpadding="10">
            <tr>
              <td align="center">
                Before Normalization<p/>
                {img2("ball_background", cssStyle)}
              </td>
              <td align="center">
                After Normalization<p/>
                {img2("normalized_ball_background", cssStyle)}
              </td>
            </tr>
          </table>
          <p/>
        </div>
    }

    val ballStages2: Elem = {
      val csStyle = Some("margin:20px;")
      <div>
        Location of Ball<p/>
        <table cellpadding="10">
          <tr>
            <td align="center">
              Before Normalization<p/>
              {img2("ball_before_normalization", csStyle)}
            </td>
            <td align="center">
              After Normalization Coarse Location<p/>
              {img2("ball_coarse", csStyle)}
            </td>
            <td align="center">
              Fine Location<p/>
              {img2("ball_fine", csStyle)}
            </td>
          </tr>
        </table>
        <p/>
      </div>
    }

    val brightSummary2: Elem = {
      <div>
        <p>Summary brightened to better show ball</p>
        {img2("brightSummary")}
      </div>
    }

    val statusText: Elem = {
      if (imageResult.imageStatus == WLImageStatus.Passed)
        <passed style="color:#000000; background:#1dc32b;"> PASSED </passed>
      else
        <failed style="color:#000000; background:#e00034;"> {imageResult.imageStatus} </failed>
    }

    val imageDate = {
      val date = Util.dicomGetTimeAndDate(imageResult.rtimage, TagByName.ContentDate, TagByName.ContentTime)
      date.get
    }

    val html2: String = {

      val mainReportRef = s"../${Output.displayFilePrefix}"
      val dicomRef = Util.sopOfAl(imageResult.rtimage) + DICOM_SUFFIX
      val dicomTextRef = Util.sopOfAl(imageResult.rtimage) + ".txt"

      val links = {
        <div>
            <a title="Go back to report" href={mainReportRef + ".html"}>Report</a>
            <a style="margin-left:40px;" title="Download original DICOM image" href={dicomRef}>DICOM</a>
            <a style="margin-left:40px;" title="View original DICOM formatted as text" href={dicomTextRef}>DICOM as text</a>
          </div>
      }

      val images = {
        <center>
          {badPixelImage2}
          {originalImage2}
          {summaryWithEdges2}
          {brightSummary2}
          {background2}
          {ballStages2}
        </center>
      }

      val diagnosticsPage = {
        <div>
          <a href={MAIN_HTML_FILE_NAME}>Home</a>
          <p> </p>
          <center>
            <h2>
              Details for Beam {wlMsg.imageName}
              <p> </p>
              <p> {statusText} </p>
            </h2>
            {links}
          </center>
          {diagnosticsText2}
          {images}
          <br> </br>
          <p style="margin-bottom:200px;"> </p>
        </div>
      }

      val text = WebUtil.wrapBody(content = ExtendedData.wrapExtendedData(extendedData, diagnosticsPage), pageTitle = wlMsg.imageName, c3 = true, runScript = None)

      text

      // html

    }

    val fos = new FileOutputStream(new File(subDir, DIAGNOSTICS_HTML_FILE_NAME))
    fos.write(html2.getBytes)
    fos.close()
  }

}
