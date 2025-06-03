package org.aqa.webrun.wl

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

    val edgeSetHtml: Elem = {

      def edgeHtml(edge: WLEdge): Elem = {
        val nl = WebUtil.titleNewline
        val title = //
          s"The profile of the edge is calculated by finding the 50%$nl" +
            s"gradient point for each column or row of pixels.$nl$nl" +
            s"It shows how 'even' the edge is, and can be indicative$nl" +
            s"of the transparency of the stem supporting the ball phantom.$nl$nl" +
            s"The coefficient of variation of the profile is a numerical$nl" +
            s"representation of the evenness of the edge."
        <tr title={title}>
          <td>
            <h4>{edge.name}</h4>
            Profile Coef Of Var: {Util.fmtDbl(edge.edgeProfileProfileCoefficientOfVariation)}
            <br>
              Image with Gradient
            </br>
            <br>
              <img src={"edge_" + edge.name + ".png"} height="200" />
            </br>
          </td>
          <td>
            <h4>Edge Profile</h4>
            {edge.edgeProfileChart.html}
          </td>
        </tr>
      }

      <div>
        <table class="table table-bordered" style="text-align: center;">
          {edgeHtml(imageResult.edgeSet.get.top)}
          {edgeHtml(imageResult.edgeSet.get.bottom)}
          {edgeHtml(imageResult.edgeSet.get.left)}
          {edgeHtml(imageResult.edgeSet.get.right)}
        </table>
      </div>

    }

    val diagnosticsText: Elem = {
      val diagnosticFile = new File(subDir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME)
      if (diagnosticFile.exists) {
        val text = FileUtil.readTextFile(diagnosticFile).right.get
        <pre style='background: #eeeeee; font-size: small'>
          {text}
        </pre>
      } else
        <span>Diagnostics file {WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME}  does not exist</span>
    }

    val badPixelImage: Elem = {
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

    val originalImage: Elem = {
      <div>
        <p/>
        <h4 style="margin-top:50px;">{imageTitle}</h4>
        <br/>
        {img2("original")}
        <p/>
      </div>
    }

    val summaryWithEdges: Elem = {
      <div>
         <h4 style="margin-top:50px;">Summary with Edges</h4>
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

    val background: Elem = {
      val cssStyle = Some("margin:50px;")
      <div>
          <h4 style="margin-top:50px;">Background Surrounding Ball</h4>
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

    val ballStages: Elem = {
      val csStyle = Some("margin:50px;")
      <div>
        <h4 style="margin-top:50px;">Location of Ball</h4>
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

    val brightSummary: Elem = {
      <div>
        <h4 style="margin-top:50px;">Summary brightened to better show ball</h4>
        {img2("brightSummary")}
      </div>
    }

    val statusText: Elem = {
      if (imageResult.imageStatus == WLImageStatus.Passed)
        <passed style="color:#000000; background:#1dc32b;"> PASSED </passed>
      else
        <failed style="color:#000000; background:#e00034;"> {imageResult.imageStatus} </failed>
    }

    val html: String = {

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
          {badPixelImage}
          {originalImage}
          {summaryWithEdges}
          {brightSummary}
          {background}
          {ballStages}
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
          {edgeSetHtml}
          {diagnosticsText}
          {images}
          <br> </br>
          <p style="margin-bottom:200px;"> </p>
        </div>
      }

      val runScript = {
        val edgeSet = imageResult.edgeSet.get
        val js = Seq( //
          edgeSet.top.edgeProfileChart.javascript,
          edgeSet.bottom.edgeProfileChart.javascript,
          edgeSet.left.edgeProfileChart.javascript,
          edgeSet.right.edgeProfileChart.javascript
        ).mkString("\n")

        s"<script>\n$js\n</script>"
      }

      val text = WebUtil.wrapBody(content = ExtendedData.wrapExtendedData(extendedData, diagnosticsPage), pageTitle = wlMsg.imageName, c3 = true, runScript = Some(runScript))

      text
    }

    val fos = new FileOutputStream(new File(subDir, DIAGNOSTICS_HTML_FILE_NAME))
    fos.write(html.getBytes)
    fos.close()
  }

}
