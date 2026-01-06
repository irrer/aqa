package org.aqa.webrun.wl.nonCardinal

import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.wl.WLgenHtml
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLMessage
import org.aqa.Logging
import org.aqa.web.C3Chart
import org.aqa.Config

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.File
import javax.vecmath.Point2d
import scala.xml.Elem

case class WLNonCardinalHTML(analysis: WLNonCardAnalysis, wlMessage: Option[WLMessage]) extends Logging {

  /**
    * For returning HTML and js from functions.
    * @param elem HTML
    * @param js Javascript.
    */
  private case class ElemJS(elem: Elem, js: String) {}

  private def makeImageHTML(bufImg: BufferedImage, fileName: String, caption: String): ElemJS = {
    val id: String = C3Chart.makeUniqueChartIdTag
    val js: String = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

    val url = {
      val file = new File(analysis.subDir, fileName)
      Util.writePng(bufImg, file)
      file.getName
    }

    val content = {
      <div title="Click image for full-sized image.">
        <a href={url}>
          <div style="border: 1px solid lightgrey;">
            <div class='zoom' id={id} style="margin: 20px;">
              <img class="img-responsive fit-image" src={url}/>
            </div>
          </div>
       </a>
        <div style="text-align:center;">
          <i>{caption}</i>
        </div>
    </div>
    }

    ElemJS(content, js)
  }

  private def makeCloseupImage(): Unit = {
    val pngFile = new File(analysis.subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
    val bufImg = WLNonCardCompositeImage.makeCompositeImage(analysis)
    Util.writePng(bufImg, pngFile)
    wlMessage.foreach(_.info(s"Wrote file $pngFile"))
  }

  private def showWlMessage(): Elem = {
    if (wlMessage.isDefined) {
      <div>
        <h3>Log Messages</h3>
        <pre style="background: #eeeeee; font-size: small">
          {WebUtil.nl + wlMessage.get.toString}
        </pre>
      </div>
    } else {
      <span></span>
    }
  }

  private val beamName = {
    val n1 = { if (analysis.beamName.isDefined) analysis.beamName.get + " " else "" }.trim
    val n2 = { if (wlMessage.isDefined) " " + wlMessage.get.imageName else "" }.trim
    (n1 + " " + n2).trim
  }

  private def dicomAsText(): Elem = {
    val file = new File(analysis.subDir, "dicom.html")

    val content = {

      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>DICOM for {beamName}</h2>
          <pre style="background: #eeeeee; font-size: small">
            {DicomUtil.attributeListToString(analysis.al)}
          </pre>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = beamName,
      runScript = None
    )

    Util.writeFile(file, text)
    logger.info(s"Wrote DICOM metadata file ${file.getAbsolutePath}")

    <a href={file.getName}>DICOM Metadata</a>
  }

  private val fmt: String = "%20.6f"

  /**
    * Format a point as text.
    * @param p Point to format.
    * @return Single string.
    */
  private def fmtPoint(p: Point2d): String = {
    def fd(d: Double) = fmt.format(d).trim
    s"${fd(p.x)}, ${fd(p.y)}"
  }

  private def fmtPoint(p: Point2D.Double): String = {
    fmtPoint(new Point2d(p.x, p.y))
  }

  private def coarseLocationHTML(): ElemJS = {

    val imageHTML = makeImageHTML(analysis.nonCardEdge.coarseImage(), "coarseLocation.png", "First step is to get the coarse location of the entire field.")

    val center = analysis.nonCardEdge.locateCoarseCenter()

    val textElem = {
      <div>
        <h3>Coarse Location of Field</h3>
        Center: {fmtPoint(center)}
        <br>Box encloses the general location of the edges, with the center being a coarse estimate.</br>
        <p>Profiles in the X and Y axis were used find this.</p>
        <p>Hover mouse over image to zoom, click image for full sized image.</p>
      </div>
    }

    val content = {
      <div class="row">
        <div class="col-md-4" >
          {textElem}
        </div>
        <div class="col-md-5" >
          {imageHTML.elem}
        </div>
      </div>
    }

    ElemJS(content, imageHTML.js)
  }

  private def approximateEdgeHTML(): ElemJS = {

    val imageHTML = makeImageHTML(analysis.approxImg, "approximateEdge.png", "Locate the approximate position of the edges.")

    val center = analysis.nonCardEdge.approximateEdgeSet.center_pix

    val change = {
      val approximate = analysis.nonCardEdge.locateCoarseCenter()
      new Point2d(approximate.x - center.x, approximate.y - center.y)
    }

    val textElem: Elem = {
      <div>
        <h3>Approximate Center of Edges</h3>
        <p>Center: {fmtPoint(center)}</p>
        <p>Change from coarse measurement: {fmtPoint(change)}</p>
        <p>Total XY change: {fmt.format(change.distance(new Point2d()))}</p>
        <p>The four boxes show the areas that were used to make an edge gradient. The line inside each box shows where the edge was found.</p>
        <p>Each small line near the center of the image shows the halfway point between the opposing edges. The point where
          these lines (would) cross is the center found in this phase of the analysis.</p>
        <p>In this step, to create four AOIs radiating from the coarse center are created, using the collimator angle to orient them in
          the proper direction. The AOIs are narrow so that they are narrower than the</p>
        <p>field.  They are also extended to the edges of the image because it is not known at this point how big the field is.</p>
        <p>The edge of each AOI that is closest to the center is determined by finding the minimum pixel brightness Between the center and the AOI.</p>
        <p>Hover mouse over image to zoom, click for full image.</p>
      </div>
    }

    val content = {
      <div class="row">
        <div class="col-md-4" >
          {textElem}
        </div>
        <div class="col-md-5" >
          {imageHTML.elem}
        </div>
      </div>
    }

    ElemJS(content, imageHTML.js)
  }

  /**
    * Create an HTML representation of the edge's profile.
    * @param edge For this edge.
    * @return Chart showing the profile.
    */
  private def edgeProfile(edge: WLNonCardEdge): ElemJS = {

    // sampling distance in mm
    val increment_mm = analysis.trans.pix2IsoDistX(Config.WLNonCardEdgePixelResolution)

    val xValueList = edge.edgeProfile.indices.map(_ * increment_mm)

    val chart = new C3Chart(
      height = Some(200),
      xAxisLabel = "mm",
      xDataLabel = "CU",
      xValueList = xValueList,
      // xFormat // default:  String = ".4g",
      yAxisLabels = Seq("Offset (mm)"),
      yDataLabel = "Offset (mm)",
      yValues = Seq(edge.edgeProfile)
      // yFormat // default: String = ".4g",
      // yColorList // default: Seq[Color] = Seq(),
      // regionList // default: Seq[C3Chart.Region] = Seq()
    )

    val content: Elem = {
      <div>
        <h4>{edge.name} Penumbra Profile</h4>
        {chart.html}
      </div>
    }

    ElemJS(content, chart.javascript)
  }

  /**
    * Create an HTML representation of the edge's gradient.
    * @param edge For this edge.
    * @return Chart showing the profile.
    */
  private def edgeGradient(edge: WLNonCardEdge): ElemJS = {

    // sampling distance in mm
    val increment_mm = analysis.trans.pix2IsoDistX(Config.WLNonCardEdgePixelResolution)

    val xValueList = edge.gradient.indices.map(_ * increment_mm)

    val chart = new C3Chart(
      height = Some(200),
      xAxisLabel = "CU",
      xDataLabel = "mm",
      xValueList = xValueList,
      // xFormat // default:  String = ".4g",
      yAxisLabels = Seq("Offset (mm)"),
      yDataLabel = "Offset (mm)",
      yValues = Seq(edge.gradient)
      // yFormat // default: String = ".4g",
      // yColorList // default: Seq[Color] = Seq(),
      // regionList // default: Seq[C3Chart.Region] = Seq()
    )

    val content: Elem = {
      <div>
        <h4 title="This shows how straight the edge is.">{edge.name} Gradient (leaf end profile) </h4>
        {chart.html}
      </div>
    }

    ElemJS(content, chart.javascript)
  }

  /**
    * Show the HTML for the results of the precise edge location.
    * @return HTML and JS
    */
  private def preciseEdgeHTML(): ElemJS = {

    val imageHTML = makeImageHTML(analysis.img, "preciseEdge.png", "Measure precise location of edges using wider AOIs")

    val center = analysis.nonCardEdge.edgeSet.center_pix

    val change = {
      val approximate = analysis.nonCardEdge.approximateEdgeSet.center_pix
      new Point2d(approximate.x - center.x, approximate.y - center.y)
    }

    val textElem: Elem = {
      <div>
        <h3>Precise Center of Edges</h3>
        Center: {fmtPoint(center)}
        <p>Change from approximate measurement: {fmtPoint(change)}</p>
        <p>Total XY change: {fmt.format(change.distance(new Point2d()))}</p>
        <p></p>
        <p>The four boxes show the areas that were used to make an edge gradient. The line inside each box shows where
          the edge was found.</p>
        <p>Each small line near the center of the image shows the halfway point between the opposing edges. The point
        where these lines (would) cross is the center found in this phase of the analysis.</p>
        <p> In this step, again four AOIs are created.  This time they are wider to use a larger number of pixels
        for better accuracy.  The width of each AOI is limited by the separation of the edges found in the previous
        step.  The length of the boxes is long enough to extend beyond the edges, but not to the edge of the image.
        If they were longer it would increase computation time and yield the same results.  Note that the image in
        This phase is being sampled at a high resolution, so keeping the AOI smaller saves time.</p>
        <p>The edge of each AOI that is closest to the center is determined by finding the minimum pixel brightness
        Between the center and the AOI.</p>
        
        <p>Hover mouse over image to zoom, click for full image.</p>
      </div>
    }

    val profileList = analysis.nonCardEdge.edgeSet.edgeList.map(edgeProfile)
    val gradientList = analysis.nonCardEdge.edgeSet.edgeList.map(edgeGradient)

    val profileElem = {
      <div class="row">
        <div class="col-md-6" >
          {profileList.head.elem}
        </div>
        <div class="col-md-6" >
          {profileList(1).elem}
        </div>
      </div>
        <div class="row">
          <div class="col-md-6" >
            {profileList(2).elem}
          </div>
          <div class="col-md-6" >
            {profileList(3).elem}
          </div>
        </div>
    }

    val gradientElem = {
      <div class="row">
        <div class="col-md-6" >
          {gradientList.head.elem}
        </div>
        <div class="col-md-6" >
          {gradientList(1).elem}
        </div>
      </div>
        <div class="row">
          <div class="col-md-6" >
            {gradientList(2).elem}
          </div>
          <div class="col-md-6" >
            {gradientList(3).elem}
          </div>
        </div>
    }

    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-4" >
            {textElem}
          </div>
          <div class="col-md-5" >
            {imageHTML.elem}
          </div>
        </div>
        {profileElem}
        {gradientElem}
      </div>
    }

    val profileJs = profileList.map(_.js).mkString("\n")
    val gradientJs = gradientList.map(_.js).mkString("\n")

    val js = s"${imageHTML.js}\n$profileJs\n$gradientJs"

    ElemJS(content, js)
  }

  private val wlBallImage = WLBallImage(analysis)

  private val xBallProfilesHTML: ElemJS = {

    val yValueList = analysis.nonCardBall.xProfile
    val increment = analysis.trans.pix2IsoDistX(Config.WLNonCardBallPixelResolution)
    val xValueList = yValueList.indices.map(_ * increment)

    val xProfile = new C3Chart(
      height = Some(200),
      xAxisLabel = "mm",
      xDataLabel = "CU",
      xValueList = xValueList,
      yAxisLabels = Seq("Offset (mm)"),
      yDataLabel = "Offset (mm)",
      yValues = Seq(yValueList)
    )

    ElemJS(xProfile.html, xProfile.javascript)
  }

  /**
    * Make a profile for the ball.
    * @param yValueList Profile values.
    * @return
    */
  private def makeBallProfile(yValueList: Seq[Double]): ElemJS = {

    val increment = analysis.trans.pix2IsoDistX(Config.WLNonCardBallPixelResolution)
    val xValueList = yValueList.indices.map(_ * increment)

    val xProfile = new C3Chart(
      height = Some(200),
      xAxisLabel = "mm",
      xDataLabel = "CU",
      xValueList = xValueList,
      yAxisLabels = Seq("Offset (mm)"),
      yDataLabel = "Offset (mm)",
      yValues = Seq(yValueList)
    )

    ElemJS(xProfile.html, xProfile.javascript)
  }

  private def ballHTML(): ElemJS = {

    val center = {
      val pix = analysis.nonCardBall.center_pix
      analysis.trans.pix2Iso(pix.x, pix.y)
    }

    val textElem = {
      <div>
        <h3>Ball</h3>
        Center: {fmtPoint(center)}
        <br>The ball.  Add gooder text.</br>
      </div>
    }

    val foreground = makeImageHTML(wlBallImage.ballImage(), "ball.png", "Ball with center marked.")
    val background = makeImageHTML(wlBallImage.ballBackgroundImage(), "ballBackground.png", "Background of ball to show effects of stem.")
    val xProfile = makeBallProfile(analysis.nonCardBall.xProfile)
    val yProfile = makeBallProfile(analysis.nonCardBall.yProfile)

    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-4" >
            {textElem}
          </div>
          <div class="col-md-4" >
            {foreground.elem}
          </div>
          <div class="col-md-4" >
            {background.elem}
          </div>
        </div>
        <div class="row">
          <div class="col-md-6" >
            {xProfile.elem}
          </div>
          <div class="col-md-6" >
            {yProfile.elem}
          </div>
        </div>
      </div>
    }

    val allJS = Seq(foreground, background, xProfile, yProfile).map(_.js).mkString("\n")

    ElemJS(content, allJS)
  }

  private def makeDiagnosticsHtml(): Unit = {
    val coarseHTML = coarseLocationHTML()
    val approximate = approximateEdgeHTML()
    val preciseEdge = preciseEdgeHTML()
    val ball = ballHTML()

    val allJS = Seq(coarseHTML, approximate, preciseEdge, ball).map(_.js).mkString("\n")

    val js = s"<script>\n$allJS\n</script>"

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <div class="row">
            <div class="col-md-5">
              <h2>Details for Beam {beamName}</h2>
            </div>
            <div class="col-md-2">
              {dicomAsText()}
            </div>
          </div>
          <hr/>
          {showWlMessage()}
        </div>
        <div>
          <hr/>
          {coarseHTML.elem}
        </div>
        <div>
          <hr/>
          {approximate.elem}
        </div>
        <div>
          <hr/>
          {preciseEdge.elem}
        </div>
        <div>
          <hr/>
          {ball.elem}
        </div>
        <p style="margin:256px;"> </p>
      </div>
    }

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = s"Beam $beamName",
      c3 = true,
      runScript = Some(js)
    )

    val htmlFile = new File(analysis.subDir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME)

    Util.writeFile(htmlFile, text)
  }

  def generate(): Unit = {
    makeCloseupImage()

    makeDiagnosticsHtml()
  }

}
