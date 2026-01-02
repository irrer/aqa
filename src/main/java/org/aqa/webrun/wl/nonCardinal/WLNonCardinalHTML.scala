package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.wl.WLgenHtml
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLMessage
import org.aqa.Logging
import org.aqa.web.C3Chart

import java.awt.Color
import java.awt.Rectangle
import java.io.File
import javax.vecmath.Point2d
import scala.xml.Elem

case class WLNonCardinalHTML(analysis: WLNonCardAnalysis, wlMessage: Option[WLMessage]) extends Logging {

  private def makeCloseupImage(): Unit = {
    val pngFile = new File(analysis.subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
    val bufImg = WLNonCardCompositeImage.makeCompositeImage(analysis)
    Util.writePng(bufImg, pngFile)
    wlMessage.foreach(_.info(s"Wrote file $pngFile"))
  }

  private def showWlMessage(): Elem = {
    if (wlMessage.isDefined) {
      <pre style="background: #eeeeee; font-size: small">
        {wlMessage.get.toString}
      </pre>
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

  private def coarseLocationHTML(): Elem = {
    val id: String = C3Chart.makeUniqueChartIdTag

    val js = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

    val url = {
      val file = new File(analysis.subDir, "coarseLocation.png")
      Util.writePng(analysis.nonCardEdge.coarseImage(), file)
      file.getName
    }

    val center = analysis.nonCardEdge.locateCoarseCenter()

    val content = {
      <div>
        <h3>Coarse Location</h3>
        Center: {fmtPoint(center)}
        <br>Box encloses the general location of the edges, with the center being a coarse estimate.</br>
        <br>Profiles in the X and Y axis were used find this.</br>
        <p></p>
        <script> {js} </script>
        <a href={url}>
          <div class='zoom' id={id}>
            <img width="512" src={url}/>
          </div>
        </a>
      </div>
    }

    content
  }

  private def approximateEdgeHTML(): Elem = {
    val id: String = C3Chart.makeUniqueChartIdTag

    val js = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

    val url = {
      val file = new File(analysis.subDir, "approximateEdge.png")
      Util.writePng(analysis.approxImg, file)
      file.getName
    }

    val center = analysis.nonCardEdge.approximateEdgeSet.center_pix

    val change = {
      val approximate = analysis.nonCardEdge.locateCoarseCenter()
      new Point2d(approximate.x - center.x, approximate.y - center.y)
    }

    val content = {
      <div>
        <h3>Approximate Center of Edges</h3>
        Center: {fmtPoint(center)}
        <br>Change from coarse measurement: {fmtPoint(change)}</br>
        <br>Total XY change: {fmt.format(change.distance(new Point2d()))}</br>
        <br>The four boxes show the areas that were used to make an edge gradient. The line inside each box</br>
        <br>Shows where the edge was found.</br>
        <p></p>
        <br>Each small line near the center of the image shows the halfway point between the opposing edges.</br>
        <br>The point where these lines (would) intersect is the center found in this phase of the analysis.</br>
        <p></p>
        <br>In this step, to create four AOIs radiating from the coarse center are created, using the collimator</br>
        <br>angle to orient them in the proper direction. The AOIs are narrow so that they are narrower than the</br>
        <br>field.  They are also extended to the edges of the image because it is not known at this point how big</br>
        <br>the field is.
        <br>The edge of each AOI that is closest to the center is determined by finding the minimum pixel brightness</br>
        <br>Between the center and the AOI.</br>
        </br>
        <br>Hover mouse over image to zoom.</br>
        <script> {js} </script>
        <a href={url}>
          <div class='zoom' id={id}>
            <img width="512" src={url}/>
          </div>
        </a>
      </div>
    }

    content
  }

  private def preciseEdgeHTML(): Elem = {
    val id: String = C3Chart.makeUniqueChartIdTag

    val js = s"""$$(document).ready(function(){ $$('#$id').zoom(); });""".replaceAllLiterally("\"", WebUtil.singleQuote)

    val url = {
      val file = new File(analysis.subDir, "preciseEdge.png")
      Util.writePng(analysis.img, file)
      file.getName
    }

    val center = analysis.nonCardEdge.edgeSet.center_pix

    val change = {
      val approximate = analysis.nonCardEdge.approximateEdgeSet.center_pix
      new Point2d(approximate.x - center.x, approximate.y - center.y)
    }

    val content = {
      <div>
        <h3>Precise Center of Edges</h3>
        Center: {fmtPoint(center)}
        <br>Change from approximate measurement: {fmtPoint(change)}</br>
        <br>Total XY change: {fmt.format(change.distance(new Point2d()))}</br>
        <p></p>
        <br>The four boxes show the areas that were used to make an edge gradient. The line inside each box</br>
        <br>Shows where the edge was found.</br>
        <p></p>
        <br>Each small line near the center of the image shows the halfway point between the opposing edges.</br>
        <br>The point where these lines (would) intersect is the center found in this phase of the analysis.</br>
        <br>Hover mouse over image to zoom.</br>
        <p></p>
        <br>In this step, again four AOIs are created.  This time they are wider to use a larger number of pixels</br>
        <br>for better accuracy.  The width of each AOI is limited by the separation of the edges found in the previous</br>
        <br>step.  The length of the boxes is long enough to extend beyond the edges, but not to the edge of the image.</br>
        <br>If they were longer it would increase computation time and yield the same results.  Note that the image in</br>
        <br>This phase is being sampled at a high resolution, so keeping the AOI smaller saves time.</br>
        <br>The edge of each AOI that is closest to the center is determined by finding the minimum pixel brightness</br>
        <br>Between the center and the AOI.</br>
        <br></br>
        <script> {js} </script>
        <a href={url}>
          <div class='zoom' id={id}>
            <img width="512" src={url}/>
          </div>
        </a>
      </div>
    }

    content
  }

  /**
    * Make an image of the given edge.  Write the image to a file and return an element to display it.
    * @param edge For this edge
    * @return Element to display image.
    */
  def edgeImage(edge: WLNonCardEdge): Elem = {

    val pointList = Seq( ///
      edge.loHiAoi,
      edge.loLoAoi,
      edge.hiHiAoi,
      edge.hiLoAoi
    )

    val minX = pointList.map(_.x).min.round.toInt
    val maxX = pointList.map(_.x).max.round.toInt
    val minY = pointList.map(_.y).min.round.toInt
    val maxY = pointList.map(_.y).max.round.toInt

    // number of pixels to leave around the area.
    val border = 5

    val area = new Rectangle(minX, minY, maxX - minX, maxY - minY)

    val dicomImage = analysis.preprocessedImage

    val areaWithBorder = new Rectangle( //
      Math.min(minX - border, 0),
      Math.min(minY - border, 0),
      Math.max(maxX - minX + (border * 2), dicomImage.width - 1),
      Math.max(maxY - minY + (border * 2), dicomImage.height - 1)
    )

    val bufImg = WLBlankImage.make(dicomImage, analysis.nonCardEdge.edgeSet)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.yellow)

    ???

  }

  private def edgeHTML(): Elem = {

    val e = analysis.nonCardEdge.edgeSet.X1
    analysis.nonCardEdge.edgeSet.X1.profile
    ???
  }

  private def makeDiagnosticsHtml(): Unit = {

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>Details for Beam {beamName}</h2>
          {dicomAsText()}
          {showWlMessage()}
        </div>
        <div>
          <hr/>
          {coarseLocationHTML()}
        </div>
        <div>
          <hr/>
          {approximateEdgeHTML()}
        </div>
        <div>
          <hr/>
          {preciseEdgeHTML()}
        </div>
        <div style="margin:50px;"></div>
      </div>
    }

    val runScript = "" // TODO  zoom needed?

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = s"Beam ${beamName}",
      c3 = true,
      runScript = Some(runScript)
    )

    val htmlFile = new File(analysis.subDir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME)

    Util.writeFile(htmlFile, text)
  }

  def generate(): Unit = {
    makeCloseupImage()

    makeDiagnosticsHtml()
  }

}
