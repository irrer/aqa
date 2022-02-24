package org.aqa.webrun.gapSkew

import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import scala.xml.Elem

case class LeafSetHtml(extendedData: ExtendedData, leafSet: LeafSet, runReq: GapSkewRunReq) {

  def leafSetToHtml: Elem = {

    val pngFile = GapSkewHtml.imageFileOf(leafSet, extendedData.output.dir)
    Util.writePng(leafSet.image, pngFile)

    def td(d: Double): Elem = {
      <td title={d.toString}>{Util.fmtDbl(d).trim}</td>
    }

    val skewTable = {
      // @formatter:off
      <table class="table table-bordered">
        <thead>
          <tr>
            <th> Position (mm) </th>
            <th> Rotation (deg) </th>
            <th> Left - Right (mm) </th>
            <th> Left (mm) </th>
            <th> Right (mm) </th>
            <th> Planned (mm) </th>
          </tr>

          <tr>
            <td>
              Top
            </td>
            {td(leafSet.gapSkew.topAngle_deg)}
            {td(leafSet.topLeft.yPosition_mm - leafSet.topRight.yPosition_mm)}
            {td(leafSet.topLeft.yPosition_mm)}
            {td(leafSet.topRight.yPosition_mm)}
            {td(leafSet.leafPositionRtplanTop_mm)}
          </tr>

          <tr>
            <td>
              Bottom
            </td>
            {td(leafSet.gapSkew.bottomAngle_deg)}
            {td(leafSet.bottomLeft.yPosition_mm - leafSet.bottomRight.yPosition_mm)}
            {td(leafSet.bottomLeft.yPosition_mm)}
            {td(leafSet.bottomRight.yPosition_mm)}
            {td(leafSet.leafPositionRtplanBottom_mm)}
          </tr>

        </thead>
      </table>
      // @formatter:on
    }

    def offsetTable = {
      // @formatter:off
      <table class="table table-bordered">
        <thead>
          <tr>
            <th>
              Position (mm)
            </th>
            <th>
              Top - Bottom - <br/> Planned (mm)
            </th>
            <th>
              Top (mm)
            </th>
            <th>
              Bottom (mm)
            </th>
          </tr>

          <tr>
            <td>
              Left
            </td>
            {td(leafSet.topLeft.yPosition_mm - leafSet.bottomLeft.yPosition_mm - (leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm))}
            {td(leafSet.topLeft.yPosition_mm)}
            {td(leafSet.topRight.yPosition_mm)}
          </tr>

          <tr>
            <td>
              Right
            </td>
            {td(leafSet.bottomRight.yPosition_mm - leafSet.topRight.yPosition_mm + (leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm))}
            {td(leafSet.bottomLeft.yPosition_mm)}
            {td(leafSet.bottomRight.yPosition_mm)}
          </tr>

        </thead>
      </table>
      // @formatter:on
    }

    def htmlName = DicomHtml(extendedData).makeDicomContent(runReq.rtimageMap(leafSet.beamName), leafSet.beamName, Some(GapSkewHtml.imageFileOf(leafSet, extendedData.output.dir).getName))

    /*
    // accumulate all of the chart javascript here
    def chartJavascript = ArrayBuffer[String]()

     * Make a chart that shows the profile of the beam at the edge.  Also put the javascript in the common buffer.
     * @param leaf Leaf to show.
     * @return HTML reference to chart.
    def makeChart(leaf: Leaf): Elem = {

      val chart = new C3Chart(
        // width = Some(400),
        height = Some(300),
        xAxisLabel = "Position",
        xDataLabel = "cm",
        xValueList = leaf.profile.map(_.y_mm),
        yAxisLabels = Seq("CU"),
        yDataLabel = "CU",
        yValues = Seq(leaf.profile.map(_.cu)),
        //yFormat = ".0f",
        yFormat = ".8gf",
        yColorList = Seq(new Color(0, 255, 255))
      )

      chartJavascript += chart.javascript
      chart.html
    }
     */

    /*
    val idPrefix = Util.textToId(leafSet.beamName)

    val edgeChartList: Elem = {
      <div>
        <h3>Edge Profiles</h3>
        <ul class="nav nav-tabs">
          <li class="active">
            <a data-toggle="tab" href={"#" + idPrefix + "topLeft"}>Top Left {Util.fmtDbl(leafSet.topLeft.yPosition_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href={"#" + idPrefix + "topRight"}>Top Right {Util.fmtDbl(leafSet.topRight.yPosition_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href={"#" + idPrefix + "bottomLeft"}>Bottom Left {Util.fmtDbl(leafSet.bottomLeft.yPosition_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href={"#" + idPrefix + "bottomRight"}>Bottom Right {Util.fmtDbl(leafSet.bottomRight.yPosition_mm)}</a>
          </li>
        </ul>

        <div class="tab-content">
          <div id={idPrefix + "topLeft"} class="tab-pane fade in active">
            <h3>Top Left</h3>
            {makeChart(leafSet.topLeft)}
          </div>
          <div id={idPrefix + "topRight"} class="tab-pane fade">
            <h3>Top Right</h3>
            {makeChart(leafSet.topRight)}
          </div>
          <div id={idPrefix + "bottomLeft"} class="tab-pane fade">
            <h3>Bottom Left</h3>
            {makeChart(leafSet.bottomLeft)}
          </div>
          <div id={idPrefix + "bottomRight"} class="tab-pane fade">
            <h3>Bottom Right</h3>
            {makeChart(leafSet.bottomRight)}
          </div>
        </div>
      </div>
    }
     */

    def content = {

      Trace.trace("Angles.  top: " + leafSet.gapSkew.topAngle_deg + "    bottom: " + leafSet.gapSkew.bottomAngle_deg + "    avg: " + leafSet.gapSkew.averageAngle_deg)

      <div class="row">
        <hr/>
        <div class="col-md-6">
          <center>
            <h3 style="margin:20px;" title="Average rotational error of top and bottom (0 is ideal).">
              {leafSet.beamName + " " + WebUtil.nbsp + " " + WebUtil.nbsp + " " + WebUtil.nbsp + " Rotation: " + Util.fmtDbl(leafSet.gapSkew.averageAngle_deg) + " deg"}
            </h3>
            {skewTable}
            <p></p>
            {offsetTable}
          </center>
        </div>
        <div class="col-md-6">
          <center>
            <a href={htmlName} style="margin:8px;">
              View larger image and metadata
              <img class="img-responsive fit-image" src={pngFile.getName}/>
            </a>
          </center>
        </div>
        { /* edgeChartList */ }
      </div>
    }

    content
  }

}
