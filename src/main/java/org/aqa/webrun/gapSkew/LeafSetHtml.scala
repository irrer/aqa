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
      def isJawToText(isJaw: Boolean): String = if (isJaw) "Jaw" else "MLC"

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
            <td style="white-space: nowrap;">Top ({isJawToText(leafSet.gapSkew.topIsJaw)})</td>
            {td(leafSet.gapSkew.topAngle_deg)}
            {td(leafSet.topLeft.yPosition_mm - leafSet.topRight.yPosition_mm)}
            {td(leafSet.topLeft.yPosition_mm)}
            {td(leafSet.topRight.yPosition_mm)}
            {td(leafSet.leafPositionRtplanTop_mm)}
          </tr>

          <tr>
            <td style="white-space: nowrap;">Bottom ({isJawToText(leafSet.gapSkew.topIsJaw)})</td>
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
      </div>
    }

    content
  }

}
