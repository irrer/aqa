package org.aqa.webrun.gapSkew

import org.aqa.db.GapSkew
import org.aqa.web.WebUtil
import org.aqa.webrun.gapSkew.GapSkewUtil.fmt2

import scala.xml.Elem

class GapSkewHtmlTable(gapSkew: GapSkew, dicomMetadataUrl: String, imageUrl: String) {

  private def span(t: String, value: Double, title: String): Elem = {
    <span title={title + WebUtil.titleNewline + "Value: " + value} style="white-space: nowrap;">
        {t + ": " + fmt2(value)}
      </span>
  }

  private val gs = gapSkew

  private val topLeft = {
    <center>
      <h4>Top Left</h4>
      <br/>
      {span(t = "Error (mm)", value = gs.topLeftHorzDelta_mm, "Top left planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gs.topLeftPlanned_mm.get, "Top left position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gs.topLeftValue_mm.get, "Top left actual measured position (mm)")}
    </center>
  }

  private val topRight = {
    <center>
      <h4>Top Right</h4>
      <br/>
      {span(t = "Error (mm)", value = gs.topRightHorzDelta_mm, "Top right planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gs.topRightPlanned_mm.get, "Top right position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gs.topRightValue_mm.get, "Top right actual measured position (mm)")}
    </center>
  }

  private val bottomLeft = {
    <center>
      <h4>Bottom Left</h4>
      <br/>
      {span(t = "Error (mm)", value = gs.bottomLeftHorzDelta_mm, "Bottom left planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gs.bottomLeftPlanned_mm.get, "Bottom left position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gs.bottomLeftValue_mm.get, "Bottom left actual measured position (mm)")}
    </center>
  }

  private val bottomRight = {
    <center>
      <h4>Bottom Right</h4>
      <br/>
      {span(t = "Error (mm)", value = gs.bottomRightHorzDelta_mm, "Bottom right planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gs.bottomRightPlanned_mm.get, "Bottom right position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gs.bottomRightValue_mm.get, "Bottom right actual measured position (mm)")}
    </center>
  }

  private val topCenter = {
    <center>
      {span(t = "Top skew (deg)", value = gs.topHorzSkew_deg, "Angle of top edge (deg)")}
      <br/>
      {span(t = "Change in top horizontal edge (mm)", value = gs.topHorzDelta_mm, "Rise or fall in top horizontal edge (mm)")}
    </center>
  }

  private val bottomCenter = {
    <center>
      {span(t = "Bottom skew (deg)", value = gs.bottomHorzSkew_deg, "Angle of bottom edge (deg)")}
      <br/>
      {span(t = "Change in bottom horizontal edge (mm)", value = gs.bottomHorzDelta_mm, "Rise or fall in bottom horizontal edge (mm)")}
    </center>
  }

  private val leftCenter = {
    <div style="display: table; height: 384px; overflow: hidden;">
      <div style="display: table-cell; vertical-align: middle; horizontal-align: right;">
        {span(t = "Left Vert Error (mm)", value = gs.leftDeltaSeparationOfHorzEdges_mm, "Planned separation - measured separation (mm).")}
        <br/>
        {span(t = "Planned (mm)", value = gs.plannedEdgeSeparation_mm, "Planned separation of horizontal edges.")}
        <br/>
        {span(t = "Measured(mm)", value = gs.leftDeltaSeparationOfHorzEdges_mm, "Measured distance between top and bottom on the left side (mm)")}
      </div>
    </div>
  }

  private val rightCenter = {
    <div style="display: table; height: 384px; overflow: hidden;">
      <div style="display: table-cell; vertical-align: middle; horizontal-align: left;">
        {span(t = "Right Vert Error (mm)", value = gapSkew.plannedEdgeSeparation_mm, "Planned separation - measured separation (mm).")}
        <br/>
        {span(t = "Planned (mm)", value = gapSkew.plannedEdgeSeparation_mm, "Rise or fall in bottom horizontal edge (mm)")}
        <br/>
        {span(t = "Measured(mm)", value = gs.rightDeltaSeparationOfHorzEdges_mm, "Measured distance between top and bottom on the right side (mm)")}
      </div>
    </div>
  }

  private val center = {
    <center>
      <a href={dicomMetadataUrl}>
        Click for larger image and DICOM metadata.
        <img class="img-responsive fit-image" src={imageUrl} style="width:384px;"/>
      </a>
    </center>
  }
  // @formatter:off
  // @formatter:on

  /**
    * Create a 3x3 table with all of the angles, errors, and positions.
    * @return HTML table.
    */
  def detailTable: Elem = {
    <table class="table table-bordered">
      <tr>
        <td>{topLeft}</td>
        <td>{topCenter}</td>
        <td>{topRight}</td>
      </tr>
      <tr>
        <td>{leftCenter}</td>
        <td>{center}</td>
        <td>{rightCenter}</td>
      </tr>
      <tr>
        <td>{bottomLeft}</td>
        <td>{bottomCenter}</td>
        <td>{bottomRight}</td>
      </tr>
    </table>
  }

}
