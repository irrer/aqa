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

  private val topLeft =
    <center>
      <h4>Top Left</h4>
      <br/>
      {span(t = "Error (mm)", value = gapSkew.topLeftDeltaY_mm, "Planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gapSkew.topPlannedY_mm, "Position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gapSkew.topLeftY_mm, "Actual measured position (mm)")}
    </center>

  private val topRight =
    <center>
      <h4>Top Right</h4>
      <br/>
      {span(t = "Error (mm)", value = gapSkew.topRightDeltaY_mm, "Planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gapSkew.topPlannedY_mm, "Position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gapSkew.topRightY_mm, "Actual measured position (mm)")}
    </center>

  private val bottomRight =
    <center>
      <h4>Bottom Right</h4>
      <br/>
      {span(t = "Error (mm)", value = gapSkew.bottomRightDeltaY_mm, "Planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gapSkew.bottomPlannedY_mm, "Position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gapSkew.bottomRightY_mm, "Actual measured position (mm)")}
    </center>

  private val bottomLeft =
    <center>
      <h4>Bottom Left</h4>
      <br/>
      {span(t = "Error (mm)", value = gapSkew.bottomLeftDeltaY_mm, "Planned position - actual position (mm)")}
      <br/>
      {span(t = "Planned (mm)", value = gapSkew.bottomPlannedY_mm, "Position in RTPLAN (mm)")}
      <br/>
      {span(t = "Measured (mm)", value = gapSkew.bottomLeftY_mm, "Actual measured position (mm)")}
    </center>

  private val topCenter =
    <center>
      {span(t = "Top skew (deg)", value = gapSkew.topAngle_deg, "Angle of top edge (deg)")}
      <br/>
      {span(t = "Change in top horizontal edge (mm)", value = gapSkew.topDeltaY_mm, "Rise or fall in top horizontal edge (mm)")}
    </center>

  private val bottomCenter =
    <center>
      {span(t = "Bottom skew (deg)", value = gapSkew.bottomAngle_deg, "Angle of bottom edge (deg)")}
      <br/>
      {span(t = "Change in bottom horizontal edge (mm)", value = gapSkew.bottomDeltaY_mm, "Rise or fall in bottom horizontal edge (mm)")}
    </center>

  private val leftCenter =
    <div style="display: table; height: 384px; overflow: hidden;">
      <div style="display: table-cell; vertical-align: middle; horizontal-align: right;">
        {span(t = "Left Vert Error (mm)", value = gapSkew.leftVerticalError_mm, "Angle of bottom edge (deg)")}
        <br/>
        {span(t = "Planned (mm)", value = gapSkew.plannedVertical_mm, "Rise or fall in bottom horizontal edge (mm)")}
        <br/>
        {span(t = "Measured(mm)", value = gapSkew.leftVertical_mm, "Measured distance between top and bottom on the left side (mm)")}
      </div>
    </div>

  private val rightCenter =
    <div style="display: table; height: 384px; overflow: hidden;">
      <div style="display: table-cell; vertical-align: middle; horizontal-align: left;">
        {span(t = "Right Vert Error (mm)", value = gapSkew.rightVerticalError_mm, "Angle of bottom edge (deg)")}
        <br/>
        {span(t = "Planned (mm)", value = gapSkew.plannedVertical_mm, "Rise or fall in bottom horizontal edge (mm)")}
        <br/>
        {span(t = "Measured(mm)", value = gapSkew.rightVertical_mm, "Measured distance between top and bottom on the right side (mm)")}
      </div>
    </div>

  private val center = {
    <center>
      <a href={dicomMetadataUrl}>
        Click for larger image and DICOM metadata.
        <img class="img-responsive fit-image" src={imageUrl} style="width:384px;"/>
      </a>
    </center>
  }

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
