package org.aqa.webrun.stakitt.stakittHTML

import edu.umro.ImageUtil.ImageUtil
import org.aqa.Logging
import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.Analysis
import org.aqa.webrun.stakitt.StakittResult

import scala.xml.Elem

/**
  * Make statistics HTML.
  * @param analysis Results
  */
case class HtmlStats(analysis: Analysis) extends Logging {

  /**
    * Calculate the median value of a list.
    *
    * @param list For this list
    * @return Median.
    */
  private def median(list: Seq[Double]): Double = {
    val isEven = (list.size % 2) == 0

    val index = list.size / 2

    val sorted = list.sorted

    if (isEven) {
      (sorted(index) + sorted(index + 1)) / 2.0
    } else
      sorted(index)
  }

  private def statistics(): Elem = {

    def stats(name: String, offsetList: Seq[Double]): Elem = {

      <tr>
        <td style="padding:10px;">
          <b>{name}</b>
        </td>
        {WebUtil.setPrecisionAttr(<td style="padding:10px;"> </td>, offsetList.min)}
        {WebUtil.setPrecisionAttr(<td style="padding:10px;"> </td>, offsetList.max)}
        {WebUtil.setPrecisionAttr(<td style="padding:10px;"> </td>, offsetList.sum / offsetList.size)}
        {WebUtil.setPrecisionAttr(<td style="padding:10px;"> </td>, median(offsetList))}
        {WebUtil.setPrecisionAttr(<td style="padding:10px;"> </td>, ImageUtil.stdDev(offsetList.map(_.toFloat)))}
      </tr>

    }

    def endStats(name: String, resultList: Seq[StakittResult]): Elem =
      stats(name, resultList.map(_.stakitt.leafEndOffset_mm))

    /**
     * Show the measured rotation of the collimator.
     * @return Description of rotation.
     */
    def skew(): Elem = {

      val x1MeanBorderOffset = analysis.resultColumns.head.map(r => (r.stakitt.measuredMajorSide_mm + r.stakitt.measuredMinorSide_mm) / 2).sum / analysis.resultColumns.head.size
      val x1X = analysis.resultColumns.head.map(r => r.stakitt.measuredEndPosition_mm).sum / analysis.resultColumns.head.size
      val x2MeanBorderOffset = analysis.resultColumns.last.map(r => (r.stakitt.measuredMajorSide_mm + r.stakitt.measuredMinorSide_mm) / 2).sum / analysis.resultColumns.head.size
      val x2X = analysis.resultColumns.last.map(r => r.stakitt.measuredEndPosition_mm).sum / analysis.resultColumns.head.size
      val xDistance = x2X - x1X
      val yDistance = x1MeanBorderOffset - x2MeanBorderOffset
      val angle_radians = Math.atan(yDistance / xDistance)
      val angle_degrees = Math.toDegrees(angle_radians)
      val changePer400mm = Math.sin(angle_radians) * 400

      <div style="margin-left:20px;">
        <b>Measured Collimator Rotation Angle (degrees): </b>{WebUtil.setPrecisionAttr(<span> </span>, angle_degrees)}
        <b style="margin-left:40px;">Rotation Angle (mm/400mm): </b>{WebUtil.setPrecisionAttr(<span> </span>, changePer400mm)}
      </div>
    }

    val header = {
      <thead>
        <tr>
          <th style="padding:10px;">
            Value
          </th>
          <th style="padding:10px;">
            Min
          </th>
          <th style="padding:10px;">
            Max
          </th>
          <th style="padding:10px;">
            Mean
          </th>
          <th style="padding:10px;">
            Median
          </th>
          <th style="padding:10px;">
            Std Dev
          </th>
        </tr>
      </thead>
    }

    <div>
      <h3>
        Statistics
      </h3>
      {skew()}
      <div style="margin:20px;">
        <h4>Table Showing Differences of Measured Leaf End - Planned End,<br/> and Measured Gap - Planned Gap.</h4>
      </div>
      <table class="table-responsive table-bordered" style="margin:25px;">
        {header}
        {endStats("Bank X1", analysis.x1BankResultList)}
        {endStats("Bank X2", analysis.x2BankResultList)}
        {stats("Gap", analysis.gapRows.flatten.map(_.error))}
      </table>
    </div>
  }

  val elem: Elem = statistics()
}
