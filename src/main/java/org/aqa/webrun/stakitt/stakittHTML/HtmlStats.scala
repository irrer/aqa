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

    val x1MeanBorderOffset = analysis.resultColumns.head.map(r => (r.stakitt.measuredMajorSide_mm + r.stakitt.measuredMinorSide_mm) / 2).sum / analysis.resultColumns.head.size
    val x2MeanBorderOffset = analysis.resultColumns.last.map(r => (r.stakitt.measuredMajorSide_mm + r.stakitt.measuredMinorSide_mm) / 2).sum / analysis.resultColumns.head.size

    def skew_mm: Double = {
      // TODO
      ???
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
      <h3>
        Skew:   mm, degrees
      </h3>
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
