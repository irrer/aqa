package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.C3Chart
import org.aqa.web.WebUtil.ElemJS
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Config

import scala.xml.Elem

/**
  * Make a single bar chart. Values to be plotted are given as a histogram.  The tricky part is that each covers a range of
  * values, so C3 is given an offset to make the tick marks appear in the right place.
  *
  * We also want the different bar charts to have the same scale so that when viewed it is easier to compare them.  This
  * means that several parameters have to be supplied that specify the chart's dimension and scale.
  *
  * Histogram values are binned to 0.1 mm.  This is baked in to the code (hard-coded).
  *
  * @param tickRange Number of tick marks to the left and right of zero.  (e.g. a value of 10 would make 21 total.)
  * @param barColor Color of a bar that is with tolerance.
  * @param errorColor Color of a bar that is out of tolerance.
  * @param maxY Maximum vertical value.
  * @param histogramUnsorted Binned data in no particular order.
  * @param tolerance Limit for the amount of acceptable error.
  */
case class StakittBarChart(tickRange: Int, barColor: String, errorColor: String, maxY: Int, histogramUnsorted: Seq[StakittBarChart.StakittBin], tolerance: Double) {

  // sort by X position
  private val histogram = histogramUnsorted.sortBy(_.xLo)

  // Make the chart.
  def elemJs: ElemJS = {

    val id = C3Chart.makeUniqueChartIdTag

    // This is inserted into the HTML where the chart is to appear.
    val html = <div id={id}> {id} </div>

    // If there are many tick marks, and they are all labelled, then the labels overlap and are
    // hard to read. This indicates how many to skip.  If the value is 1, then all tick marks are
    // labeled.  If 2, then every other (even numbered) tick marks are labeled, and so on.
    val skip = ((tickRange * 0.2) - 1).round.toInt

    // The values of all tick marks.
    val tickValueList = (-tickRange to tickRange).map(_ / 10.0)

    // This is a debugging aid.  Generate a list of human-readable values that show up as a comment in the JavaScript.
    val native = histogram.mkString("    ||    ")

    //noinspection SpellCheckingInspection
    val js = {
      s"""
         |    // native: $native
         |    const $id = c3.generate({
         |      bindto: "#$id",
         |
         |      data: {
         |        x: "x",
         |        columns: [
         |          // Bars are centered at 0.5, 1.5, 2.5, etc.
         |          // This lets tick marks sit at 0, 1, 2, etc.,
         |          // which are the boundaries between bars.
         |          [ "x",      ${histogram.map(_.xLo + 0.05).mkString(", ")} ],
         |          [ "Value",  ${histogram.map(_.count).mkString(", ")}]
         |        ],
         |        type: "bar",
         |        color: function(color, d) {
         |          if ((d.x < -$tolerance) || (d.x > $tolerance))
         |            return "$errorColor";
         |          return "$barColor";
         |        }
         |      },
         |
         |      bar: {
         |        width: {
         |          // Controls bar width.  A ratio of 0.5 makes the bar %50 of the width between ticks. A ratio of 1.0 makes adjacent bars touch each other.
         |          ratio: 0.9
         |        }
         |      },
         |
         |      axis: {
         |        x: {
         |          min: -${tickRange / 10.0},
         |          max:  ${tickRange / 10.0},
         |          padding: {
         |            left: 0,
         |            right: 0
         |          },
         |
         |          // These become the X-axis tick labels.
         |          tick: {
         |            // Tick marks are placed at the bar boundaries,
         |            // which puts them between the bars.
         |            values: [ ${tickValueList.mkString(", ")} ],
         |            format: function (x) {
         |              const xx = Math.round(x * 10);
         |              if (($skip == 1) || ((xx % $skip) == 0))
         |                return x.toFixed(1);
         |              else
         |                return "";
         |            },
         |            outer: false
         |          },
         |
         |          label: {
         |            text: "Offset",
         |            position: "outer-center"
         |          }
         |        },
         |
         |        y: {
         |          min: 0,
         |          max: $maxY,
         |          label: {
         |            text: "Value",
         |            position: "outer-middle"
         |          }
         |        }
         |      },
         |
         |      tooltip: {
         |        contents: function (d) {
         |          const barIndex = d[0].index;
         |          const xValue = d[0].x - 0.05;
         |          const lo = xValue.toFixed(1)
         |          const hi = (xValue + 0.1).toFixed(1)
         |          const xText = `$${lo} to $${hi}`;
         |          const yValue = d[0].value;
         |
         |          return `
         |            <div style="background: white; border: 1px solid #999; border-radius: 4px; padding: 8px 12px; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.2); font-size: 14px; ">
         |                $${xText}<br/><h3>$${yValue}</h3>
         |            </div>
         |          `;
         |         }
         |      },
         |
         |      legend: {
         |        show: false
         |      }
         |    });
         |""".stripMargin.replaceAll("\r", "")
    }

    ElemJS(html, js)

  }
}

object StakittBarChart {

  /**
    * Contain one histogram bin.
    * @param xLo Lower X bound of contents.
    * @param count Number of readings x:   xLo <= x < (x + 0.1)
    */
  case class StakittBin(xLo: Double, count: Int) {
    val name: String = {
      count.toString
    }

    override def toString: String = {
      s"$xLo : $count"
    }

    def xPosition(tickRange: Int): Double = (xLo * 10).round + tickRange + 0.5
  }

  def makeBarHtml(analysis: Analysis): ElemJS = {

    /**
      * Group the given values into 0.1 mm bins for display as a histogram.
      *
      * Each group contains the values for X as: <code>lo <= X < hi</code>.  Example: <code>0.0 <= X < 0.1</code>
      *
      * @param valueList List of offsets or gap errors.
      * @return List of bins.
      */
    def makeBinList(valueList: Seq[Double]): Seq[StakittBin] = {

      /** Grouped leaf offsets that are all within the same tenth of a mm.  Each group contains the
        * values bounded as: <code> lo <= offset < hi </code>.  The map key is the lower limit * 10.
        */
      val grouped = valueList.groupBy(offset => (offset * 10).floor)

      def groupToStakittBin(key: Double, list: Seq[Double]): StakittBin = {
        val xLo = key / 10.0
        StakittBin(xLo, list.size)
      }

      // convert each group into a StakittBin
      grouped.map(kv => groupToStakittBin(kv._1, kv._2)).toSeq
    }

    val gapOffsetList = analysis.gapRows.flatten.map(_.error)
    val allOffsets = analysis.stakittList.map(_.stakitt.leafEndOffset_mm) ++ gapOffsetList

    val aBankBinList = makeBinList(analysis.x1BankResultList.map(_.stakitt.leafEndOffset_mm))
    val bBankBinList = makeBinList(analysis.x2BankResultList.map(_.stakitt.leafEndOffset_mm))
    val gapBinList = makeBinList(gapOffsetList)

    val minOffset = allOffsets.min
    val maxOffset = allOffsets.max

    val tickRange: Int = {
      val max = Math.max(minOffset.abs, maxOffset.abs)
      (max.ceil * 10).round.toInt
    }

    /** color for bars within limits */
    val offsetBarColor = "#09C97F"

    /** color for gap within limits */
    val gapBarColor = "#BAB9BA"

    /** color for bars outside of limits */
    val barErrorColor = "#F95668"

    /** Establish the height for all bar charts. */
    val maxY = (aBankBinList ++ bBankBinList ++ gapBinList).map(_.count).max

    /**
      * Create the histogram, specifying the sam parameters for all histograms so that they have the same
      * scale.  This lets the user more easily compare the results between them.
      * @param stakittBinList List of bins.
      * @param barColor Color for bars that are within limits.
      * @return A bar chart.
      */
    def makeBarHistogram(stakittBinList: Seq[StakittBin], barColor: String, tolerance: Double): ElemJS = {
      StakittBarChart(tickRange, barColor, barErrorColor, maxY, stakittBinList, tolerance).elemJs
    }

    val bankABarHistogram = makeBarHistogram(aBankBinList, offsetBarColor, Config.StakittLeafTolerance_mm)
    val bankBBarHistogram = makeBarHistogram(bBankBinList, offsetBarColor, Config.StakittLeafTolerance_mm)
    val gapBarHistogram = makeBarHistogram(gapBinList, gapBarColor, Config.StakittGapTolerance_mm)

    val elem: Elem = {
      <div style="text-align: center;">

        <div class="row">
          <h3> Bank A (X1) Histogram </h3>
          {bankABarHistogram.elem}
        </div>

        <div class="row">
          <h3> Bank B (X2) istogram </h3>
          {bankBBarHistogram.elem}
        </div>

        <div class="row">
          <h3> Leaf Gap Histogram </h3>
          {gapBarHistogram.elem}
        </div>

      </div>
    }

    val js = Seq(bankABarHistogram, bankBBarHistogram, gapBarHistogram).map(_.js).mkString("\n")

    ElemJS(elem, js)
  }
}
