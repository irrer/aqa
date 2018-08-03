package org.aqa.webrun.phase2

import org.aqa.db.Output
import scala.xml.Elem
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import org.aqa.run.ProcedureStatus
import org.aqa.Util
import org.aqa.db.Wedge
import java.awt.geom.Point2D

object WedgeHTML {

  private val htmlFileName = "Wedge.html"

  private val scriptPrefix = {
    """
<script>
"""
  }

  private val scriptSuffix = {
    """
    </script>
"""
  }

  /**
   * Make a tiny summary and link to the detailed report.
   */
  private def makeSummary(status: ProcedureStatus.Value) = {
    val iconImage = if ((status == ProcedureStatus.pass) || (status == ProcedureStatus.done)) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={ htmlFileName }>
          { WedgeAnalysis.subProcedureName }<br/>
          <img src={ iconImage } height="32"/>
        </a>
      </div>
    }
    elem
  }

  def slope(wedgeList: Seq[Wedge]): Elem = {
    val posList = wedgeList.map(w => w.position_mm)

    val sumX = posList.sum
    val sumXsq = posList.map(p => p * p).sum
    val sumY = wedgeList.map(w => w.radiodensity_hu).sum
    val sumXY = wedgeList.map(w => w.position_mm * w.radiodensity_hu).sum

    val m = ((wedgeList.size * sumXY) - (sumX * sumY)) / ((wedgeList.size * sumXsq) - (sumX * sumX))
    val b = (sumY - (m * sumX)) / wedgeList.size
    println("m: " + m)
    println("b: " + b)
    ???
  }

  def makeDisplay(extendedData: ExtendedData, wedgeListList: Seq[Seq[Wedge]], status: ProcedureStatus.Value, runReq: RunReq): Elem = {

    val outputDir = extendedData.output.dir

    def charts: String = {
      wedgeListList.map(wedgeList => WedgeProfileChart.chart(wedgeList)).mkString(scriptPrefix, "\n", scriptSuffix)
    }

    def wedgeHtml(wedgeList: Seq[Wedge]): Elem = {
      <div class="row">
        <h3>Beam { wedgeList.head.beamName } </h3>
        <div id={ WedgeProfileChart.beamRef(wedgeList.head.beamName) }>filler</div>
      </div>
    }

    val content = {
      <div>
        <div class="col-md-10" align="middle">
          { wedgeListList.map(wedgeList => wedgeHtml(wedgeList)) }
        </div>
      </div>
    }

    val html = Phase2Util.wrapSubProcedure(extendedData, content, WedgeAnalysis.subProcedureName, status, Some(charts), runReq)
    val outFile = new File(outputDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status)
  }

  def main(args: Array[String]): Unit = {
    val xList = Seq(2, 3, 5, 7, 9)
    val yList = Seq(4, 5, 7, 10, 15)

    val wList = (0 until 5).map(i => new Wedge(None, -1, "", "", xList(i), yList(i)))

    slope(wList)
  }

}
