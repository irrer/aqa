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

  def makeDisplay(extendedData: ExtendedData, wedgeListList: Seq[Seq[Wedge]], status: ProcedureStatus.Value, runReq: RunReq): Elem = {

    val outputDir = extendedData.output.dir

    def charts: String = {
      wedgeListList.map(wedgeList => WedgeProfileChart.chart(wedgeList)).mkString(scriptPrefix, "\n", scriptSuffix)
    }

    def wedgeHtml(wedgeList: Seq[Wedge]): Elem = {
      <div class="row" title="Wedge Profile">
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

    val html = Phase2Util.wrapSubProcedure(extendedData, content, WedgeAnalysis.subProcedureName, status, Some(charts))
    val outFile = new File(outputDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status)
  }

}
