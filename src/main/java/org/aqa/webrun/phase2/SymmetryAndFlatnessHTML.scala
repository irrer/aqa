package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point
import edu.umro.ImageUtil.ImageText
import java.io.File
import org.aqa.web.WebServer
import org.aqa.web.WebUtil

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessHTML extends Logging {

  private val htmlFileName = "SymmetryAndFlatness.html"
  private val subDirName = "SymmetryAndFlatness"

  private def makeSubDir(outputDir: File): File = {
    val subDir = new File(outputDir, subDirName)
    subDir.mkdirs
    subDir
  }

  def annotatedImageFile(subDir: File, beamName: String): File = {
    val fileName = "Sym_Flat_" + WebUtil.stringToUrlSafe(beamName) + ".png"
    new File(subDir, fileName)
  }

  def beamHtmlFile(subDir: File, beamName: String): File = {
    val fileName = WebUtil.stringToUrlSafe(beamName) + ".html"
    new File(subDir, fileName)
  }

  private def summary(mainHtmlFile: File, resultListSize: Int, status: ProcedureStatus.Value) = {
    val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={ WebServer.urlOfResultsFile(mainHtmlFile) }>
          Symmetry &amp; Flatness Beams:{ resultListSize }<br/>
          <img src={ iconImage } height="32"/>
        </a>
      </div>
    }
    elem
  }

  def makeDisplay(extendedData: ExtendedData, resultList: List[SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult], status: ProcedureStatus.Value, runReq: RunReq): Elem = {
    val subDir = makeSubDir(extendedData.output.dir)
    val mainHtmlFile = new File(subDir, htmlFileName)
    resultList.par.map(r => Util.writePng(r.annotatedImage, annotatedImageFile(subDir, r.beamName)))

    val html = Phase2Util.wrapSubProcedure(extendedData, SymmetryAndFlatnessMainHTML.makeContent(subDir, extendedData, resultList, status, runReq), "Symmetry and Flatness", status, None, runReq)
    Util.writeFile(mainHtmlFile, html)

    resultList.map(result => SymmetryAndFlatnessBeamHTML.makeDisplay(subDir, extendedData, result, status, runReq))

    summary(mainHtmlFile, resultList.size, status)
  }

}
