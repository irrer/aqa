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

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessMainHTML extends Logging {

  private def titleDicomMetadata = "Click to view DICOM metadata"
  private def titleDetails = "Click to view details"
  private def titleAxialSymmetry = "Axial symmetry from top to bottom: (top-bottom)/bottom.  Max limit is " + Config.SymmetryLimit
  private def titleTransverseSymmetry = "Transverse symmetry from right to left: (right-left)/left.  Max limit is " + Config.SymmetryLimit
  private def titleFlatness = "Flatness: (max-min)/(max+min).  Max limit is " + Config.FlatnessLimit

  private def tableHead: Elem = {
    <thead>
      <tr>
        <th style="text-align: center;" title={ titleDicomMetadata }>
          Beam
          <br/>
          Name
        </th>
        <th style="text-align: center;" title={ titleDetails }>
          Details
        </th>
        <th style="text-align: center;" title={ titleAxialSymmetry }>
          Axial Symmetry
        </th>
        <th style="text-align: center;" title={ titleTransverseSymmetry }>
          Transverse Symmetry
        </th>
        <th style="text-align: center;" title={ titleFlatness }>
          Flatness
        </th>
      </tr>
    </thead>
  }

  private def dicomRefColumn(beamName: String, extendedData: ExtendedData, runReq: RunReq): Elem = {
    val dicomFile = runReq.rtimageMap(beamName)
    val link = extendedData.dicomHref(dicomFile)
    <td style="vertical-align: middle;" title={ titleDicomMetadata } rowspan="3"><a href={ link }>{ beamName }</a></td>
  }

  private def detailsColumn(subDir: File, beamName: String, extendedData: ExtendedData, runReq: RunReq): Elem = {
    val img = {
      <img src={ WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, beamName)) } width="100"/>
    }

    val ref = {
      <a href={ WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, beamName)) }>{ img }</a>
    }

    <td style="text-align: center;vertical-align: middle;" title={ titleDetails } rowspan="3">{ ref }</td>
  }

  private def axialSymmetryColumn(result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult): Elem = {
    val bsClass = if (result.axialSymmetryStatus == ProcedureStatus.pass) "" else "danger"
    <td class={ bsClass } style="text-align: center;" title={ titleAxialSymmetry }>
      { result.axialSymmetry.formatted("%14.6f") }
    </td>
  }

  private def transverseSymmetryColumn(result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult): Elem = {
    val bsClass = if (result.transverseSymmetryStatus == ProcedureStatus.pass) "" else "danger"
    <td class={ bsClass } style="text-align: center;" title={ titleTransverseSymmetry }>
      { result.transverseSymmetry.formatted("%14.6f") }
    </td>
  }

  private def flatnessColumn(result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult): Elem = {
    val bsClass = if (result.flatnessStatus == ProcedureStatus.pass) "" else "danger"
    <td class={ bsClass } style="text-align: center;" title={ titleFlatness }>
      { result.flatness.formatted("%14.6f") }
    </td>
  }

  private def makeRow(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, runReq: RunReq): Seq[Elem] = {
    Seq(
      {
        <tr align="center">
          { dicomRefColumn(result.beamName, extendedData, runReq) }
          { detailsColumn(subDir, result.beamName, extendedData, runReq) }
          { <td>Axial Base</td> }
          { <td>Axial Percent</td> }
          { axialSymmetryColumn(result) }
        </tr>
      },
      {
        <tr>
          { <td>Transverse Base</td> }
          { <td>Transverse Percent</td> }
          { transverseSymmetryColumn(result) }
        </tr>
      },
      {
        <tr>
          { <td>Flatness Base</td> }
          { <td>Flatness Percent</td> }
          { flatnessColumn(result) }
        </tr>
      })
  }

  def makeContent(subDir: File, extendedData: ExtendedData, resultList: List[SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult], status: ProcedureStatus.Value, runReq: RunReq): Elem = {
    val content = {
      <div class="col-md-6 col-md-offset-3">
        <table class="table table-bordered">
          { tableHead }
          { resultList.map(r => makeRow(subDir, extendedData, r, runReq: RunReq)) }
        </table>
      </div>
    }
    content
  }

}
