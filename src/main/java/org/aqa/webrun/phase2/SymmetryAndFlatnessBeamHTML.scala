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
object SymmetryAndFlatnessBeamHTML extends Logging {

  private def graph(name: String, valueList: Seq[Double], xList: Seq[Double]): (Elem, String) = {

    Trace.trace(name + "  valueList.size: " + valueList.size)
    Trace.trace(name + "  xList.size: " + xList.size)
    val id = "Chart_" + WebUtil.stringToUrlSafe(name)

    val html = {
      <div>
        <div class="col-md-10" align="middle">
          <div class="row">
            <h3>{ name }</h3>
            <div id={ id }>filler</div>
          </div>
        </div>
      </div>
    }

    val javascript = {
      """      
      var """ + id + """ = c3.generate({
                data: {
                    x: 'Position mm',
                    columns: [
                         [ 'Position mm', """ + xList.mkString(", ") + """],
                         [ 'CU', """ + valueList.mkString(", ") + """ ]
                    ]
                },
                point: { // show point on hover
                    r: 0,
                    focus : {
                        expand: {
                            r:4
                        }
                    }
                },
                bindto : '#""" + id + """',
                axis: {
                    x: {
                        label: 'Position mm',
                        tick: {
                            format: d3.format('.3g')
                        }
                    },
                    y: {
                        label: 'CU',
                        tick: {
                            format: d3.format('.4g')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb' ]
                }
            });
"""
    }

    (html, javascript)
  }

  private def makeContent(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq): (Elem, String) = {
    val graphTransverse = graph("Transverse " + result.beamName, result.transverseProfile, result.transverse_mm)
    val graphAxial = graph("Axial " + result.beamName, result.axialProfile, result.axial_mm)
    val content = {
      <div class="col-md-12" align="middle">
        <div class="row">
          { <img src={ WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, result.beamName)) }/> }
        </div>
        <div class="row">
          { graphTransverse._1 }
        </div>
        <div class="row">
          { graphAxial._1 }
        </div>
      </div>
    }
    val javascript = "<script>\n" + graphTransverse._2 + graphAxial._2 + "\n</script>\n"
    (content, javascript)
  }

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq) = {
    val elemJavascript = makeContent(subDir, extendedData, result, status, runReq)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, "Symmetry and Flatness Beam " + result.beamName, result.status, Some(elemJavascript._2), runReq)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.beamName), html.getBytes)
  }
}
