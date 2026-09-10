package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.webrun.psm.PSMCharts
import org.aqa.webrun.psm.PSMGradientAscent
import org.aqa.webrun.psm.PSMGrid
import org.aqa.webrun.psm.PSMRunReq

import java.awt.Color
import java.io.File
import javax.vecmath.Point2d
import scala.collection.immutable.Seq
import scala.xml.Elem

/**
  * Generate HTML page to show all PSM data.
  */

class PSMMainHTML(
    extendedData: ExtendedData,
    rtplan: AttributeList,
    resultList: Seq[PSMBeamAnalysisResult],
    psmGradientAscent: Option[PSMGradientAscent],
    ffAl: AttributeList,
    ffImgNormalized: DicomImage,
    wdAl: Option[AttributeList],
    wdImg: Option[DicomImage],
    beamResponsesNotNormalizedImg: DicomImage,
    psmImg: Option[DicomImage],
    psmRunReq: PSMRunReq
) extends Logging {

  def make(): Unit = {

    val centerResult = {
      val centerPoint_mm = new Point2d(0, 0)
      resultList.minBy(r => r.psmBeam.center.distance(centerPoint_mm))
    }

    val resultHtml = new ResultHtml(extendedData, resultList)

    val grid = PSMGrid(resultList)

    val dir = extendedData.output.dir

    def make(): (Elem, String) = {

      val trans = new IsoImagePlaneTranslator(centerResult.rtimage)

      val ffRow = PSMHtmlImage( //
        extendedData,
        name = "Flood Field Normalized",
        image = ffImgNormalized,
        grid = grid,
        trans,
        dir = dir,
        al = Some(centerResult.rtimage),
        valueGetter = psmBeam => psmBeam.floodField_cu.get,
        color = Some(Color.white)
      )

      /*
      val wdRow: Option[PSMHtmlImage] = {
        if (wdImg.isDefined && wdAl.isDefined)
          Some(
            PSMHtmlImage( //
              extendedData,
              name = "Whole Detector",
              image = wdImg.get,
              grid = grid,
              trans,
              dir = dir,
              al = wdAl,
              valueGetter = psmBeam => psmBeam.wholeDetector_cu.get,
              color = Some(Color.white)
            )
          )
        else
          None
      }
       */

      val beamResponsesNotNormalizedRow = PSMHtmlImage( //
        extendedData,
        name = "Beam Responses Not Normalized",
        image = beamResponsesNotNormalizedImg,
        grid = grid,
        trans,
        dir = dir,
        valueGetter = psmBeam => psmBeam.mean_cu,
        resultList = resultList,
        color = Some(Color.white)
      )

      val beamResponsesWithNormalizationImg: DicomImage = {

        val centerResponse = grid.centerBeam.mean_cu.toFloat

        def func(p: Float): Float = {
          p / centerResponse
        }

        beamResponsesNotNormalizedImg.fun1(func)
      }

      val beamResponsesWithNormalizationRow = PSMHtmlImage( //
        extendedData,
        name = "Beam Responses With Normalization",
        image = beamResponsesWithNormalizationImg,
        grid = grid,
        trans,
        dir = dir,
        valueGetter = psmBeam => psmBeam.beamResponseNormalized.get,
        resultList = resultList,
        color = Some(Color.white)
      )

      val psmRow: Option[PSMHtmlImage] = {
        if (psmImg.isDefined)
          Some(
            PSMHtmlImage( //
              extendedData,
              name = "PSM Interpolated",
              image = psmImg.get,
              grid = grid,
              trans,
              dir = dir,
              al = None,
              valueGetter = psmBeam => psmBeam.beamResponseNormalized.get,
              color = Some(Color.white)
            )
          )
        else
          None
      }

      val rowList: Seq[PSMHtmlImage] = Seq(
        Some(ffRow),
        // wdRow,
        Some(beamResponsesNotNormalizedRow),
        Some(beamResponsesWithNormalizationRow),
        psmRow
      ).flatten

      val content = {
        <table class="table responsive table-bordered" style="margin-top:25px;">
          <thead>
            <tr>
              <th title="Click for larger image, larger chart, and metadata.">
                Image
              </th>
              <th>
                Beam Values
              </th>
              <th>
                Profiles
              </th>
            </tr>
          </thead>{rowList.map(_.elem)}
        </table>
      }

      val js = rowList.map(_.js).mkString("\n")

      (content, js)
    }

    val imageContent = make()

    val historyCharts = new PSMCharts(extendedData.outputPK)

    val planHtml = PlanHTML(extendedData, rtplan)

    val psmDicomFile = new File(dir, "PSMDicom.dcm")

    val beamType: Elem = {
      val fffText = {
        val isFFF = DicomUtil.findAllTag(rtplan, TagByName.FluenceMode).map(_.getSingleStringValueOrEmptyString()).exists(_.trim.equalsIgnoreCase("FFF"))
        if (isFFF) "FFF" else "non-FFF"
      }
      val kvpText = {
        val k = DicomUtil.findAllTag(ffAl, TagByName.KVP).head.getDoubleValues.head
        0 match {
          case _ if (k.round == k) && ((k.round % 1000) == 0) => (k / 1000).round.toString + " MV"
          case _                                              => Util.fmtDbl(k / 1000) + " MV"
        }
      }

      val text = s"Beam Type: $kvpText $fffText"

      <h4>
        {text}
      </h4>
    }

    val content = {

      // placeholder for a link for a downloadable DICOM version of the PSM.  Maybe do this someday if people want it.
      val psmElem: Elem = {
        <span></span>
        /*
          if (psmImg.isDefined) {
            val elem = {
              <div class="col-md-2" title="Note that only the pixel data is relevant, not energy or other parametes..">
                <p style="margin-top:9px;">
                  <a href={psmDicomFile.getName}>Download PSM
                    <br>as DICOM</br>
                  </a>
                </p>
              </div>
            }
            elem
          } else
            <span></span>
         */
      }

      <div>
        <div class="row">
          <div class="col-md-2">
            {WebUtil.showPrecision}
          </div>
          <div class="col-md-3">
            {beamType}
          </div>
          <div class="col-md-2">
            <p style="margin-top:9px;">
              <a href={ViewOutput.viewOutputUrl(psmRunReq.floodField.outputPK)}>View Flood Field</a>
            </p>
          </div>
          <div class="col-md-1">
            <p style="margin-top:9px;">
              <a href={planHtml.fileName}>View RTPLAN</a>
            </p>
          </div>
          {psmElem}
        </div>
        <div class="row">
          <div class="col-md-10">
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {imageContent._1}
            </table>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>Mean Beam Values</h3>{historyCharts.meanChart.html}<h3>Standard Deviation of Beam Center Pixels</h3>{historyCharts.stdDevChart.html}<h3>Coordinates of Max Interpolated Points</h3>{
        historyCharts.maxInterpolationCoordinates.html
      }
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {resultHtml.make()}
          </div>
        </div>
        <div class="row">
          <p style="margin-bottom:150px;"></p>
        </div>
      </div>
    }

    val js =
      s"""<script>
         |${imageContent._2}
         |</script>
         |${PSMBeamResponseChartRestlet.makeReference(extendedData.outputPK)}
         |""".stripMargin

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", c3 = true, runScript = Some(js))
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)

  }

}
