package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.webrun.psm.PSMCharts
import org.aqa.webrun.psm.PSMDicom
import org.aqa.webrun.psm.PSMGradientAscent

import java.awt.Color
import java.io.File
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
    ffImg: DicomImage,
    wdAl: AttributeList,
    wdImg: DicomImage,
    rawImg: DicomImage,
    cbrImg: DicomImage,
    brImg: Option[DicomImage],
    psmImg: Option[DicomImage]
) extends Logging {

  def make(): Unit = {

    val resultHtml = new ResultHtml(extendedData, resultList)

    val dir = extendedData.output.dir

    def make(): (Elem, String) = {

      val trans = new IsoImagePlaneTranslator(wdAl)

      val ffRow = PSMHtmlImage(extendedData, "Flood Field", ffImg, trans, dir = dir, al = Some(ffAl))
      val wdRow = PSMHtmlImage(extendedData, "Whole Detector", wdImg, trans, dir = dir, al = Some(wdAl))
      val rawRow = PSMHtmlImage(extendedData, "Raw Image = Flood Field * Whole Detector", rawImg, trans, dir = dir)
      val cbrRow = PSMHtmlImage(extendedData, "Beam Response Beam Centers", cbrImg, trans, dir = dir, resultList = resultList, color = Some(Color.white))
      val brRow: Option[PSMHtmlImage] = {
        if (psmGradientAscent.isDefined && brImg.isDefined)
          Some(
            PSMHtmlImage(
              extendedData,
              "Beam Response Interpolated and Normalized",
              brImg.get,
              trans,
              dir = dir,
              center = Some(psmGradientAscent.get.getMaxPoint_iso),
              resultList = resultList,
              color = Some(Color.white)
            )
          )
        else
          None
      }
      val psmRow: Option[PSMHtmlImage] = {
        if (psmImg.isDefined) Some(PSMHtmlImage(extendedData, "PSM = Raw / Beam Response", psmImg.get, trans, dir = dir))
        else
          None
      }

      val content = {
        <table class="table responsive table-bordered" style="margin-top:25px;">
          <thead>
            <tr>
              <th title="Click for larger image, larger chart, and metadata.">
                Image
              </th>
              <th>
                Center Pixels
              </th>
              <th>
                Profiles
              </th>
            </tr>{}
          </thead>{ffRow.elem}{wdRow.elem}{rawRow.elem}{cbrRow.elem}{if (brRow.isDefined) brRow.get.elem}{if (psmRow.isDefined) psmRow.get.elem}
        </table>
      }

      val list: Seq[PSMHtmlImage] = Seq( //
        ffRow,
        wdRow,
        rawRow,
        cbrRow
      ) ++
        Seq(brRow, psmRow).flatten

      val js = list.map(_.js).mkString("\n")

      (content, js)
    }

    val imageContent = make()

    val historyCharts = new PSMCharts(extendedData.outputPK)

    val planHtml = PlanHTML(extendedData, rtplan)

    val psmDicomFile = new File(dir, "PSMDicom.dcm")
    if (psmImg.isDefined) {
      val psmDicom = PSMDicom.psmToDicom(psmImg.get, wdAl, RTImageLabel = "PSM as DICOM", RTImageDescription = "Only the pixel data is relevant.")
      DicomUtil.writeAttributeListToFile(psmDicom, psmDicomFile, "AQA")
      logger.info("Wrote PSM as DICOM to: " + psmDicomFile.getAbsolutePath)
    }

    val content = {
      <div>
        <div class="row">
          <div class="col-md-2 col-md-offset-1">
            {WebUtil.showPrecision}
          </div>
          <div class="col-md-2 col-md-offset-1">
            <a href={planHtml.fileName}>View RTPLAN</a>
          </div>{// @formatter:off
            if (psmImg.isDefined) {
              <div class="col-md-2 col-md-offset-1" title="Note that only the pixel data is relevant, not energy or other parametes..">
                <a href={psmDicomFile.getName}>Download PSM as DICOM</a>
               </div>
          // @formatter:on}
            }}
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {imageContent._1}
            </table>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>Mean Beam Values</h3>{historyCharts.meanChart.html}<h3>Standard Deviation of Beam Center Pixels</h3>{historyCharts.stdDevChart.html}<h3>Coordinates of Max Interpolated Points</h3>{historyCharts.maxInterpolationCoordinates.html}
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
