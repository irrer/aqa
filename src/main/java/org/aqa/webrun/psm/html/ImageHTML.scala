package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil
import org.aqa.webrun.psm.PSMGradientAscent
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.io.File
import scala.xml.Elem

class ImageHTML(
    extendedData: ExtendedData,
    rtplan: AttributeList,
    resultList: Seq[PSMBeamAnalysisResult],
    psmGradientAscent: PSMGradientAscent,
    ffAl: AttributeList,
    ffImg: DicomImage,
    wdAl: AttributeList,
    wdImg: DicomImage,
    rawAl: AttributeList,
    rawImg: DicomImage,
    brAl: AttributeList,
    brImg: DicomImage,
    psmAl: AttributeList,
    psmImg: DicomImage
) extends Logging {

  private def makeChart(dicomImage: DicomImage, al: AttributeList, yLabel: String): C3Chart = {
    val trans = new IsoImagePlaneTranslator(al)

    val xValueList = (0 until dicomImage.height).map(y => trans.pix2IsoCoordY(y))
    val area = new Rectangle((dicomImage.width / 2) - 1, 0, 2, dicomImage.height)
    val yAxial = dicomImage.getSubimage(area).rowSums.map(_ / 2.0)
    val yTransverse = dicomImage.getSubimage(new Rectangle(0, (dicomImage.height / 2) - 1, dicomImage.width, 2)).columnSums.map(_ / 2.0)

    val chart = new C3Chart(
      xAxisLabel = "Position mm", //
      xDataLabel = "Position mm", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Axial", "Transverse"), //
      yDataLabel = yLabel, //
      yValues = Seq(yAxial, yTransverse),
      yColorList = Seq(Color.green, Color.blue)
    )

    chart
  }

  private def centralPixels(image: DicomImage): Elem = {
    val centerSize = 10
    val rectangle = new Rectangle((image.width - centerSize) / 2, (image.height - centerSize) / 2, centerSize, centerSize)
    val subImage = image.getSubimage(rectangle)

    def row(y: Int): Elem = {
      <tr>{(0 until centerSize).map(x => WebUtil.setPrecisionAttr(<td></td>, subImage.get(x, y)))}</tr>
    }

    <table class="table responsive table-bordered" style="font-size: 0.75em;">
      {(0 until centerSize).map(row)}
    </table>

  }

  private case class Row(name: String, image: DicomImage, al: AttributeList, alternateImage: Option[DicomImage] = None) {

    private val id = Util.textToId(name)

    private val bufImage = image.toDeepColorBufferedImage(0.1)

    private val pngFile = new File(extendedData.output.dir, id + ".png")

    Util.writePng(bufImage, pngFile)
    logger.info("Wrote file " + pngFile.getAbsolutePath)

    private val imageRef = {
      <div>
        <h4>{name}</h4>
        <img src={pngFile.getName} width="256"/>
      </div>
    }

    private val chart = makeChart(image, al, name)

    val elem: Elem = {
      <tr>
        <td>{imageRef}</td>
        <td>{centralPixels(image)}</td>
        <td>{chart.html}</td>
      </tr>
    }

    val js: String = chart.javascript

  }

  private val ffRow = Row("Flood Field", ffImg, ffAl)
  private val wdRow = Row("Whole Detector", wdImg, wdAl)
  private val rawRow = Row("Raw Image", rawImg, rawAl)
  private val brRow = Row("Beam Response", brImg, brAl)
  private val psmRow = Row("PSM", psmImg, psmAl)

  def make(): (Elem, String) = {
    val content = {
      <table class="table responsive table-bordered" style="margin-top:25px;">
        <thead>
          <tr>
            <th>
              Image
            </th>
            <th>
              Center Pixels
            </th>
            <th>
              Profiles
            </th>
          </tr>
          {}
        </thead>
        {ffRow.elem}
        {wdRow.elem}
        {rawRow.elem}
        {brRow.elem}
        {psmRow.elem}
      </table>
    }

    (content, Seq(ffRow, wdRow, rawRow, brRow, psmRow).map(_.js).mkString("\n"))
  }

}
