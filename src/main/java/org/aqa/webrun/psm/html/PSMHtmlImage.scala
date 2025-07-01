package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.web.WebUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.Logging

import java.awt.geom.Point2D
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.Rectangle
import java.io.File
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.collection.immutable.Seq
import scala.xml.Elem

/**
  * Make the image, chart, and html to show one step of PSM processing.
  * @param extendedData Metadata.
  * @param name PSM step name.
  * @param image Scaled image.
  * @param trans Transform for scaling chart.
  * @param dir Put files here.
  * @param al DICOM metadata, if applicable.  If present, show the DICOM metadata.
  * @param center Max point, if applicable.  If present, show on image.
  * @param resultList List of results, if applicable.  If present, show the values of the 42 beams on the main image.
  */
case class PSMHtmlImage(
    extendedData: ExtendedData,
    name: String,
    image: DicomImage,
    trans: IsoImagePlaneTranslator,
    dir: File,
    al: Option[AttributeList] = None,
    center: Option[Point2D.Double] = None,
    resultList: Seq[PSMBeamAnalysisResult] = Seq(),
    color: Option[Color] = None
) extends Logging {

  private def makeChart(dicomImage: DicomImage, yLabel: String): C3Chart = {
    //val trans = new IsoImagePlaneTranslator(al)

    val xValueList = (0 until dicomImage.height).map(y => trans.pix2IsoCoordY(y))
    val area = new Rectangle((dicomImage.width / 2) - 1, 0, 2, dicomImage.height)
    val yAxial = dicomImage.getSubimage(area).rowSums.map(_ / 2.0)
    val yTransverse = dicomImage.getSubimage(new Rectangle(0, (dicomImage.height / 2) - 1, dicomImage.width, 2)).columnSums.map(_ / 2.0)

    val yValues: Seq[Seq[Double]] = {

      val yData =
        0 match {

          case _ if yAxial.size == yTransverse.size =>
            Seq(yAxial, yTransverse)

          case _ if yAxial.size > yTransverse.size =>
            val diff = yAxial.size - yTransverse.size
            val left = diff / 2
            val right = diff - left
            val value = yTransverse.min
            val t = (0 until left).map(_ => value) ++ yTransverse ++ (0 until right).map(_ => value)
            Seq(yAxial, t)

          case _ if yAxial.size < yTransverse.size =>
            val diff = yTransverse.size - yAxial.size
            val left = diff / 2
            val right = diff - left
            val value = yAxial.min
            val a = (0 until left).map(_ => value) ++ yAxial ++ (0 until right).map(_ => value)
            Seq(a, yTransverse)

        }
      yData.map(_.toList)
    }

    val yFormat = {
      val min = (yAxial ++ yTransverse).min.round.toString.length
      val max = (yAxial ++ yTransverse).max.round.toString.length

      val len = Seq(min, max, 4).max

      s"$len.${len}g"
    }

    val chart = new C3Chart(
      xAxisLabel = "Position mm", //
      xDataLabel = "Position mm", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Axial", "Transverse"), //
      yDataLabel = yLabel, //
      yValues = yValues,
      yColorList = Seq(Color.green, Color.blue),
      yFormat = yFormat
    )

    chart
  }

  private def centralPixels(image: DicomImage): Elem = {
    val centerSize = 8
    val rectangle = new Rectangle((image.width - centerSize) / 2, (image.height - centerSize) / 2, centerSize, centerSize)
    val subImage = image.getSubimage(rectangle)

    def row(y: Int): Elem = {
      <tr>{(0 until centerSize).map(x => WebUtil.setPrecisionAttr(<td></td>, subImage.get(x, y)))}</tr>
    }

    <table class="table responsive table-bordered" style="font-size: 0.70em;">
      {(0 until centerSize).map(row)}
    </table>

  }

  private def annotateBeamCenters(bufImg: BufferedImage): Unit = {

    // val trans = new IsoImagePlaneTranslator(resultList.head.rtimage)

    def fmt(d: Double): String = "%8.2f".format(d).trim

    def drawHeading(): Unit = {
      val text = "Circles show mean CU of each beam center"
      val gc = ImageUtil.getGraphics(bufImg)
      gc.setColor(Color.black)
      val offset = (ImageText.getTextDimensions(gc, text).getHeight * 1.5).toInt

      ImageText.setFont(gc, ImageText.DefaultFont, 20)

      ImageText.drawTextCenteredAt(gc, trans.width / 2, offset, text)
    }

    def drawCircle(result: PSMBeamAnalysisResult): Unit = {
      val center_pix = trans.iso2Pix(result.psmBeam.xCenter_mm, result.psmBeam.yCenter_mm)
      val gc = ImageUtil.getGraphics(bufImg)
      gc.setColor(Color.black)

      val width = trans.iso2PixDistX(Config.PSMRadius_mm * 2).toInt
      val height = trans.iso2PixDistY(Config.PSMRadius_mm * 2).toInt

      val text = fmt(result.psmBeam.mean_cu)
      //ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY - offset, text1)
      ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY, text)

      gc.drawOval((center_pix.getX - width / 2).toInt, (center_pix.getY - height / 2).toInt, width, height)
    }

    drawHeading()

    resultList.foreach(drawCircle)
  }

  private def annotateMaxCoordinates(maxPoint_iso: Point2D.Double, bufImg: BufferedImage, trans: IsoImagePlaneTranslator): Unit = {

    val maxPoint_pix = trans.iso2Pix(maxPoint_iso)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.black)

    val len = 30

    val x_pix = maxPoint_pix.getX.round.toInt
    val y_pix = maxPoint_pix.getY.round.toInt

    Seq(-1, 0, 1).foreach(i => gc.drawLine(x_pix - (len / 2), y_pix + i, x_pix + (len / 2), y_pix + i))
    Seq(-1, 0, 1).foreach(i => gc.drawLine(x_pix + i, y_pix - (len / 2), x_pix + i, y_pix + (len / 2)))

    val text = "Maximum value at " + "%6.2f".format(maxPoint_iso.getX).trim + ", " + "%6.2f".format(maxPoint_iso.getY).trim
    ImageText.drawTextCenteredAt(gc, x_pix, y_pix + len + (ImageText.getFontHeight(gc) / 2), text)
  }

  // ------------------------------------------------------------------------------------------------------

  private val id = Util.textToId(name)

  private val bufImage = {
    // if color is specified, the use it
    if (color.isDefined)
      image.toBufferedImage(color.get)
    else
      image.toDeepColorBufferedImage(0.1)
  }
  Util.addGraticules(bufImage, trans, Color.GRAY)
  if (resultList.nonEmpty)
    annotateBeamCenters(bufImage)

  if (center.isDefined)
    annotateMaxCoordinates(center.get, bufImage, trans)

  Config.applyWatermark(bufImage)

  private val pngFileName = id + ".png"
  private val pngFile = new File(dir, pngFileName)

  private val htmlFileName: String = id + ".html"

  Util.writePng(bufImage, pngFile)
  logger.info("Wrote file " + pngFile.getAbsolutePath)

  private val imageRef = {
    <div>
        <a href={htmlFileName}>
          <h4>{name}</h4>
          <img src={pngFile.getName} width="256"/>
        </a>
      </div>
  }

  private val chart = makeChart(image, name)

  val elem: Elem = {
    <tr>
        <td title="Click for larger chart, larger chart, and metadata.">{imageRef}</td>
        <td>{centralPixels(image)}</td>
        <td>{chart.html}</td>
      </tr>
  }

  val js: String = chart.javascript

  private def makeImageHtml(): Unit = {

    val alElem: Seq[Elem] =
      if (al.isDefined)
        Seq(<div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <pre style="margin-top:40px;">{WebUtil.nl + DicomUtil.attributeListToString(al.get)}</pre>
          </div>
        </div>)
      else
        Seq()

    val content = {
      <div>
          <div class="row">
            <div class="col-md-10 col-md-offset-1" >
              <h3>{name}</h3>
              {chart.html}
            </div>
          </div>
          <div class="row">
            <div class="col-md-10 col-md-offset-1" >
              <img style="margin-top:40px;" src={pngFileName}/>
            </div>
          </div>
            {alElem}
          <div class="row">
            <p style="margin-bottom:150px;"> </p>
          </div>
        </div>
    }

    val imageJs = s"<script>$js</script>"

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = name, c3 = true, runScript = Some(imageJs))
    val htmlFile = new File(dir, htmlFileName)
    Util.writeFile(htmlFile, text)
    logger.info("Wrote file " + htmlFile.getAbsolutePath)
  }

  makeImageHtml()

  // ------------------------------------------------------------------------------------------------------
}
