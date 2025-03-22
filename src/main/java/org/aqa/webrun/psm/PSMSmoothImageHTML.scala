package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.Config
import org.aqa.web.C3Chart

import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.Rectangle
import java.io.File

class PSMSmoothImageHTML(extendedData: ExtendedData, dicomImage: DicomImage, resultList: Seq[PSMBeamAnalysisResult]) extends Logging {

  val imageFileName = "smoothContouredImage.png"
  private val imageFile = new File(extendedData.output.dir, imageFileName)

  val htmlFileName = "smoothContouredImage.html"
  private val htmlFile = new File(extendedData.output.dir, htmlFileName)

  val trans = new IsoImagePlaneTranslator(resultList.head.rtimage)

  private def makeBufferedImage(dicomImage: DicomImage, resultList: Seq[PSMBeamAnalysisResult]): BufferedImage = {

    val colorMap = {
      val white = ImageUtil.rgbColorMap(Color.white)
      val blue = ImageUtil.rgbColorMap(new Color(140, 180, 255))

      (0 until 256).map(i => if ((i % 30) == 0) blue(i) else white(i))
    }

    val bufImg = dicomImage.toBufferedImage(colorMap)

    def drawCircle(result: PSMBeamAnalysisResult): Unit = {
      val gc = ImageUtil.getGraphics(bufImg)
      gc.setColor(Color.black)
      val center_pix = trans.iso2Pix(result.psmBeam.xCenter_mm, result.psmBeam.yCenter_mm)

      val width = trans.iso2PixDistX(Config.PSMRadius_mm * 2).toInt
      val height = trans.iso2PixDistY(Config.PSMRadius_mm * 2).toInt

      gc.drawOval((center_pix.getX - width / 2).toInt, (center_pix.getY - height / 2).toInt, width, height)
    }

    resultList.foreach(drawCircle)

    Util.addGraticules(bufImg, trans, Color.GRAY)

    bufImg
  }

  private val axialChart = {
    val xValueList = (0 until dicomImage.height).map(y => trans.pix2IsoCoordY(y))
    val area = new Rectangle((dicomImage.width / 2) - 1, 0, 2, dicomImage.height)
    val yValues = dicomImage.getSubimage(area).rowSums.map(_ / 2.0)
    new C3Chart(
      xAxisLabel = "Position mm", //
      xDataLabel = "Position mm", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Level"), //
      yDataLabel = "Level", //
      yValues = Seq(yValues)
    )
  }

  private val transverseChart = {
    val xValueList = (0 until dicomImage.width).map(x => trans.pix2IsoCoordX(x))
    val yValues = dicomImage.getSubimage(new Rectangle(0, (dicomImage.height / 2) - 1, dicomImage.width, 2)).columnSums.map(_ / 2.0)
    new C3Chart(
      xAxisLabel = "Position mm", //
      xDataLabel = "Position mm", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Level"), //
      yDataLabel = "Level", //
      yValues = Seq(yValues)
    )
  }

  def make(): Unit = {

    val bufImage = makeBufferedImage(dicomImage, resultList)
    Util.writePng(bufImage, imageFile)
    logger.info("Wrote PSM smooth contoured image " + imageFile.getAbsolutePath)

    val content = {
      <div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <h4 style="text-align: center;">PSM Image Normalized</h4>
            <p style="text-align: center;">With regularly spaced contour lines.</p>
            <img src={imageFileName} class="img-responsive" alt="Composite image showing centers of all beams."/>
          </div>
        </div>


        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <h4>Axial Profile</h4>
            {axialChart.html}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <h4>Transverse Profile</h4>
            {transverseChart.html}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <p style="margin-bottom:150px;"> </p>
          </div>
        </div>
        
      </div>
    }

    val js = s"<script>\n${axialChart.javascript}\n${transverseChart.javascript}\n</script>"

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM Image", c3 = true, runScript = Some(js))
    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote smooth contoured index file ${htmlFile.getAbsolutePath}")
  }

}
