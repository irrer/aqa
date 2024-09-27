package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.WebUtil

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.vecmath.Point2i
import scala.xml.Elem

object PSMHTML extends Logging {

  private def fmt(d: Double): String = d.formatted("%8.2f").trim

  /**
    * Annotate the image with the mean CU in the center of the beam.  Use the existing buffered image.
    * @param result For this beam result.
    */
  private def annotateImage(result: PSMBeamAnalysisResult): Unit = {
    val gc = ImageUtil.getGraphics(result.bufferedImage)
    gc.setColor(Color.black)
    val trans = new IsoImagePlaneTranslator(result.rtimage)
    val center_pix = trans.iso2Pix(result.psmBeam.xCenter_mm, result.psmBeam.yCenter_mm)

    val text1 = "Mean CU"
    val text2 = fmt(result.psmBeam.mean_cu)
    val offset = ImageText.getTextDimensions(gc, text1).getHeight / 2
    ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY - offset, text1)
    ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY + offset, text2)

    val width = trans.iso2PixDistX(Config.PSMRadius_mm).toInt
    val height = trans.iso2PixDistY(Config.PSMRadius_mm).toInt
    gc.drawOval((center_pix.getX - width / 2).toInt, (center_pix.getY - height / 2).toInt, width, height)
  }

  /**
    * Put the CU for each image on the composite image.
    * @param image Composite image.
    * @param resultList List of analysis results.
    */
  private def annotateCompositeImage(image: BufferedImage, resultList: Seq[PSMBeamAnalysisResult]): Unit = {
    val trans = new IsoImagePlaneTranslator(resultList.head.rtimage)
    val gc = ImageUtil.getGraphics(image)
    gc.setColor(Color.white)
    val fontOffset = ImageText.getTextDimensions(gc, "123").getHeight / 2

    def annotateCU(result: PSMBeamAnalysisResult): Unit = {
      val textCU = fmt(result.psmBeam.mean_cu)
      val x = trans.iso2PixCoordX(result.psmBeam.xCenter_mm)
      val y = trans.iso2PixCoordX(result.psmBeam.yCenter_mm - Config.PSMRadius_mm) - fontOffset
      ImageText.drawTextCenteredAt(gc, x, y, textCU)
    }

    def annotateLocation(result: PSMBeamAnalysisResult): Unit = {
      val textCU = fmt(result.psmBeam.xCenter_mm) + ", " + fmt(result.psmBeam.yCenter_mm)
      val x = trans.iso2PixCoordX(result.psmBeam.xCenter_mm)
      val y = trans.iso2PixCoordX(result.psmBeam.yCenter_mm + Config.PSMRadius_mm) + fontOffset + 2
      ImageText.drawTextCenteredAt(gc, x, y, textCU)
    }

    def annotateCompositeResult(result: PSMBeamAnalysisResult): Unit = {
      annotateCU(result)
      annotateLocation(result)
    }
    resultList.foreach(annotateCompositeResult)
  }

  /**
    * Make a web page for one result.
    * @param extendedData Metadata
    * @param result Result for one RTIMAGE.
    * @param pngFile png file.
    * @param htmlFile html file.
    */
  private def makeRtimageWebPage(extendedData: ExtendedData, result: PSMBeamAnalysisResult, pngFile: File, htmlFile: File): Unit = {

    val content = {
      <div>
        <div style="text-align:center;">
          <a href="../display.html">Back to main</a>
          <h3>{result.psmBeam.beamName}</h3>
          <img src={pngFile.getName} alt="Full DICOM Image" class="center"/>
        </div>
        <pre style="margin-top: 10px;margin-bottom: 100px;">
          {WebUtil.nl + DicomUtil.attributeListToString(result.rtimage)}
        </pre>
      </div>
    }
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)
    Util.writeFile(htmlFile, text)
  }

  /**
    * Make a web page for one result.
    * @param extendedData Metadata
    * @param result Result for one RTIMAGE.
    * @param pngFile png file.
    * @param beamDir beam directory file.
    * @param htmlLink Points to web page for the result.
    */
  private def makeTableContent(extendedData: ExtendedData, result: PSMBeamAnalysisResult, pngFile: File, beamDir: File, htmlLink: String): Elem = {
    val src = { beamDir.getName + "/" + pngFile.getName }
    <td style="text-align:center;">
      <a href={htmlLink}>
        <h4>{result.psmBeam.beamName + " : " + fmt(result.psmBeam.mean_cu)}</h4>
        <img src={src} width="120" alt="Full DICOM Image" class="center"/>
      </a>
    </td>
  }

  /**
    * Make a composite image that contains all of the
    * @param resultList results from all beams.
    * @return
    */
  private def makeCompositeImage(resultList: Seq[PSMBeamAnalysisResult]): BufferedImage = {
    // Get all values from all images so a global max and min can be established
    val dropCount = 10 // drop this many high and low values to get rid of outliers (bad pixels)
    val allValues = resultList.flatMap(_.pixelList.values).sorted.drop(dropCount).dropRight(dropCount)
    val min = allValues.head.toFloat

    val pixelArray = {
      val width = resultList.head.rtimage.get(TagByName.Columns).getIntegerValues.head
      val height = resultList.head.rtimage.get(TagByName.Rows).getIntegerValues.head

      val allPix = resultList.flatMap(_.pixelList).toMap

      def pixVal(x: Int, y: Int): Float = {
        allPix.get(new Point2i(x, y)) match {
          case Some(value) => value.toFloat
          case _           => min
        }
      }

      val pa = (0 until height).map(y => (0 until width).map(x => pixVal(x, y)))
      pa
    }

    val bufImg = new DicomImage(pixelArray).toBufferedImage(Color.white)

    bufImg
  }

  /**
    * Use bicubic spline to make a smooth image.
    * @param resultList For these results.
    * @return An image.
    */
  private def makeSmoothCompositeImage(resultList: Seq[PSMBeamAnalysisResult]): BufferedImage = {

    val trans = new IsoImagePlaneTranslator(resultList.head.rtimage)

    val sorted = PSMUtil.sortByXYLocation(resultList)

    def establishRegularlySpacedCoordinates(list: Seq[Double]): Seq[Double] = {
      val interval = (list.max - list.min) / list.size
      val first = list.min.round
      val coordinateList = list.indices.map(i => first + (i * interval))
      coordinateList
    }

    val xCoordinateList: Seq[Double] = {
      def getColumn(columnIndex: Int) = sorted.map(row => row(columnIndex))
      val columnList = sorted.head.indices.map(getColumn)
      val meanList = columnList.map(_.map(_.psmBeam.xCenter_mm).sum / sorted.head.size).map(iso => trans.iso2PixCoordX(iso))
      establishRegularlySpacedCoordinates(meanList)
    }

    val yCoordinateList: Seq[Double] = {
      val meanList = sorted.map(row => row.map(_.psmBeam.yCenter_mm).sum / sorted.size).map(iso => trans.iso2PixCoordY(iso))
      establishRegularlySpacedCoordinates(meanList)
    }

    val interpolator = new PiecewiseBicubicSplineInterpolator()

    val valueList = {
      def toCol(colIndex: Int): Array[Double] = sorted.map(row => row(colIndex).psmBeam.mean_cu).toArray
      sorted.head.indices.map(toCol).toArray
    }

    val function = interpolator.interpolate(xCoordinateList.toArray, yCoordinateList.toArray, valueList)

    val Rows = resultList.head.rtimage.get(TagByName.Rows).getIntegerValues.head
    val Columns = resultList.head.rtimage.get(TagByName.Columns).getIntegerValues.head

    val min = resultList.map(_.psmBeam.mean_cu).min.toFloat

    def makeRow(y: Int): IndexedSeq[Float] = {
      (0 until Columns).map(x => {
        ///if ((y >= xCoordinateList.head) && (y <= xCoordinateList.last) && (x >= xCoordinateList.head) && (x <= yCoordinateList.last))
        ///if ((y >= minXY) && (y <= maxXY) && (x >= minXY) && (x <= maxXY))
        if ((y > yCoordinateList.head) && (y < yCoordinateList.last) && (x > xCoordinateList.head) && (x < xCoordinateList.last))
          function.value(x, y).toFloat
        else
          min
      })
    }

    val pixelArray = (0 until Rows).map(makeRow)

    val di = new DicomImage(pixelArray)

    val bufImg = di.toBufferedImage(Color.white)
    bufImg
  }

  /**
    * Make a web page for one result.
    * @param extendedData metadata.
    * @param result For this result.
    * @return An HTML snippet that shows a thumbnail and links to the page.
    */
  private def resultToHtml(extendedData: ExtendedData, result: PSMBeamAnalysisResult): Elem = {

    val beamDirName = "beams"

    val beamDir = new File(extendedData.output.dir, beamDirName)
    beamDir.mkdirs

    val fileNamePrefix = "Beam_" + FileUtil.replaceInvalidFileNameCharacters(result.psmBeam.beamName, '_')

    val pngFile = new File(beamDir, fileNamePrefix + ".png")
    val htmlFile = new File(beamDir, fileNamePrefix + ".html")
    val htmlLink = beamDirName + "/" + htmlFile.getName

    annotateImage(result)
    Config.applyWatermark(result.bufferedImage)
    Util.writePng(result.bufferedImage, pngFile)

    makeRtimageWebPage(extendedData, result, pngFile, htmlFile)

    val tableContent = makeTableContent(extendedData, result, pngFile, beamDir, htmlLink)

    tableContent
  }

  private def rowToElem(extendedData: ExtendedData, resultRow: Seq[PSMBeamAnalysisResult]): Elem = {
    <tr>
      {resultRow.map(result => resultToHtml(extendedData, result))}
    </tr>
  }

  /**
    * Make a web page for viewing the RTPLAN and return an HTML snippet to navigate to it.
    * @param extendedData Metadata.
    * @param rtplan For this DICOM RTPLAN.
    * @return HTML reference.
    */
  private def makeRtplanHtml(extendedData: ExtendedData, rtplan: AttributeList): Elem = {
    val htmlFile = new File(extendedData.output.dir, "rtplan.html")
    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1" >
          <a href="display.html">Back to main</a>
          <h3>RTPLAN for PSM</h3>
          <pre style="margin-top: 10px;margin-bottom: 100px;">
            {WebUtil.nl + DicomUtil.attributeListToString(rtplan)}
          </pre>
        </div>
      </div>
    }
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM RTPLAN", runScript = None)
    Util.writeFile(htmlFile, text)

    val reference = {
      <a href={htmlFile.getName}>View RTPLAN</a>
    }
    reference
  }

  /**
    * Write all of the HTML.
    * @param extendedData Metadata.
    * @param resultList List of results.
    */
  def makeHtml(extendedData: ExtendedData, rtplan: AttributeList, resultList: Seq[PSMBeamAnalysisResult]): Unit = {

    val compositeImage = makeCompositeImage(resultList)
    annotateCompositeImage(compositeImage, resultList)
    val compositeFile = new File(extendedData.output.dir, "composite.png")
    Util.writePng(compositeImage, compositeFile)

    // TODO would be nice to do the WHOLE image, but bicubic spline does not support that ...
    val smoothCompositeImage = makeSmoothCompositeImage(resultList)
    val smoothCompositeFile = new File(extendedData.output.dir, "smoothComposite.png")
    Util.writePng(smoothCompositeImage, smoothCompositeFile)

    val planElem = makeRtplanHtml(extendedData, rtplan)

    val content = {
      // <div style="display:flex; align-items:center; justify-content:center; margin-bottom:200px;">
      <div>
        <div class="row">
            {planElem}
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <h4>Mean CU Readings for each beam center.</h4>
            <img src={compositeFile.getName}/>
          </div>
        </div>

        <!--
        <div class="row">
          <div class="col-md-10 col-md-offset-1" style="margin-top:25px; margin-bottom:25px;">
            <h4>Smoothed composite image.</h4>
            <img src={smoothCompositeFile.getName}/>
          </div>
        </div>
        -->
        
        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {PSMUtil.sortByXYLocation(resultList).map(row => rowToElem(extendedData, row))}
            </table>
          </div>
        </div>
      </div>
    }
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)
  }
}
