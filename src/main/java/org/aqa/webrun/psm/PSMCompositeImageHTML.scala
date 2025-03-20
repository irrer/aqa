package org.aqa.webrun.psm

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.Logging

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.vecmath.Point2i

class PSMCompositeImageHTML(extendedData: ExtendedData) extends Logging {

  private val compositeImageDirName = "compositeImage"
  private val compositeDir = new File(extendedData.output.dir, compositeImageDirName)
  private val compositeImageFileName = "compositeImage.png"
  val compositeDirRef = s"$compositeImageDirName/index.html"
  val compositeImageRef = s"$compositeImageDirName/$compositeImageFileName"

  private def fmt(d: Double): String = d.formatted("%8.2f").trim

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

    Config.applyWatermark(image)

    resultList.foreach(annotateCompositeResult)
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

  def make(resultList: Seq[PSMBeamAnalysisResult]): String = {

    compositeDir.mkdirs()

    val compositeImage = makeCompositeImage(resultList)

    annotateCompositeImage(compositeImage, resultList)
    val compositeFile = new File(extendedData.output.dir, compositeImageFileName)
    Util.writePng(compositeImage, compositeFile)
    logger.info(s"Wrote composite image file ${compositeFile.getAbsolutePath}")

    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1" >
          <h4 style="text-align: center;">Mean CU Readings for each beam center</h4>
          <img src={compositeImageFileName}/>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM RTPLAN", runScript = None)
    val indexFile = new File(compositeDir, "index.html")
    Util.writeFile(indexFile, text)
    logger.info(s"Wrote composite index file ${indexFile.getAbsolutePath}")

    compositeDirRef
  }

}
