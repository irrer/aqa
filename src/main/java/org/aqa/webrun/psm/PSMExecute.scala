package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.PSM
import org.aqa.db.PSMBeam
import org.aqa.webrun.psm.html.PSMCompositeImageHTML
import org.aqa.webrun.psm.html.PSMMainHTML

import java.awt.geom.Point2D

class PSMExecute(extendedData: ExtendedData, psmRunReq: PSMRunReq) extends Logging {

  private def makeRawImage(wdImg: DicomImage, ffImg: DicomImage): DicomImage = {
    def doRow(y: Int): IndexedSeq[Float] =
      (0 until wdImg.width).map(x => wdImg.get(x, y) * ffImg.get(x, y))

    val rawImgPixels = (0 until wdImg.height).map(doRow)

    val rawImg = new DicomImage(rawImgPixels)
    rawImg

  }

  /**
    * Perform the math of <code>raw / br</code>
    *
    * @param rawImg Raw image.
    * @param brImg  BR (Beam Response) image.
    * @return PSM image.
    */
  private def JmakePsmImage(rawImg: DicomImage, brImg: DicomImage): DicomImage = {
    def doRow(y: Int): IndexedSeq[Float] = {

      /**
        * Process one pixel in a row.  If the BR value is 0 then return 0.
        *
        * @param x X coordinate of pixel.
        * @return
        */
      def doPixel(x: Int): Float = {
        val br = brImg.get(x, y)
        if (br == 0)
          0
        else
          rawImg.get(x, y) / br
      }

      (0 until rawImg.width).map(doPixel)
    }

    val psmImgPixels = (0 until rawImg.height).map(doRow)

    val psmImg = new DicomImage(psmImgPixels)
    psmImg

  }

  private val trans = new IsoImagePlaneTranslator(psmRunReq.rtimageList.head)

  private def timeOf(al: AttributeList) = Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

  private val rtplan: AttributeList = psmRunReq.rtplan

  private val resultList: List[PSMBeamAnalysisResult] = {
    def process(rtimage: AttributeList) = PSMBeamAnalysis(rtplan, extendedData, trans, rtimage: AttributeList, psmRunReq).measure()

    val list = {
      val l = psmRunReq.rtimageList.par.map(process)
      val pointZero = new Point2D.Double(0.0, 0.0)

      def distToCenter(r: PSMBeamAnalysisResult): Double = {
        val p = new Point2D.Double(r.psmBeam.xCenter_mm, r.psmBeam.yCenter_mm)
        p.distance(pointZero)
      }
      val centerBeam = l.minBy(distToCenter)

      val centerBeamResponse = centerBeam.psmBeam.mean_cu * centerBeam.psmBeam.floodField_cu.get

      /**
        * Fix the beamResponseNormalized.
        * @param result For this response.
        * @return A new result with the normalized beam response fixed.
        */
      def fix(result: PSMBeamAnalysisResult): PSMBeamAnalysisResult = {

        /**
          * Show a nicely formatted version of results in the log.
          *
          * @param newPsmBeam Log this beam.
          */
        def logResult(newPsmBeam: PSMBeam): Unit = {
          def fmt(od: Option[Double]): String = { if (od.isDefined) "%32.28f".format(od.get) else " NA " }

          val pc = trans.iso2Pix(result.psmBeam.center)
          val nm = "%-6s".format(newPsmBeam.beamName)

          val center = {
            def fm(d: Double) = "%8.2f".format(d)
            s"center pix: ${fm(pc.getX)}, ${fm(pc.getY)}"
          }

          logger.info(
            s"PSM Beam $nm    $center" +
              s"    floodField_cu: ${fmt(newPsmBeam.floodField_cu)}" +
              s"    mean_cu: ${fmt(Some(newPsmBeam.mean_cu))}" +
              s"    beamResponseNormalized: ${fmt(newPsmBeam.beamResponseNormalized)}"
          )

        }

        val beamResponseNormalized = (result.psmBeam.mean_cu * result.psmBeam.floodField_cu.get) / centerBeamResponse

        val newPsmBeam = result.psmBeam.copy(beamResponseNormalized = Some(beamResponseNormalized))
        val newResult = result.copy(psmBeam = newPsmBeam)

        logResult(newPsmBeam)

        newResult
      }
      l.map(fix)
    }
    list.toList.sortBy(r => timeOf(r.rtimage))
  }

  private val grid = PSMGrid(resultList)

  private val interpolator: Option[PSMInterpolator] = {
    if (grid.canBeInterpolated)
      Some(new PSMInterpolator(resultList))
    else
      None
  }

  private val gradientAscent: Option[PSMGradientAscent] = interpolator.map(new PSMGradientAscent(_))

  /*
  private val psmImage: Option[DicomImage] = {
    if (gradientAscent.isEmpty)
      None
    else {
      ???
    }
  }
   */

  // ----------------------------------------------------------------------------------------

  // main processing.  Create a scaled DicomImage and Attribute list for each value.

  // private val ffImg = new DicomImage(psmRunReq.floodField.dicom).scalePixels(psmRunReq.floodField.dicom)
  private val ffImageNormalized = psmRunReq.floodField.dicomImageNormalized

  // private val wdAl = psmRunReq.wholeDetector
  // private val wdImg = new DicomImage(wdAl).scalePixels(wdAl)

  // private val rawImg = makeRawImage(wdImg, ffImgNormalized)

  private val compositeNonNormalizedImg = new PSMCompositeImageHTML(extendedData, "compositeNonNormalized").makeCompositeImage(resultList)

  // private val brImg = interpolator.map(i => i.normalizedDicomImage)

  private val psmImg = interpolator.map(i => i.normalizedDicomImage)

  // ----------------------------------------------------------------------------------------

  private val psm: Option[PSM] = {
    if (psmImg.isDefined && gradientAscent.isDefined)
      Some(
        PSM.makePSM(
          outputPK = extendedData.outputPK,
          floodFieldImageHash_md5 = psmRunReq.floodField.imageHash_md5,
          image = psmImg.get,
          xMax_mm = gradientAscent.get.getMaxPoint_iso.getX,
          yMax_mm = gradientAscent.get.getMaxPoint_iso.getY,
          psmRunReq.floodField.dicom
        )
      )
    else
      None
  }

  if (psm.isDefined) {
    psm.get.insert
    logger.info(s"Inserted PSM into database.")
  } else {
    logger.info(s"No PSM created.")
  }

  private val insertedList = resultList.map(result => result.psmBeam.insert)
  logger.info(s"Inserted ${insertedList.length} PSMBeam rows into database.")

  /* For validating against Matlab implementation.
  if (true) {
    PSMImitate(psmRunReq.floodField.dicom, psmRunReq.rtimageList, grid)
  }
   */

  private val wdImg = psmRunReq.wholeDetector.map(wd => new DicomImage(wd))

  private val mainHTML = new PSMMainHTML(
    extendedData = extendedData,
    rtplan = rtplan,
    resultList = resultList,
    psmGradientAscent = gradientAscent,
    ffAl = psmRunReq.floodField.dicom,
    ffImgNormalized = ffImageNormalized,
    wdAl = psmRunReq.wholeDetector,
    wdImg = wdImg,
    // rawImg = rawImg,
    beamResponsesNotNormalizedImg = compositeNonNormalizedImg,
    // brImg = brImg,
    psmImg = psmImg,
    psmRunReq
  )

  mainHTML.make()
  /*
   */

}
