package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.FloodField
import org.aqa.db.PSM
import org.aqa.webrun.psm.html.PSMCompositeImageHTML
import org.aqa.webrun.psm.html.PSMMainHTML

class PSMExecute(extendedData: ExtendedData, runReq: PSMRunReq) extends Logging {

  private def makeRawImage(wdImg: DicomImage, ffImg: DicomImage): DicomImage = {
    def doRow(y: Int): IndexedSeq[Float] =
      (0 until wdImg.width).map(x => wdImg.get(x, y) * ffImg.get(x, y))

    val rawImgPixels = (0 until wdImg.height).map(doRow)

    val rawImg = new DicomImage(rawImgPixels)
    rawImg

  }

  /**
    * Perform the math of <code>raw / br</code>
    * @param rawImg Raw image.
    * @param brImg BR (Beam Response) image.
    * @return PSM image.
    */
  private def makePsmImage(rawImg: DicomImage, brImg: DicomImage): DicomImage = {
    def doRow(y: Int): IndexedSeq[Float] = {

      /**
        * Process one pixel in a row.  If the BR value is 0 then return 0.
        * @param x X coordinate of pixel.
        * @return
        */
      def doPixel(x: Int): Float = {
        val br = brImg.get(x, y)
        if (br == 0)
          0
        else
          wdImg.get(x, y) / br
      }

      (0 until wdImg.width).map(doPixel)
    }

    val rawImgPixels = (0 until wdImg.height).map(doRow)

    val rawImg = new DicomImage(rawImgPixels)
    rawImg

  }

  private val trans = new IsoImagePlaneTranslator(runReq.rtimageList.head)

  private def timeOf(al: AttributeList) = Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

  private val rtplan: AttributeList = runReq.rtplan

  private val resultList = {
    def process(rtimage: AttributeList) = PSMBeamAnalysis(rtplan, extendedData, trans, rtimage: AttributeList).measure()

    val list = runReq.rtimageList.par.map(process)
    list.toList.sortBy(r => timeOf(r.rtimage))
  }

  private val interpolator = new PSMInterpolator(resultList)

  private val gradientAscent = new PSMGradientAscent(interpolator)

  // ----------------------------------------------------------------------------------------

  // main processing.  Create a scaled DicomImage and Attribute list for each value.

  private val ffAl = runReq.floodField
  private val ffImg = new DicomImage(ffAl).scalePixels(ffAl)

  private val wdAl = runReq.wholeDetector
  private val wdImg = new DicomImage(wdAl).scalePixels(wdAl)

  private val rawImg = makeRawImage(wdImg, ffImg)

  private val cbrImg = new PSMCompositeImageHTML(extendedData).makeCompositeImage(resultList)

  private val brImg = interpolator.normalizedDicomImage

  private val psmImg = makePsmImage(rawImg, brImg)

  // ----------------------------------------------------------------------------------------

  private def getReferencedFloodField: FloodField = {
    val uploadedFloodFieldHash = FloodField.makeFloodField(extendedData.output.outputPK.get, runReq.floodField).imageHash_md5
    val ff = FloodField.getByImageHash(extendedData.machine.machinePK.get, uploadedFloodFieldHash)
    ff.head
  }

  private val psm = PSM.makePSM(
    outputPK = extendedData.outputPK,
    floodFieldImageHash_md5 = getReferencedFloodField.imageHash_md5,
    image = psmImg,
    xMax_mm = gradientAscent.getMaxPoint_iso.getX,
    yMax_mm = gradientAscent.getMaxPoint_iso.getY,
    wdAl
  )

  psm.insert
  logger.info(s"Inserted PSM into database.")

  private val insertedList = resultList.map(result => result.psmBeam.insert)
  logger.info(s"Inserted ${insertedList.length} PSMBeam rows into database.")

  private val mainHTML = new PSMMainHTML(
    extendedData = extendedData,
    rtplan = rtplan,
    resultList = resultList,
    psmGradientAscent = gradientAscent,
    ffAl = ffAl,
    ffImg = ffImg,
    wdAl = wdAl,
    wdImg = wdImg,
    rawImg = rawImg,
    cbrImg = cbrImg,
    brImg = brImg,
    psmImg = psmImg
  )

  mainHTML.make()

}
