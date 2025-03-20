package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util
import org.restlet.Response

class PSMExecute(extendedData: ExtendedData, runReq: PSMRunReq, response: Response) extends Logging {

  /**
    * When converting a PSM which is represented in floating point to a DICOM image which is in
    * integers, there will be some round-off errors.  This logs the largest errors.
    * @param before Original PSM
    * @param after PSM after being converted to DICOM and back.
    */
  private def showRoundTripError(before: DicomImage, after: DicomImage): Unit = {

    val top10 = after.pixelData.flatten.zip(before.pixelData.flatten).map(ab => (ab._1 - ab._2).abs).distinct.sorted.takeRight(10)

    logger.info("Top 10 diffs round trip PSM to DICOM back to PSM:  " + top10.mkString("\n    "))
  }

  /**
    * Construct and save the PSM.
    * @param resultList Get prototype from this list.
    */
  private def savePsm(resultList: Array[PSMBeamAnalysisResult]): AttributeList = {

    val psmImage = PSMUtil.makePSMImage(resultList)

    // the first image by chronological delivery date
    val firstRtimage = {
      def dateOf(al: AttributeList): Long = {
        DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get.getTime
      }
      resultList.minBy(r => dateOf(r.rtimage)).rtimage
    }

    val psmDicom = PSMDicom.psmToDicom(psmImage, firstRtimage)

    showRoundTripError(psmImage, PSMDicom.dicomToPsm(psmDicom))

    psmDicom
  }

  private val trans = new IsoImagePlaneTranslator(runReq.rtimageList.head)

  private def timeOf(al: AttributeList) = Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

  private val rtplan: AttributeList = runReq.rtplan

  Trace.trace()
  private val resultList = runReq.rtimageList.sortBy(timeOf).par.map(rtimage => PSMBeamAnalysis(rtplan, extendedData, trans, rtimage: AttributeList).measure()).toArray
  Trace.trace()

  private val psm = savePsm(resultList)
  Trace.trace()

  private val insertedList = resultList.map(result => result.psmBeam.insert)

  logger.info(s"Inserted ${insertedList.length} PSMBeam rows.")

  PSMHTML.makeHtml(extendedData, rtplan, resultList, psm)
}
