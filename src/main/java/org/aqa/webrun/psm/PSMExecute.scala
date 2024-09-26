package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util
import org.restlet.Response

class PSMExecute(extendedData: ExtendedData, runReq: PSMRunReq, response: Response) extends Logging {

  private val trans = new IsoImagePlaneTranslator(runReq.rtimageList.head)

  private def timeOf(al: AttributeList) = Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

  private val rtplan: AttributeList = runReq.rtplan

  private val resultList = runReq.rtimageList.sortBy(timeOf).par.map(rtimage => PSMBeamAnalysis(rtplan, extendedData, trans, rtimage: AttributeList).measure()).toArray

  private val insertedList = resultList.map(result => result.psmBeam.insert)

  logger.info(s"Inserted ${insertedList.size} PSMBeam rows.")

  PSMHTML.makeHtml(extendedData, resultList)

}
