package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData

case class WLAnalyse(extendedData: ExtendedData, runReq: WLRunReq) {

  def process(rtimage: AttributeList): ProcedureStatus.Value = {
    val processImage = new WLProcessImage(extendedData, rtimage)
    val imageResult = processImage.process
    if (imageResult.imageStatus == ImageStatus.Passed)
      ProcedureStatus.pass
    else
      ProcedureStatus.fail
  }

  runReq.epidList.foreach(process) // TODO rm
  //runReq.epidList.par.foreach(process) // TODO put in

}
