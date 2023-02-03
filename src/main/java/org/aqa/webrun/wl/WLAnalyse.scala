package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.ExtendedData

case class WLAnalyse(extendedData: ExtendedData, runReq: WLRunReq) {

  def process(rtimage: AttributeList): WLImageResult = {
    val processImage = new WLProcessImage(extendedData, rtimage, runReq)
    val imageResult = processImage.process
    imageResult
  }

  runReq.epidList.foreach(process) // TODO rm
  //runReq.epidList.par.foreach(process) // TODO put in

}
