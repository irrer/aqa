package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomBeam
import org.aqa.run.RunReqClass

case class PSMRunReq(rtplan: AttributeList, wholeDetector: AttributeList , rtimageList: Seq[AttributeList], floodField: AttributeList) extends RunReqClass {


  case class Beam(   top: Double, bottom: Double,  left: Double, right: Double) {
    val width = (right - left).abs
    val height = (bottom - top).abs
  }

  private def toBeam(rtimage: AttributeList): Beam = {
    val beam = DicomBeam(rtplan, rtimage)
    Beam( beam.y2Jaw.get, beam.y1Jaw.get,  beam.x1Jaw.get, beam.x2Jaw.get )
  }



  val wholeImage = {

    // beam.
  }

}
