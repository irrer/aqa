package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.restlet.Response

import javax.vecmath.Point2i

class PSMExecute(extendedData: ExtendedData, runReq: PSMRunReq, response: Response) {

  val radius_mm = 5.0 // TODO make this a configuration parameter

  val trans = new IsoImagePlaneTranslator(runReq.rtimageList.head)

  /**
    * Make a list of coordinates that are within a circle centered at 0,0.
    */
  val pointList: Seq[Point2i] = {
    val xHi = (trans.iso2PixDistX(radius_mm) + 2).round.toInt
    val xLo = -xHi

    val yHi = (trans.iso2PixDistY(radius_mm) + 2).round.toInt
    val yLo = -xHi

    def nearCenter(x: Int, y: Int): Boolean = {
      val xyIso = trans.pix2Iso(x, y)
      val xIso = xyIso.getX
      val yIso = xyIso.getY
      val distance = Math.sqrt((xIso * xIso) + (yIso * yIso))

      distance <= radius_mm
    }

    val list = for (x <- xLo until xHi; y <- yLo until yHi; if nearCenter(x, y)) yield new Point2i()
    list
  }

  val rtplan = runReq.rtplan

  runReq.rtimageList.map(rtimage => PSMBeamAnalysis(rtplan, pointList, trans, rtimage: AttributeList).measure(extendedData.output.dir))

}
