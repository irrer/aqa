package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Logging
import org.aqa.webrun.ExtendedData

/**
  * Process a single Winston Lutz image.
  *
  * @param extendedData Metadata.
  * @param rtimage DICOM to process.
  * @param index Indicates the time order in which this image was captured (0 is the first).
  * @param runReq Uploaded DICOM data.
  */
case class WLProcessImage2(extendedData: ExtendedData, rtimage: AttributeList, index: Int, runReq: WLRunReq) extends Logging {

  private val wlMsg = WLMessage(runReq, rtimage)

  val trans = new IsoImagePlaneTranslator(rtimage)

private val wlPreprocessImage =  WLPreprocessImage(rtimage = rtimage, wlMsg = Some(wlMsg))

  private val dicomImage = wlPreprocessImage.preprocessedImage




  val coarseBox = WLCoarseBox(dicomImage, trans, Some(wlMsg))

  val coarseAoiRectangle = coarseBox.locate()
  //

}
