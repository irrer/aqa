package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import org.aqa.Logging
import org.aqa.run.RunReqClass

/**
  * Container for validated data needed to run procedure.
  *
  * @param extendedData Metadata from database.
  * @param rtplan RTPLAN referenced.
  * @param rtimageMap Map of RTIMAGEs by beam name.
  */
case class GapSkewRunReq(rtplan: AttributeList, rtimageMap: Map[String, AttributeList]) extends RunReqClass with Logging {}
