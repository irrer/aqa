package org.aqa.webrun.mlcqa

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.ExtendedData

/**
 * Container for validated data needed to run procedure.
 *
 * @param extendedData Metadata from database.
 * @param rtplan RTPLAN referenced.
 * @param rtimageMap Map of RTIMAGEs by beam name.
 */
case class MlcQaRunReq(extendedData: Option[ExtendedData], rtplan: AttributeList, rtimageMap: Map[String, AttributeList]) {

}
