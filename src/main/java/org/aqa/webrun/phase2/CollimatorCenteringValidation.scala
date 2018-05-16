package org.aqa.webrun.phase2

import org.aqa.web.WebUtil._
import com.pixelmed.dicom.SOPClass
import org.aqa.db.Machine
import org.aqa.Util
import org.aqa.DicomFile
import org.aqa.db.CollimatorCentering
import org.aqa.Config

/**
 * Validate the inputs for CollimatorCentering.
 */
object CollimatorCenteringValidation {

  private val MIN_IMAGES = 3

  private def formErr(msg: String, uploadFileInput: Option[IsInput]) = Left(Error.make(uploadFileInput.get, msg))

  /**
   * Validate the given data, and, if it is valid, organize it into a <code>CollimatorCenteringRunRequirements</code> object.  If
   * it is not valid, then return a message indicating the problem.
   */
  def validate(valueMap: ValueMapT, plan: DicomFile, rtImageList: IndexedSeq[DicomFile], uploadFileInput: Option[IsInput]): Either[StyleMapT, CollimatorCenteringRunRequirements] = {

    val fileFlood = Phase2Util.findRtimageByBeamName(plan, rtImageList, Config.FloodFieldBeamName)
    val file090 = Phase2Util.findRtimageByBeamName(plan, rtImageList, Config.CollimatorCentering090BeamName)
    val file270 = Phase2Util.findRtimageByBeamName(plan, rtImageList, Config.CollimatorCentering270BeamName)

    val result = (fileFlood, file090, file270) match {
      case (Some(flood), Some(f090), Some(f270)) => Right(new CollimatorCenteringRunRequirements(flood, f090, f270))
      case (None, _, _) => formErr("The flood field image associated with beam " + Config.FloodFieldBeamName + " could be found.", uploadFileInput)
      case (_, None, _) => formErr("The 90 image associated with beam " + Config.CollimatorCentering090BeamName + " could be found.", uploadFileInput)
      case (_, _, None) => formErr("The 270 image associated with beam " + Config.CollimatorCentering270BeamName + " could be found.", uploadFileInput)
    }

    result
  }

}