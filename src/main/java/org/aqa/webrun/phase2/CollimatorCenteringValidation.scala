package org.aqa.webrun.phase2

import org.aqa.Config

/**
 * Validate the inputs for CollimatorCentering.  Require the 90 and 270 files to exist.
 */
object CollimatorCenteringValidation extends Phase2Validation {

  /**
   * Validate the given data.
   */

  override def validate(runReq: RunReq): Option[String] = {
    val file090 = runReq.rtimageMap.get(Config.CollimatorCentering090BeamName)
    val file270 = runReq.rtimageMap.get(Config.CollimatorCentering090BeamName)

    val result = (file090, file270) match {
      case (None, None) => Some("Neither the 90 image associated with beam " + Config.CollimatorCentering090BeamName + " or the " + "could not be found.")
      case (None, _) => Some("The 90 image associated with beam " + Config.CollimatorCentering090BeamName + " could not be found.")
      case (_, None) => Some("The 270 image associated with beam " + Config.CollimatorCentering270BeamName + " could not be found.")
      case (Some(f90), Some(f270)) => None // success
    }

    result
  }

}