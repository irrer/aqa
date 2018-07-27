package org.aqa.webrun.phase2

import org.aqa.DicomFile
import org.aqa.Config

object PositioningCheckValidation extends Phase2Validation {

  /**
   * Validate the given data, and, if it is valid, return None, else return a message indicating the problem.
   */
  override def validate(runReq: RunReq): Option[String] = {
    val foundBeams = Config.PositioningCheckBeamNameList.filter(beamName => runReq.rtimageMap.get(beamName).isDefined)
    val missing = Config.PositioningCheckBeamNameList.diff(foundBeams)

    if (missing.nonEmpty) Some("Beam(s) missing for positioning check: " + missing.mkString("  "))
    else None

  }
}