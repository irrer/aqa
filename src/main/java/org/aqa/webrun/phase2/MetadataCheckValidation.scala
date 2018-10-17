package org.aqa.webrun.phase2

import org.aqa.DicomFile
import org.aqa.Config

object MetadataCheckValidation extends Phase2Validation {

  /**
   * Validate the given data, and, if it is valid, return None, else return a message indicating the problem.
   */
  override def validate(runReq: RunReq): Option[String] = {
    val foundBeams = runReq.rtimageMap.keys.filter(beamName => Config.MetadataCheckBeamNameList.contains(beamName)).toList
    val missing = Config.MetadataCheckBeamNameList.diff(foundBeams)

    if (missing.nonEmpty) Some("Beam(s) missing for positioning check: " + missing.mkString("  "))
    else None

  }
}