package org.aqa.webrun.phase2.metadataCheck

import org.aqa.DicomFile
import org.aqa.Config
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.Phase2Validation
import org.aqa.webrun.phase2.Phase2Validation
import org.aqa.webrun.phase2.Phase2Validation

object MetadataCheckValidation extends Phase2Validation {

  /**
   * Validate the given data, and, if it is valid, return None, else return a message indicating the problem.
   */
  override def validate(runReq: RunReq): Option[String] = {
    None
  }
}
