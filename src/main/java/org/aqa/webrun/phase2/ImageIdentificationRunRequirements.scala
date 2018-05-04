package org.aqa.webrun.phase2

import org.aqa.db.Machine
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.RunRequirements

/**
 * Encapsulate the data necessary to perform the ImageIdentification part of the Phase2 procedure.
 */
case class ImageIdentificationRunRequirements(machine: Machine, sessionDir: File, plan: DicomFile, imageIdFileList: Seq[ImageIdentificationFile]) extends RunRequirements {
  override val fileList = plan.file +: imageIdFileList.map(iid => iid.dicomFile.file).toIndexedSeq
}

