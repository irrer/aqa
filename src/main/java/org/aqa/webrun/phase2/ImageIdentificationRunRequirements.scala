package org.aqa.webrun.phase2

import org.aqa.db.Machine
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.RunRequirements
import org.aqa.Util

/**
 * Encapsulate the data necessary to perform the ImageIdentification part of the Phase2 procedure.
 */
case class ImageIdentificationRunRequirements(machine: Machine, sessionDir: File, plan: DicomFile, imageIdFileList: Seq[ImageIdentificationFile])
  extends RunRequirements[ImageIdentificationRunRequirements] {
  override val fileList = plan.file +: imageIdFileList.map(iid => iid.dicomFile.file).toIndexedSeq
  override def reDir(dir: File) = {
    new ImageIdentificationRunRequirements(machine, sessionDir, plan.reDir(dir), imageIdFileList.map(ii => ii.reDir(dir)))
  }
}

