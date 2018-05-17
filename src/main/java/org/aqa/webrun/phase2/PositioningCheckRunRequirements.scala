package org.aqa.webrun.phase2

import org.aqa.db.Machine
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.RunRequirements
import org.aqa.Util
import org.aqa.Config

/**
 * Encapsulate the data necessary to perform the PositioningCheck part of the Phase2 procedure.
 */
case class PositioningCheckRunRequirements(sessionDir: File, plan: DicomFile, imageIdFileList: Seq[PositioningCheckFile])
  extends RunRequirements[PositioningCheckRunRequirements] {
  override val fileList = plan.file +: imageIdFileList.map(iid => iid.dicomFile.file).toIndexedSeq
  override def reDir(dir: File) = {
    val newPlan = if (plan.file.getParentFile == Config.sharedDir) plan else plan.reDir(dir)
    new PositioningCheckRunRequirements(sessionDir, newPlan, imageIdFileList.map(ii => ii.reDir(dir)))
  }
}

