package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.PositioningCheck
import org.aqa.Util

/**
 *    TODO
 */
case class XPositioningCheckFile(dicomFile: DicomFile, positioningCheck: PositioningCheck) {
  def reDir(dir: File) = new XPositioningCheckFile(dicomFile.reDir(dir), positioningCheck)
}
