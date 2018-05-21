package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.PositioningCheck
import org.aqa.Util

/**
 * Associate results with file.
 */
case class XPositioningCheckFile(dicomFile: DicomFile, positioningCheck: PositioningCheck) {
  //def reDir(dir: File) = new PositioningCheckFile(dicomFile.reDir(dir), positioningCheck)
}
