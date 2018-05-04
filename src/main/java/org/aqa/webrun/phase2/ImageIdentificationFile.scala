package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.ImageIdentification

/**
 *    TODO
 */
case class ImageIdentificationFile(dicomFile: DicomFile, imageIdentification: ImageIdentification) {
  def move(oldDir: File, newDir: File): ImageIdentificationFile = {
    if (dicomFile.file.getParentFile == oldDir) {
      new ImageIdentificationFile(new DicomFile(new File(newDir, dicomFile.file.getName)), imageIdentification)
    } else this
  }
}
