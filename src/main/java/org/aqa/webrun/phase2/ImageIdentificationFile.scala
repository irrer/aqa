package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.ImageIdentification
import org.aqa.Util

/**
 *    TODO
 */
case class ImageIdentificationFile(dicomFile: DicomFile, imageIdentification: ImageIdentification) {
  def reDir(dir: File) = new ImageIdentificationFile(dicomFile.reDir(dir), imageIdentification)
}
