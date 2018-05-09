package org.aqa.webrun.phase2

import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.RunRequirements

/**
 * Encapsulate the data necessary to perform the CollimatorCentering part of the Phase2 procedure.
 */
case class CollimatorCenteringRunRequirements(image090: DicomFile, image270: DicomFile)
  extends RunRequirements[CollimatorCenteringRunRequirements] {
  override val fileList = IndexedSeq(image090.file, image270.file)
  override def reDir(dir: File) = new CollimatorCenteringRunRequirements(image090.reDir(dir), image270.reDir(dir))
}
