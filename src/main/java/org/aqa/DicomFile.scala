package org.aqa

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TagFromName

case class DicomFile(file: File) extends Logging {
  private lazy val readResult = Util.readDicomFile(file)
  lazy val valid = readResult.isRight
  lazy val attributeList: Option[AttributeList] = if (readResult.isRight) Some(readResult.right.get) else None
  lazy val error: Option[Throwable] = if (readResult.isLeft) Some(readResult.left.get) else None

  def isModality(sopClassUID: String): Boolean = {
    valid && Util.isModality(attributeList.get, sopClassUID)
  }
}

object DicomFile {

  /**
   * Return a list of all the DICOM files in the given directory.  Return the list of all files, DICOM or not.
   */
  def readDicomInDir(dir: File): Seq[DicomFile] = {
    Util.listDirFiles(dir).map(f => new DicomFile(f))
  }

  /**
   * Return a list of DicomFiles that differ by SOPInstanceUID.
   */
  def distinctSOPInstanceUID(dicomFileList: Seq[DicomFile]): Seq[DicomFile] = {
    dicomFileList.filter(df => df.valid).map(df => (Util.sopOfAl(df.attributeList.get), df)).toMap.values.toSeq
  }
}