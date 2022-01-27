package org.aqa.simpleRtPlan

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities
import org.aqa.Config
import org.aqa.Util

import java.io.File

case class TemplateFileRef(file: File, modality: String, UID: String) {

  def fileToDicom(): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }
}

class TemplateFiles {

  private def listFiles(file: File, list: Seq[File]): Seq[File] = {
    if (file.isDirectory) {
      list ++ Util.listDirFiles(file).flatMap(f => listFiles(f, Seq()))
    } else {
      if (DicomFileUtilities.isDicomOrAcrNemaFile(file))
        list :+ file
      else
        list
    }
  }

  private def makeTemplateFileRef(file: File): TemplateFileRef = {
    val al = new AttributeList
    al.read(file)
    TemplateFileRef(file, Util.modalityOfAl(al), Util.sopOfAl(al))
  }

  val fileList: Seq[TemplateFileRef] = {
    val dir = Config.SimpleRtplanTemplateDir
    if (dir.isDefined && dir.get.isDirectory)
      listFiles(dir.get, Seq()).map(makeTemplateFileRef)
    else
      Seq()
  }

  def ofModality(modality: String): Seq[TemplateFileRef] = fileList.filter(_.modality.equals(modality))

}
