/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.procedures

import java.io.File
import org.aqa.Util
import org.aqa.Logging
import org.apache.poi.ss.usermodel.Cell
import org.aqa.run.ProcedureStatus
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter
import org.aqa.web.WebUtil
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import org.aqa.Config
import org.aqa.db.Db
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.DicomFileUtilities
import org.aqa.db.Output
import edu.umro.DicomDict.TagByName

object UploadTransAndOpen extends Logging {

  val openName = "OPEN_Baseline.dcm"

  val transName = "TRANS_Baseline.dcm"

  private def curDir = new File(System.getProperty("user.dir"))

  private case class DicomFile(file: File, attributeList: AttributeList) {
    /**  Get the ExposureSequence --> ExposureTime.  */
    def getExposureTime: Option[Int] = {
      try {
        val exposureSequence = attributeList.get(TagByName.ExposureSequence).asInstanceOf[SequenceAttribute]
        val childAl = exposureSequence.getItem(0).getAttributeList
        Some(childAl.get(TagByName.ExposureTime).getIntegerValues()(0))
      } catch {
        case t: Throwable => { None }
      }
    }
  }

  private def fileToDicomFile(file: File): Option[DicomFile] = {
    if (file.canRead && DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
      try {
        val al = new AttributeList
        al.read(file)
        Some(new DicomFile(file, al))
      } catch {
        case t: Throwable => None
      }
    } else None
  }

  private def readDicomFiles: Seq[DicomFile] = curDir.getParentFile.listFiles.toSeq.map(f => fileToDicomFile(f)).flatten

  private def copyConfigFile(name: String, dicomFile: DicomFile) = {
    val dir = new File(System.getenv(Util.machineConfigDirEnvName))
    val outFile = new File(dir, name)
    val binaryData = Util.readBinaryFile(dicomFile.file).right.get
    println("writing DICOM file " + outFile.getAbsolutePath)
    Util.writeBinaryFile(outFile, binaryData)
    println("wrote DICOM file " + outFile.getAbsolutePath)
    val localOutFile = new File(outFile.getName)
    Util.writeBinaryFile(localOutFile, binaryData)
    println("wrote DICOM file " + localOutFile.getAbsolutePath)
  }

  private def copyFiles(open: DicomFile, trans: DicomFile): Unit = {
    println("found both qualifying DICOM files")
    copyConfigFile(openName, open)
    copyConfigFile(transName, trans)
  }

  def writeHtml = {
    val html = {
      <div class="row">
        <div class="col-md-5 col-md-offset-2">
          <h3>Success</h3>
          Both the open field and transmission files were written to the configuration directory for
                machine{ System.getenv(Util.machineIdEnvName) + " at " + System.getenv(Util.institutionIdEnvName) }
        </div>
      </div>
    }
    val file = new File(Output.displayFilePrefix + ".html")
    val text = WebUtil.wrapBody(html, "Uploaded Open and Transmission")
    Util.writeFile(file, text)
  }

  /**
   * Upload calibration (baseline) files for running the DLG Averages over 2.5mm x 60mm layers for
   * a total area of interest of 60mm x 200mm
   *
   */
  def main(args: Array[String]): Unit = {
    try {
      println("Starting UploadTransAndOpen")
      val dicomList = readDicomFiles
      dicomList.map(df => println("Found DICOM file " + df.file.getName))
      val exposureTimeList = dicomList.filter(df => df.getExposureTime.isDefined)
      exposureTimeList.map(df => println("Found DICOM file with exposure time: " + df.file.getName))
      println("Number of DICOM files found that have exposure time defined: " + exposureTimeList.size)
      if (exposureTimeList.size != 2) ProcedureStatus.terminate("Abort: Should be exactly 2 DICOM files but there were " + exposureTimeList.size, ProcedureStatus.abort)

      if (exposureTimeList(0).getExposureTime.get < exposureTimeList(1).getExposureTime.get)
        copyFiles(dicomList(0), dicomList(1))
      else
        copyFiles(dicomList(1), dicomList(0))

      writeHtml

      ProcedureStatus.terminate("Done", ProcedureStatus.done)
    } catch {
      case t: Throwable => ProcedureStatus.terminate("Unexpected error: " + fmtEx(t), ProcedureStatus.abort)
    }
  }

}
