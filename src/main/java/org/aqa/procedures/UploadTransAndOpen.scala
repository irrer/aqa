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

object UploadTransAndOpen {

    private val openName = "OPEN_Baseline.dcm"

    private val transName = "TRANS_Baseline.dcm"

    def curDir = new File(System.getProperty("user.dir"))

    def fileToAttributeList(file: File): Option[AttributeList] = {
        try {
            val al = new AttributeList
            al.read(file)
            Some(al)
        }
        catch {
            case t: Throwable => None
        }
    }

    def readDicomFiles: Seq[AttributeList] = {
        curDir.getParentFile.listFiles.toSeq.map(f => fileToAttributeList(f)).flatten
    }

    /**
     * Get the ExposureSequence --> ExposureTime or die trying.
     */
    def getExposureTime(al: AttributeList): Int = {
        try {
            val exposureSequence = al.get(TagFromName.ExposureSequence).asInstanceOf[SequenceAttribute]
            val childAl = exposureSequence.getItem(0).getAttributeList
            childAl.get(TagFromName.ExposureTime).getIntegerValues()(0)
        }
        catch {
            case t: Throwable => {
                ProcedureStatus.terminate("Unable to get : (0018,1150) ExposureTime", ProcedureStatus.abort)
                -1
            }
        }
    }

    private def writeConfigFile(name: String, al: AttributeList) = {
        val dir = new File(System.getenv(???))
        val file = new File(dir, name)
        val transferSyntax = al.get(TagFromName.TransferSyntaxUID).getSingleStringValueOrDefault(TransferSyntax.ExplicitVRLittleEndian);
        al.write(file, transferSyntax, true, true);
        println("wrote DICOM file " + file.getAbsolutePath)
    }

    private def copyFiles(open: AttributeList, trans: AttributeList): Unit = {
        println("found both qualifying DICOM files")
        writeConfigFile(openName, open)
        writeConfigFile(transName, trans)
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
            if (dicomList.size != 2) ProcedureStatus.terminate("Abort: Should be exactly 2 DICOM files but there were " + dicomList.size, ProcedureStatus.abort)
            val exposureTimeList = dicomList.map(al => getExposureTime(al))

            if (exposureTimeList(0) < exposureTimeList(1))
                copyFiles(dicomList(0), dicomList(1))
            else
                copyFiles(dicomList(1), dicomList(0))

            ProcedureStatus.terminate("Done", ProcedureStatus.done)
        }
        catch {
            case t: Throwable => ProcedureStatus.terminate("Unexpected error: " + Logging.fmtEx(t), ProcedureStatus.abort)
        }
    }

}
