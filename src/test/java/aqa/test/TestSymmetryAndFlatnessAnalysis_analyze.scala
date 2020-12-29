package aqa.test

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.db.DbSetup
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import java.sql.Timestamp
import java.util.Date


/**
 * Test the analysis portion of symmetry, flatness, and constancy.  Does not write results to the database.
 */
class TestSymmetryAndFlatnessAnalysis_analyze extends FlatSpec with Matchers {

  "SymAndFlat_analysis" should "calc point values" in {
    TestSymmetryAndFlatnessAnalysis_analyze.doTest()
    true should be(true)
  }

}

object TestSymmetryAndFlatnessAnalysis_analyze {

  private def readDicom(file: File) = {
    val al = new AttributeList
    al.read(file)
    al
  }

  /**
   * Get a working copy of run requirements from the test directory.  We are assuming that the test
   * data does not need to be carefully checked.
   *
   * @param inputDir Read DICOM files from here.
   * @return A new RunReq
   */
  private def getRtimageMap(inputDir: File): Map[String, AttributeList] = {
    val alL = Util.listDirFiles(inputDir).map(f => readDicom(f)).toList
    val rtimageList = alL.filter(al => Util.isRtimage(al))
    val rtplan = alL.filter(al => Util.isRtplan(al)).head
    val rtimageMap = rtimageList.map(al => (Phase2Util.getBeamNameOfRtimage(rtplan, al).get, al)).toMap
    rtimageMap
  }

  private def doTest(): Unit = {
    Config.validate
    DbSetup.init
    Trace.trace("-------------------------------")

    // resources
    val inputDir = new File("src\\test\\resources\\TestSymmetryAndFlatnessAnalysis")
    println("Using input directory: " + inputDir.getAbsolutePath)

    val rtimageMap = getRtimageMap(inputDir)

    val collimatorCentering = new CollimatorCentering(None,
      outputPK = 0,
      status = "PASS",
      SOPInstanceUID090 = "12345",
      SOPInstanceUID270 = "23456",
      xCollimatorCenter_mm = 0,
      yCollimatorCenter_mm = 0,

      X1_090_mm = 0,
      X2_090_mm = 0,
      Y1_090_mm = 0,
      Y2_090_mm = 0,

      X1_270_mm = 0,
      X2_270_mm = 0,
      Y1_270_mm = 0,
      Y2_270_mm = 0
    )

    val beamName = "J20G0-6X"

    val al = rtimageMap("J20G0-6X")


    val dataDate: Date = Seq(
      DicomUtil.getTimeAndDate(al, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      DicomUtil.getTimeAndDate(al, TagFromName.ContentDate, TagFromName.ContentTime)).flatten.head
    println("Data Date: " + dataDate)

    val result = SymmetryAndFlatnessAnalysis.testAnalyze(beamName, machinePK = 22, new Timestamp(dataDate.getTime), al, new DicomImage(al), collimatorCentering)
    Trace.trace(result.result)
    //println(result.maintenanceRecordBaseline)
    Trace.trace(result.pointSet)
    Trace.trace
  }

  def main(args: Array[String]): Unit = {
    Trace.trace
    doTest()
    Trace.trace
  }
}
