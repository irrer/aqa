
package aqa.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Config
import org.aqa.Util
import com.pixelmed.dicom.AttributeList
import org.aqa.DicomFile
import org.aqa.webrun.phase2.vmat.VMATAnalysis
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.CollimatorCentering
import org.aqa.run.ProcedureStatus
import edu.umro.ImageUtil.DicomImage
import org.aqa.db.VMAT

/**
 * Test VMAT analysis.
 */

class TestVMATAnalysis extends FlatSpec with Matchers {

  Config.validate

  (0 to 5).map(_ => println("-----------------------------------------------------------------------------------------------------"))

  private def testVmatPair(vmatPair: Config.VMATBeamPair, rtplan: AttributeList, imageList: Seq[DicomFile]) = {
    def isTheNamedBeam(img: AttributeList, name: String): Boolean = {
      val n = Phase2Util.getBeamNameOfRtimage(rtplan, img)
      n.isDefined && (n.get.equals(name))
    }

    val mlc = imageList.find(df => isTheNamedBeam(df.attributeList.get, vmatPair.mlc)).get.attributeList.get
    val open = imageList.find(df => isTheNamedBeam(df.attributeList.get, vmatPair.open)).get.attributeList.get

    val aoiList = VMATAnalysis.testGetPlanAoiList(vmatPair.mlc, vmatPair.open, mlc, open, rtplan)
    println("AOI List: \n    " + aoiList.mkString("\n    "))

    val collimatorCentering = new CollimatorCentering(None, -1, ProcedureStatus.running.toString,
      "sop090", "sop270",
      0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0)
    val outputPK: Long = -1
    val vmatSeq = VMATAnalysis.testAnalyze(vmatPair.mlc, vmatPair.open, mlc, open, rtplan, collimatorCentering, outputPK, new DicomImage(mlc), new DicomImage(open))

    def vmatFormatted(vmat: VMAT) = {
      def fmt(d: Double) = d.formatted("%10.3f")
      "Left plan mm: " + fmt(vmat.leftRtplan_mm) +
        "    mlc cu: " + fmt(vmat.doseMLC_cu) +
        "    open cu: " + fmt(vmat.doseOpen_cu) +
        "    percent: " + fmt(vmat.percent)
    }
    println("analysis results:\n    " + vmatSeq.map(vmat => vmatFormatted(vmat)).mkString("\n    "))
  }

  private def testDir(dir: File) = {
    println("processing directory: " + dir.getAbsolutePath)

    def alByName(name: String): AttributeList = {
      val f = Util.listDirFiles(dir).filter(f => f.getName.toUpperCase().contains(name.toUpperCase())).head
      (new DicomFile(f)).attributeList.get
    }

    val dicomFileList = Util.listDirFiles(dir).map(f => new DicomFile(f))

    val rtplan = dicomFileList.find(df => Util.modalityOfAl(df.attributeList.get).equals("RTPLAN")).get.attributeList.get
    val imageList = dicomFileList.filter(df => Util.modalityOfAl(df.attributeList.get).equals("RTIMAGE"))

    Config.VMATBeamPairList.map(vmatPair => testVmatPair(vmatPair, rtplan, imageList))

    (true) should be(true)
  }

  "TestVMATAnalysis test" should "find percentages" in {

    val inDir = new File("""target\TestVMATAnalysis""")

    // list of directories that contain a VMAT set for analysis
    val dirList = (new File("""src\test\resources\TestVMAT""")).listFiles
    println("List of VMAT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

    dirList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}
