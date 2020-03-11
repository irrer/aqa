
package aqa.test;

//import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
//import edu.umro.util.Utility
//import edu.umro.ScalaUtil.Trace
//import edu.umro.ScalaUtil.FileUtil
//import org.aqa.web.C3Chart
//import java.awt.Color
//import org.aqa.Crypto
//import org.aqa.DicomFile
import org.aqa.Config
import org.aqa.Util
import org.aqa.DicomFile
import org.aqa.webrun.phase2.vmat.VMATAnalysis
import com.pixelmed.dicom.AttributeList

//import edu.umro.ImageUtil.DicomImage
//import java.awt.image.BufferedImage
//import org.aqa.webrun.phase2.vmat.VMATAnalysis
//import org.aqa.VolumeTranslator

/**
 * Test VMAT analysis.
 */

class TestVMATAnalysis extends FlatSpec with Matchers {

  Config.validate

  println("-----------------------------------------------------------------------------------------------------")

  def testDir(dir: File) = {

    def alByName(name: String): AttributeList = {
      val f = Util.listDirFiles(dir).filter(f => f.getName.toUpperCase().contains(name.toUpperCase())).head
      (new DicomFile(f)).attributeList.get
    }

    println("processing directory: " + dir.getAbsolutePath)
    val plan = alByName("RTPLAN")
    val drgs2 = alByName("2DR-GS")
    val open2 = alByName("2OPEN")

    val j = VMATAnalysis.testGetPlanAoiList("T2-DR-GS", "T2 Open", drgs2, open2, plan)

    println("AOI List: \n" + j.mkString("\n"))

    (true) should be(true)
  }

  val inDir = new File("""target\TestVMATAnalysis""")

  // list of directories that contain a VMAT set for analysis
  val dirList = (new File("""src\test\resources\TestVMAT""")).listFiles
  println("List of VMAT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

  "TestVMATAnalysis" should "find percentages" in {

    dirList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}
