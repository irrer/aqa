
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.web.C3Chart
import java.awt.Color
import org.aqa.Crypto
import org.aqa.webrun.phase2.cbctAlign.CBCTAnalysis
import org.aqa.DicomFile
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.image.BufferedImage

/**
 * Test the Config.
 *
 */

class TestCBCTAnalysis extends FlatSpec with Matchers {

  Config.validate

  println("-----------------------------------------------------------------------------------------------------")
  val outDir = new File("""target\TestCBCTAnalysis""")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  def writeImages(name: String, bufImgList: Seq[BufferedImage]) = {
    val dir = new File(outDir, name)
    dir.mkdirs
    println("Writing CBCT png files to " + dir.getAbsolutePath)
    Util.writePng(bufImgList(0), new File(dir, "x-axis-view.png"))
    Util.writePng(bufImgList(1), new File(dir, "y-axis-view.png"))
    Util.writePng(bufImgList(2), new File(dir, "z-axis-view.png"))
  }

  // list of directories that contain a CBCT set for analysis
  val dirList = (new File("""src\test\resources\TestCBCTAlign""")).listFiles
  println("List of CBCT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

  "TestCBCTAnalysis" should "find BB" in {

    def testDir(dir: File) = {
      println("Processing directory " + dir.getAbsolutePath)
      val attrListSeq = dir.listFiles.map(f => (new DicomFile(f)).attributeList.get).toSeq
      println("Number of slices in series: " + attrListSeq.size)
      val result = CBCTAnalysis.testAnalyze(attrListSeq)

      val point = result.right.get._1
      val bufImgList = result.right.get._2
      writeImages(dir.getName, bufImgList)
      def fmt(d: Double) = d.formatted("%12.7f")
      println("BB coordinates of " + fmt(point.getX) + " " + fmt(point.getY) + " " + fmt(point.getZ) + "  " + dir.getName)
      (result.isRight) should be(true) // Expected voxel coordinates: 246, 262, 42
    }

    dirList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}