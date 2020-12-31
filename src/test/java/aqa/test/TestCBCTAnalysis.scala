
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
import org.aqa.DicomFile
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.image.BufferedImage
import org.aqa.webrun.bbByCBCT.BBbyCBCTAnalysis
import org.aqa.VolumeTranslator

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
  val dirList = (new File("""src\test\resources\TestCBCTAlign""")).listFiles.sortBy(_.getName)
  println("List of CBCT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

  "TestCBCTAnalysis" should "find BB" in {

    def testDir(dir: File) = {
      println("Processing directory " + dir.getAbsolutePath)
      val attrListSeq = dir.listFiles.map(f => (new DicomFile(f)).attributeList.get).toSeq
      println("Number of slices in series: " + attrListSeq.size)
      val result = BBbyCBCTAnalysis.volumeAnalysis(attrListSeq, new File(outDir, dir.getName))

      val point = result.right.get.cbctFrameOfRefLocation_mm
      val bufImgList = result.right.get.imageXYZ
      writeImages(dir.getName, bufImgList)
      def fmt(d: Double) = d.formatted("%12.7f")
      println("BB    coordinates of " + fmt(point.getX) + " " + fmt(point.getY) + " " + fmt(point.getZ) + "  " + dir.getName)
      val voxCoords = (new VolumeTranslator(attrListSeq)).mm2vox(point)
      if (true) { // TODO rm
        println("Voxel coordinates of " + fmt(voxCoords.getX) + " " + fmt(voxCoords.getY) + " " + fmt(voxCoords.getZ) + "  " + dir.getName)
      }
      (result.isRight) should be(true) // Expected voxel coordinates: 246, 262, 42
    }

    Trace.trace
    dirList.map(dir => testDir(dir))
    Trace.trace

    println("Wrote files to " + outDir.getAbsolutePath)
    (11 > 10) should be(true)
  }

}

object TestCBCTAnalysis {
  def main(args: Array[String]): Unit = {
    new TestCBCTAnalysis
  }
}
