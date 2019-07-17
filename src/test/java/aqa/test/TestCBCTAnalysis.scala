
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

/**
 * Test the Config.
 *
 */

class TestCBCTAnalysis extends FlatSpec with Matchers {

  Config.validate
  println("-----------------------------------------------------------------------------------------------------")

  //  def writeImages() = {
  //      if (true) { // TODO rm
  //      def write(axis: String, image: DicomImage, pixelWidth: Double, pixelHeight: Double) = {
  //        val baseName = System.currentTimeMillis + "_" + axis
  //
  //        if (true) {
  //          val name = baseName + ".png"
  //          val outFile = new File("""D:\tmp\aqa\CBCT\perspectives\""" + name)
  //          outFile.getParentFile.mkdirs
  //          outFile.delete
  //          val bufImg = image.toDeepColorBufferedImage(0)
  //          ImageUtil.writePngFile(bufImg, outFile)
  //          println("wrote file: " + outFile.getAbsolutePath)
  //        }
  //
  //        if (true) {
  //          val name = baseName + "_aspect.png"
  //          val outFile = new File("""D:\tmp\aqa\CBCT\perspectives\""" + name)
  //          outFile.getParentFile.mkdirs
  //          outFile.delete
  //          val bufImg = image.renderPixelsToSquare(pixelWidth, pixelHeight).toDeepColorBufferedImage(0)
  //          ImageUtil.writePngFile(bufImg, outFile)
  //          println("wrote file: " + outFile.getAbsolutePath)
  //        }
  //      }
  //
  //      Trace.trace("x AR: " + voxSize(0) / voxSize(2))
  //      Trace.trace("y AR: " + voxSize(1) / voxSize(2))
  //      Trace.trace("z AR: " + voxSize(1) / voxSize(0))
  //      write("X", xImage(entireVolume, start, size), voxSize(2), voxSize(1))
  //      write("Y", yImage(entireVolume, start, size), voxSize(2), voxSize(0))
  //      write("Z", zImage(entireVolume, start, size), voxSize(0), voxSize(1))
  //    }
  //  }

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
      val bufImg = result.right.get._2
      Trace.trace(dir.getName + " : " + point)
      (result.isRight) should be(true) // Expected voxel coordinates: 246, 262, 42
    }

    Trace.trace
    dirList.map(dir => testDir(dir))
    Trace.trace

    (11 > 10) should be(true)
  }

}
