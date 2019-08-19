
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
import org.aqa.VolumeTranslator
import org.aqa.webrun.bbByEpid.BBbyEPIDAnalysis
import org.aqa.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ImageUtil
import java.awt.Rectangle
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

/**
 * Test the Config.
 *
 */

class TestBBbyEPIDAnalysis extends FlatSpec with Matchers {

  println("-----------------------------------------------------------------------------------------------------")

  if (false) {
    val inDirList = (new File("""D:\tmp\aqa\CBCT""")).listFiles.filter(f => f.getName.startsWith("MQATX"))

    val fileList = inDirList.map(d => d.listFiles).flatten.filter(f => f.getName.startsWith("RI."))

    def show(al: AttributeList): String = {
      def fmt(d: Double) = d.formatted("%7.1f")
      val StationName = al.get(TagFromName.StationName).getSingleStringValueOrEmptyString
      val DeviceSerialNumber = al.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrEmptyString.formatted("%16s")
      val PatientID = al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString.formatted("%20s")
      val GantryAngle = {
        val ga = (al.get(TagFromName.GantryAngle).getDoubleValues.head).round.toInt
        (ga % 360).formatted("%4d")
      }
      val IsocenterPosition = al.get(TagFromName.IsocenterPosition).getDoubleValues.map(d => fmt(d)).mkString(", ")
      "DevSerNo: " + DeviceSerialNumber +
        "    Station: " + StationName +
        "    PatientID: " + PatientID +
        "    GantryAngle: " + GantryAngle +
        "    Isocenter: " + IsocenterPosition
    }

    val stuff = fileList.map(f => new DicomFile(f)).map(df => show(df.attributeList.get)).distinct.sorted
    println(stuff.mkString("\n"))
  }

  println("-----------------------------------------------------------------------------------------------------")

  Config.validate

  println("-----------------------------------------------------------------------------------------------------")
  val outDir = new File("""target\TestBBbyEPIDAnalysis""")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  def writeImages(name: String, bufImgList: Seq[BufferedImage]) = {
    val dir = new File(outDir, name)
    dir.mkdirs
    println("Writing EPID png files to " + dir.getAbsolutePath)
    Util.writePng(bufImgList(0), new File(dir, "x-axis-view.png"))
    Util.writePng(bufImgList(1), new File(dir, "y-axis-view.png"))
    Util.writePng(bufImgList(2), new File(dir, "z-axis-view.png"))
  }

  // list of directories that contain a EPID sets for analysis
  val fileList = (new File("""src\test\resources\TestBBbyEPIDAnalysis""")).listFiles
  println("List of EPID files used as input:\n    " + fileList.map(file => file.getAbsolutePath).mkString("\n    "))

  "TestBBbyEPIDAnalysis" should "find BB" in {

    def testDir(file: File) = {
      println("Processing file " + file.getAbsolutePath)
      val al = (new DicomFile(file)).attributeList.get
      val iso = BBbyEPIDAnalysis.testFindBB(al)
      println("result: " + iso)
      val di = new DicomImage(al)
      val trans = new IsoImagePlaneTranslator(al)
      val pix = trans.iso2Pix(iso)
      val bufImg = di.toDeepColorBufferedImage(0.001)
      val graphics = ImageUtil.getGraphics(bufImg)
      graphics.setColor(Color.white)
      def rnd(d: Double) = d.round.toInt
      val radius = 4
      val rect = new Rectangle(rnd(pix.getX - radius), rnd(pix.getY - radius), radius * 2, radius * 2)
      graphics.drawOval(rnd(pix.getX - radius), rnd(pix.getY - radius), radius * 2, radius * 2)
      val pngFile = new File(outDir, (file.getName.replace(".dcm", ".png")))
      ImageUtil.writePngFile(bufImg, pngFile)
      println("Wrote image to " + pngFile.getAbsolutePath)
      (true) should be(true)
    }

    fileList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}
