
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
import org.aqa.webrun.bbByEpid.BBbyEPIDImageAnalysis
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ImageUtil
import java.awt.Rectangle
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.geom.Point2D
import org.aqa.webrun.bbByEpid.BBbyEPIDAnnotateImages

/**
 * Test the Config.
 *
 */

class TestBBbyEPIDImageAnalysis extends FlatSpec with Matchers {

  println("-----------------------------------------------------------------------------------------------------")

  private def d2i(d: Double) = d.round.toInt

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

  (0 to 10).map(_ => println)
  println("-----------------------------------------------------------------------------------------------------")
  val outDir = new File("""target\TestBBbyEPIDImageAnalysis""")
  outDir.mkdirs
  outDir.listFiles.map(f => Utility.deleteFileTree(f))

  def writeImages(name: String, bufImgList: Seq[BufferedImage]) = {
    val dir = new File(outDir, name)
    dir.mkdirs
    println("Writing EPID png files to " + dir.getAbsolutePath)
    Util.writePng(bufImgList(0), new File(dir, "x-axis-view.png"))
    Util.writePng(bufImgList(1), new File(dir, "y-axis-view.png"))
    Util.writePng(bufImgList(2), new File(dir, "z-axis-view.png"))
  }

  private def showPixValues(center_pix: Point2D.Double, trans: IsoImagePlaneTranslator, image: DicomImage) = {
    val range = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm * 2).ceil.toInt

    def get(x: Int, y: Int) = image.get(d2i(x + center_pix.getX), d2i(y + center_pix.getY)).round.toInt
    val pixVals = for (y <- (-range to range); x <- (-range to range)) yield { get(x, y) }
    val minPix = pixVals.min

    for (y <- (-range to range)) {
      for (x <- (-range to range)) {
        print((get(x, y) - minPix).formatted("%5d"))
      }
      println
    }
  }

  // list of directories that contain a EPID sets for analysis
  val fileList = (new File("""src\test\resources\TestBBbyEPIDImageAnalysis""")).listFiles
  println("List of EPID files used as input:\n    " + fileList.map(file => file.getAbsolutePath).mkString("\n    "))

  "TestBBbyEPIDImageAnalysis" should "find BB" in {

    def testDir(file: File) = {
      println("Processing file " + file.getAbsolutePath)
      val al = (new DicomFile(file)).attributeList.get
      val iso = BBbyEPIDImageAnalysis.findBB(al)
      println("result: " + iso)
      val di = new DicomImage(al)
      val trans = new IsoImagePlaneTranslator(al)
      val isoAs2D = new Point2D.Double(iso.right.get.pix.getX, iso.right.get.pix.getY)
      val pix = trans.iso2Pix(isoAs2D)

      showPixValues(pix, trans, di)

      val annotatedImages = new BBbyEPIDAnnotateImages(al, Some(isoAs2D))

      val pngFileFull = new File(outDir, (file.getName.replace(".dcm", "_full.png")))
      ImageUtil.writePngFile(annotatedImages.fullBufImg, pngFileFull)
      println("Wrote image to " + pngFileFull.getAbsolutePath)

      val pngFileCloseup = new File(outDir, (file.getName.replace(".dcm", "_closeup.png")))
      ImageUtil.writePngFile(annotatedImages.closeupBufImg, pngFileCloseup)
      println("Wrote image to " + pngFileCloseup.getAbsolutePath)

      if (false) {
        //val bufImg = di.toDeepColorBufferedImage(0.001)
        val bufImg = di.toBufferedImage(Color.yellow)
        val graphics = ImageUtil.getGraphics(bufImg)
        graphics.setColor(Color.white)
        def rnd(d: Double) = d.round.toInt
        val radius = 6
        graphics.drawOval(rnd(pix.getX - radius), rnd(pix.getY - radius), radius * 2, radius * 2)
        val pngFile = new File(outDir, (file.getName.replace(".dcm", ".png")))
        ImageUtil.writePngFile(bufImg, pngFile)
        println("Wrote image to " + pngFile.getAbsolutePath)
      }

      if (false) {
        val scale = 20
        def d2i(d: Double) = d.round.toInt
        val sz = 16
        val rect = new Rectangle(d2i(pix.getX - (sz / 2)), d2i(pix.getY - (sz / 2)), sz, sz)
        val subImage = di.getSubimage(rect)
        val bufImg = ImageUtil.magnify(subImage.toBufferedImage(Color.white), scale)
        val graphics = ImageUtil.getGraphics(bufImg)
        graphics.setColor(Color.red)
        val radius = 6 * scale
        val pixScaled = new Point2D.Double(pix.getX * scale, pix.getX * scale)
        graphics.drawOval(d2i((pixScaled.getX * scale) - (radius + rect.getX)), d2i(pixScaled.getY - (radius + rect.getY)), radius * 2, radius * 2)
        //graphics.drawOval(d2i(pixScaled.getX - radius), d2i(pixScaled.getY - radius), radius * 2, radius * 2)
        val pngFile = new File(outDir, (file.getName.replace(".dcm", "w.png")))
        ImageUtil.writePngFile(bufImg, pngFile)
        println("Wrote image to " + pngFile.getAbsolutePath)
      }

      (true) should be(true)
    }

    fileList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}
