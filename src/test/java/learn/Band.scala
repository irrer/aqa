package learn

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.DicomFile
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.io.File

/**
  * View image characteristics.  Review coordinate system.
  */
object Band {
  def main(args: Array[String]): Unit = {
    val file = new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_002_2020-03-23T19-13-17.000.dcm""")
    val df = new DicomFile(file)
    val al = df.attributeList.get
    val di = new DicomImage(al)

    val top = 0
    val height = di.height
    val rect = new Rectangle(0, top, di.width, height)
    val sub = di.getSubimage(rect)
    val sums = {
      val s = sub.columnSums.map(_ / height)
      val min = s.min
      s.map(_ - min)
    }

    println(sums.mkString("\n", "\n", "\n"))

    val img = sub.toDeepColorBufferedImage(0.01)
    if (true) {
      val trans = new IsoImagePlaneTranslator(al)
      val g = ImageUtil.getGraphics(img)
      val dim = 10

      def draw(pos: Int): Unit = {
        g.setColor(Color.white)
        val p1 = trans.iso2Pix(pos, pos)
        println(p1)
        g.drawRect(Util.d2i(p1.getX), Util.d2i(p1.getY), dim, dim)
        g.drawString(pos.toString, Util.d2i(p1.getX), Util.d2i(p1.getY))
      }

      draw(-100)
      draw(-50)
      draw(0)
      draw(50)
      draw(100)
    }
    val unique = (System.currentTimeMillis() % 1000).formatted("%3d")
    val imgFile = new File(file.getParentFile, file.getName.dropRight(4) + unique + ".png")
    Util.writePng(img, imgFile)
    println("Wrote " + imgFile.getAbsolutePath)
  }

}
