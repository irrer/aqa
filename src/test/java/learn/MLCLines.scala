package learn

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator

import java.awt.Color
import java.io.File

object MLCLines {
  def main(args: Array[String]): Unit = {
    println("starting")

    val file = new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_002_2020-03-23T19-13-17.000.dcm""")

    val al = new AttributeList
    al.read(file)

    val di = new DicomImage(al)
    val img = di.toBufferedImage(Color.WHITE)

    val gc = ImageUtil.getGraphics(img)
    gc.setColor(Color.green)

    val transform = new IsoImagePlaneTranslator(al)

    if (true) {
      val y = transform.iso2PixCoordY(0.6).round.toInt
      gc.drawLine(0, y, di.width-1, y)
    }

    if (true) {
      val y = transform.iso2PixCoordY(100).round.toInt
      gc.drawLine(0, y, di.width-1, y)
    }

    val pngFile = new File("""D:\tmp\aqa\GapSkew\MLCLines.png""")

    val png = ImageUtil.writePngFile   (img, pngFile)
    println("wrote file: " + pngFile.getAbsolutePath)

    println("finished")
  }

}
