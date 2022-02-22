package org.aqa.webrun.gapSkew

import edu.umro.ImageUtil.DicomImage
import org.aqa.DicomFile
import org.aqa.Util

import java.awt.Rectangle
import java.io.File

object Band {
  def main(args: Array[String]): Unit = {
    val file = new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_002_2020-03-23T19-13-17.000.dcm""")
    val df = new DicomFile(file)
    val al = df.attributeList.get
    val di = new DicomImage(al)

    val top = 360
    val height = 5
    val rect = new Rectangle(0, top, di.width, height)
    val sub = di.getSubimage(rect)
    val sums = {
      val s = sub.columnSums.map(_ / height)
      val min = s.min
      s.map(_ - min)
    }

    println(sums.mkString("\n", "\n", "\n"))

    val img = sub.toDeepColorBufferedImage(0.01)
    val unique = (System.currentTimeMillis() % 1000).formatted("%3d")
    val imgFile = new File(file.getParentFile, file.getName.dropRight(4) + unique + ".png")
    Util.writePng(img, imgFile)
    println("Wrote " + imgFile.getAbsolutePath)
  }

}
