package learn.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import org.aqa.Util
import us.hebi.matlab.mat.format.Mat5

import java.awt.Rectangle
import java.io.File

object PSM {

  val outDir = new File("target\\psm")

  private def middleOf(image: DicomImage): String = {

    val size = 10
    val x = (image.width - size) / 2
    val y = (image.height - size) / 2

    val rect = new Rectangle(x, y, size, size)

    val sub = image.getSubimage(rect)

    sub.pixelsToText
  }

  private def psmToDicomImage(psmFile: File): DicomImage = {
    val entry = Mat5.readFromFile(psmFile).getEntries.iterator().next()
    val dimensions = entry.getValue.getDimensions
    val width = dimensions(0)
    val height = dimensions(1)
    val matrix = Mat5.readFromFile(psmFile).getMatrix("PSM")

    def toRow(y: Int): IndexedSeq[Float] = {
      val aa = (0 until width).map(x => {
        matrix.getDouble(y, x).toFloat
      })
      aa
    }

    val pixelData = (0 until height).map(toRow)

    new DicomImage(pixelData)
  }

  private def savePng(dicomImage: DicomImage, name: String, pct: Double = 0.001): Unit = {
    val list = dicomImage.pixelData.flatten.groupBy(identity).toList.map(g => DicomImage.HistPoint(g._1, g._2.size)).sortWith((a, b) => a.value < b.value)


    val minMax = {
       val sorted = dicomImage.pixelData.flatten.sorted
      (sorted.drop(5).head, sorted.dropRight(5).last)
    }

    val buf = dicomImage.toDeepColorBufferedImage(minMax._1, minMax._2)
    val pngFile = new File(outDir, name + ".png")
    Util.writePng(buf, pngFile)
    println("Wrote file " + pngFile.getAbsolutePath)
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting ...")
    val dir = new File("""src\test\resources\learnPSM""")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs

    val wd0dicom = new DicomFile(new File(dir, "RI.zzz_EpidPerformanceCheck.WD-0.dcm"))
    val testDicom = new DicomFile(new File(dir, "6x_FF.dcm"))
    val psmFile = new File(dir, "PSM_6x_CMN_230520.mat")

    Trace.trace(s"wd0dicom:\n${DicomUtil.attributeListToString(wd0dicom.attributeList.get)}\n")
    println("\n---------------------\n")

    Trace.trace(s"testDicom:\n${DicomUtil.attributeListToString(testDicom.attributeList.get)}\n")

    val wd0Image = new DicomImage(wd0dicom.attributeList.get)
    val testImage = new DicomImage(testDicom.attributeList.get)
    val psmImage = psmToDicomImage(psmFile)

    val wd0Xtest = {
      val pixelData = testImage.pixelData.zip(wd0Image.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
      new DicomImage(pixelData)
    }

    val psmCorrected = {
      val pixelData = wd0Xtest.pixelData.zip(psmImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 / pair._2))
      new DicomImage(pixelData)
    }

    savePng(wd0Image, "wd0Image")
    savePng(testImage, "testImage")
    savePng(wd0Xtest, "wd0Xtest")
    savePng(psmImage, "psmImage")
    savePng(psmCorrected, "psmCorrected", 0)

    Trace.trace("\nwd0:\n     " + middleOf(wd0Image))
    Trace.trace("\ntest:\n     " + middleOf(testImage))
    Trace.trace("\nwd0 x test:\n     " + middleOf(wd0Xtest))
    Trace.trace("\npsm:\n     " + middleOf(psmImage))
    Trace.trace("\npsmCorrected:\n     " + middleOf(psmCorrected))

  }

}
