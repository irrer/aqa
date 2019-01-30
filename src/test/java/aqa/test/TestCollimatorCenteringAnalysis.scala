package aqa.test

import java.io.File
import org.aqa.DicomFile
import edu.umro.ImageUtil.ImageUtil
import javax.imageio.ImageIO
import org.aqa.webrun.phase2.MeasureTBLREdges
import java.awt.Point
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator

object TestCollimatorCenteringAnalysis {

  def main(args: Array[String]): Unit = {

    val fileNames = Seq(
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_90_0a.dcm""",
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_270_0a1.dcm""",
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_0_0a5.dcm""")

    val fileList = fileNames.map(name => new File(name))

    def processFile(file: File) = {
      val dicomFile = new DicomFile(file)
      val image = new DicomImage(dicomFile.attributeList.get)
      val translator = new IsoImagePlaneTranslator(dicomFile.attributeList.get)
      val results = MeasureTBLREdges.measure(image, translator, 270, image, new Point(0, 0))

      val bufImg = results.bufferedImage
      val meas = results.measurementSet

      println("meas: " + meas)
      val pngFileName = file.getName.replaceAll(".dcm$", ".png").replaceAll(".DCM$", ".png")
      val pngFile = new File(new File("target"), pngFileName)
      pngFile.delete
      ImageIO.write(bufImg, "png", pngFile)
      println("created file: " + pngFile.getAbsolutePath)
    }

    fileList.map(file => processFile(file))

  }
}