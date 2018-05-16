package aqa.test

import java.io.File
import org.aqa.webrun.phase2.CollimatorCenteringAnalysis
import org.aqa.DicomFile
import edu.umro.ImageUtil.ImageUtil
import javax.imageio.ImageIO

object TestCollimatorCenteringAnalysis {

  def main(args: Array[String]): Unit = {

    val fileNames = Seq(
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_90_0a.dcm""",
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_270_0a1.dcm""",
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_0_0a5.dcm""")

    val fileList = fileNames.map(name => new File(name))

    def processFile(file: File) = {
      val results = CollimatorCenteringAnalysis.testHook(new DicomFile(file))

      val bufImg = results.bufferedImage
      val meas = results.measurementSet

      println("meas: " + meas)
      val pngFileName = file.getName.replaceAll(".dcm$", ".png").replaceAll(".DCM$", ".png")
      val pngFile = new File(new File("target"), pngFileName)
      pngFile.delete
      ImageIO.write(bufImg, "png", pngFile)
      println("created file: " + pngFile)
    }

    fileList.map(file => processFile(file))

  }
}