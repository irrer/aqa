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
      """D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000\RI.$JM_AQA_phase2_v000.MV_270_0a1.dcm""")

    val fileList = fileNames.map(name => new File(name))

    val results = CollimatorCenteringAnalysis.fineMeasure((new DicomFile(fileList.head)).attributeList.get)

    val meas = results._1
    val bufImg = results._2

    println("meas: " + meas)
    val pngFile = new File("""target\\90_0a.png""")
    ImageIO.write(bufImg, "png", pngFile)
  }
}