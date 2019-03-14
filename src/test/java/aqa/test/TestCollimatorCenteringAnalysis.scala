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
import org.aqa.Util

object TestCollimatorCenteringAnalysis {

  def main(args: Array[String]): Unit = {

    val inDir = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources""")
    val fileNames = Seq("TestCollimatorCenteringAnalysis_1.dcm", "TestCollimatorCenteringAnalysis_2.dcm", "TestCollimatorCenteringAnalysis_3.dcm")

    val fileList = fileNames.map(name => new File(inDir, name))

    def processFile(file: File) = {
      val dicomFile = new DicomFile(file)
      val al = dicomFile.attributeList.get
      val image = new DicomImage(al)
      val translator = new IsoImagePlaneTranslator(al)
      val expected_mm = MeasureTBLREdges.imageCollimatorPositions(al).toTBLR(Util.collimatorAngle(al))

      val results = MeasureTBLREdges.measure(image, translator, Some(expected_mm), 270, image, new Point(0, 0), 0.5)

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