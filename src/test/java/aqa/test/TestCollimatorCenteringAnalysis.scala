package aqa.test

import java.io.File
import org.aqa.DicomFile
import edu.umro.ImageUtil.ImageUtil
import javax.imageio.ImageIO
import org.aqa.webrun.phase2.MeasureTBLREdges
import java.awt.Point
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeList

object TestCollimatorCenteringAnalysis {

  def main(args: Array[String]): Unit = {

    val inDir = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\TestCollimatorCenteringAnalysis""")
    val fileNames = Seq("TestCollimatorCenteringAnalysis_1.dcm", "TestCollimatorCenteringAnalysis_2.dcm", "TestCollimatorCenteringAnalysis_3.dcm")

    val fileList = fileNames.map(name => new File(inDir, name))

    val rtplan = {
      val al = new AttributeList
      al.read(new File(inDir, "TestCollimatorCenteringAnalysisRtplan.dcm"))
      al
    }

    def processFile(file: File) = {
      val dicomFile = new DicomFile(file)
      val rtimage = dicomFile.attributeList.get
      val image = new DicomImage(rtimage)
      val translator = new IsoImagePlaneTranslator(rtimage)
      val expected_mm = MeasureTBLREdges.imageCollimatorPositions(rtimage, rtplan).toTBLR(Util.collimatorAngle(rtimage))

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