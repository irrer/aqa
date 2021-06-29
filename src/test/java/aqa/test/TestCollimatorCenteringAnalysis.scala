/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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