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


package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import edu.umro.ScalaUtil.Trace
import org.opensourcephysics.numerics.CubicSpline
import org.aqa.webrun.phase2.leafPosition.LeafPositionCoarseLeafSides
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.DicomFile

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestCenterDoseAnalysis_testConstructCenterDose extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources""")
  val fileNameList = Seq(
    "TestCenterDoseFlood.dcm",
    "TestCenterDoseJ20G0_6F.dcm",
    "TestCenterDoseT3Open.dcm")

  "TestCenterDoseAnalysis_testConstructCenterDose" should "yield a center dose" in {

    def testFile(fileName: String) = {
      val dicomFile = new DicomFile(new File(dir, fileName))
      println("\n\nfile: " + dicomFile.file.getAbsolutePath)
      val beamName = fileName
      val centerDose = CenterDoseAnalysis.testConstructCenterDose(beamName, dicomFile)
      println("centerDose: " + centerDose)
      System.exit(99)
    }

    fileNameList.map(fn => testFile(fn))

    true should be(true)
  }
}
