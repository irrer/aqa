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

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.db.VMAT
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.vmat.VMATAnalysis
import org.scalatest.Assertion
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.awt.Rectangle
import java.io.File

/**
  * Test VMAT analysis.
  */

class TestVMATAnalysis extends FlatSpec with Matchers {

  Config.validate

  (0 to 5).foreach(_ => println("-----------------------------------------------------------------------------------------------------"))

  private def testVmatPair(vmatPair: Config.VMATBeamPair, rtplan: AttributeList, imageList: Seq[DicomFile]): Unit = {
    println("Testing VMAT beam pair: " + vmatPair)
    def isTheNamedBeam(img: AttributeList, name: String): Boolean = {
      val n = Phase2Util.getBeamNameOfRtimage(rtplan, img)
      n.isDefined && n.get.equals(name)
    }

    val mlc = imageList.find(df => isTheNamedBeam(df.attributeList.get, vmatPair.MLC)).get.attributeList.get
    val open = imageList.find(df => isTheNamedBeam(df.attributeList.get, vmatPair.OPEN)).get.attributeList.get

    if (false) { // TODO rm
      println
      println(new DicomImage(mlc).getSubimage(new Rectangle(1190 - 50, 1190 - 40, 50, 50)).pixelsToText)
      println
      println(new DicomImage(open).getSubimage(new Rectangle(1190 - 50, 1190 - 40, 50, 50)).pixelsToText)
      System.exit(99)
    }

    val aoiList = VMATAnalysis.testGetPlanAoiList(vmatPair, rtplan)
    println("AOI List: \n    " + aoiList.mkString("\n    "))

    // fake centering with offsets at 0
    val collimatorCentering =
      new CollimatorCentering(
        None,
        -1,
        ProcedureStatus.running.toString,
        "sop090",
        "sop270",
        gantryAngleRounded_deg = 0,
        beamName090 = "beam090",
        beamName270 = "beam270",
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      )
    val outputPK: Long = -1
    val vmatSeq = VMATAnalysis.testAnalyze(vmatPair, mlc, open, rtplan, collimatorCentering, outputPK, new DicomImage(mlc), new DicomImage(open))

    def vmatFormatted(vmat: VMAT) = {
      def fmt(d: Double) = d.formatted("%10.3f")
      "Left plan mm: " + fmt(vmat.leftRtplan_mm) +
        "    mlc cu: " + fmt(vmat.doseMLC_cu) +
        "    open cu: " + fmt(vmat.doseOpen_cu) +
        "    percent: " + fmt(vmat.percent)
    }
    println("analysis results:\n    " + vmatSeq.map(vmat => vmatFormatted(vmat)).mkString("\n    "))
  }

  private def testDir(dir: File) = {
    println("processing directory: " + dir.getAbsolutePath)

    val dicomFileList = Util.listDirFiles(dir).filter(f => f.getName.toLowerCase.endsWith(".dcm")).map(f => new DicomFile(f))

    val rtplan = dicomFileList.find(df => Util.modalityOfAl(df.attributeList.get).equals("RTPLAN")).get.attributeList.get
    val imageList = dicomFileList.filter(df => Util.modalityOfAl(df.attributeList.get).equals("RTIMAGE"))

    Config.VMATBeamPairList.foreach(vmatPair => testVmatPair(vmatPair, rtplan, imageList))

    true should be(true)
  }

  def runTest: Assertion = {

    // list of directories that contain a VMAT set for analysis
    val dirList = new File("""src\test\resources\TestVMAT""").listFiles
    println("List of VMAT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

    dirList.map(dir => testDir(dir))

    true should be(true)

  }

  "TestVMATAnalysis test" should "find percentages" in {
    runTest
  }

}

object TestVMATAnalysis {
  def main(args: Array[String]): Unit = {
    (new TestVMATAnalysis).runTest
  }
}
