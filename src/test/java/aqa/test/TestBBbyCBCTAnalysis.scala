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
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnnotateImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ImageUtil
import org.aqa.webrun.bbByCBCT.BBbyCBCTAnalysis
import com.pixelmed.dicom.AttributeList

/**
 * Test the BBbyCBCTAnalysis.  Input directories each contain one CT series of DICOM images.
 *
 */

class TestBBbyCBCTAnalysis extends FlatSpec with Matchers {

  val outDir = new File("""target\TestBBbyCBCTAnalysis""")

  def readDicomFile(ct: File) = {
    val al = new AttributeList
    al.read(ct)
    al
  }

  private def testCt(ctDir: File, expectation: Boolean) = {
    val parentOutputDir = new File(outDir, if (expectation) "pass" else "false")
    val outputDir = new File(parentOutputDir, ctDir.getName)
    outputDir.mkdirs
    val cbctSeries = Util.sortByZ(ctDir.listFiles.toSeq.map(ct => readDicomFile(ct)))

    println("Processing input CT dir: " + ctDir.getAbsolutePath + " ==> " + outputDir.getAbsolutePath)
    val result = BBbyCBCTAnalysis.volumeAnalysis(cbctSeries, outputDir)

    val ok = (result.isRight && expectation) || (result.isLeft && (!expectation))
    if (result.isRight && expectation) println("Passed as expected: " + result.right.get.fineLocation_vox)
    if (result.isLeft && (!expectation)) println("Failed as expected")
    ok should be(true)
  }

  def runCbctTest = {
    val valid = Config.validate
    val start = System.currentTimeMillis
    val mainDataDir = new File("""src\test\resources\TestBBbyCBCTAnalysis""")
    println("Using  input dir: " + mainDataDir.getAbsolutePath)
    println("Using output dir: " + outDir.getAbsolutePath)

    Util.deleteFileTreeSafely(outDir)
    outDir.mkdirs

    val passDirList = (new File(mainDataDir, "pass")).listFiles.toSeq
    passDirList.map(ctDir => testCt(ctDir, true))

    val failDirList = (new File(mainDataDir, "fail")).listFiles.toSeq
    failDirList.map(ctDir => testCt(ctDir, false))
    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

  "pass" should "pass" in {
    runCbctTest
  }
}

object TestBBbyCBCTAnalysis {
  def main(args: Array[String]): Unit = {
    (new TestBBbyCBCTAnalysis).runCbctTest
  }
}
