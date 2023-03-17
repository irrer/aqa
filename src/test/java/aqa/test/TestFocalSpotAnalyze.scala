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
import com.pixelmed.dicom.DicomFileUtilities
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.webrun.focalSpot.FSSet
import org.aqa.Config
import org.aqa.Logging
import org.aqa.webrun.focalSpot.FSMeasure
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test the FSAnalyze.measure function, which does the edge measurements for focal spot.
  */

class TestFocalSpotAnalyze extends FlatSpec with Matchers with Logging {
  "Focal Spots" should "be good" in {

    val outDir = new File(new File("target"), "FocalSpot")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs()

    def showResult(inDir: File, fsSet: FSSet): Unit = {

      val outSubDir = new File(outDir, inDir.getName)
      outSubDir.mkdirs()

      def save(fsMeasure: FSMeasure): Unit = {
        val outFileName = fsMeasure.beamName + ".png"
        val outFile = new File(outSubDir, outFileName)
        Util.writePng(fsMeasure.bufferedImage, outFile)
        println("Wrote file " + outFile.getAbsolutePath)
      }

      save(fsSet.jaw090)
      save(fsSet.jaw270)
      save(fsSet.mlc090)
      save(fsSet.mlc270)

      Trace.trace("X Alignment: " + fsSet.focalSpotAlignmentX)
      Trace.trace("Y Alignment: " + fsSet.focalSpotAlignmentY)
      println
    }

    def readDicomFile(file: File): Option[AttributeList] = {
      if (DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
        try {
          val al = new AttributeList
          al.read(file)
          Some(al)
        } catch {
          case _: Throwable => None
        }
      } else
        None
    }

    def doMlc(inDir: File, alList: Seq[AttributeList]): Unit = {

      println("\n\nProcessing " + inDir.getAbsolutePath)
      val rtplan = alList.find(Util.isRtplan).get
      val rtimageList = alList.filter(Util.isRtimage)

      ??? // TODO
      // val fsSetList = FSAnalysis(rtplan, rtimageList).setList

      // fsSetList.foreach(fsSet => showResult(inDir, fsSet))

    }

    /** Main directory containing test data. */
    val mainDir = new File("""src\test\resources\TestFocalSpot""")

    Config.validate

    /** Individual test cases. */
    val dirList = Util.listDirFiles(mainDir)

    def toInput(dir: File): (File, Seq[AttributeList]) = {
      val alList = Util.listDirFiles(dir).flatMap(readDicomFile)
      (dir, alList)
    }

    val list = dirList.map(toInput)
    println("\nProcessing directories:")
    list.foreach(da => doMlc(da._1, da._2))

    true should be(true)
    println("\nDone.")
  }
}
