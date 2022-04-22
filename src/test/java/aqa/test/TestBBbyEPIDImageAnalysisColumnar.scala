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

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.Utility
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.webrun.bbByEpid.BBbyEPIDImageAnalysis
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import java.text.SimpleDateFormat

/**
  * Test the Config.
  *
  */

class TestBBbyEPIDImageAnalysisColumnar extends FlatSpec with Matchers {

  Config.validate

  println("-----------------------------------------------------------------------------------------------------")

  private val mainInDir = TestBBbyEPIDImageAnalysisColumnar.mainInDir
  private val passInDir = new File(mainInDir, "pass")
  private val marginalInDir = new File(mainInDir, "marginal")
  private val failInDir = new File(mainInDir, "fail")

  val mainOutputDir = new File(new File("target"), mainInDir.getName)
  Utility.deleteFileTree(mainOutputDir)
  mainOutputDir.mkdirs

  // list of directories that contain a EPID set for analysis
  println("Using main input directory: " + mainInDir.getAbsolutePath)

  "TestBBbyEPIDImageAnalysisColumnar" should "find BB" in {

    def outputDir(inFile: File) = {
      val outputDirName = inFile.getName.replaceAll(".dcm", "").replaceAll(".DCM", "")
      val subOutputDir = new File(mainOutputDir, inFile.getParentFile.getName)
      Util.mkdirs(subOutputDir)
      val dir = new File(subOutputDir, outputDirName)
      Util.deleteFileTreeSafely(dir)
      Util.mkdirs(dir)
      dir
    }

    def fmt(d: Double): String = d.formatted("%15.10f")
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss")
    val spreadsheet = new StringBuilder

    /**
      * Add a non-final column to the spreadsheet.
      * @param content Contents of column.
      */
    def asp(content: Any): Unit = spreadsheet.append(content.toString + ",")

    /**
      * Add final column to the spreadsheet.
      * @param content Contents of column.
      */
    def asf(content: String): Unit = spreadsheet.append(content + "\n")

    def saveSpreadsheet(name: String): Unit = {
      asf("")
      asp("CSV file written ")
      asf(dateFormat.format(System.currentTimeMillis()))
      val file = new File(mainOutputDir, name + ".csv")
      Util.writeFile(file, spreadsheet.toString())
    }
    def testPassFile(file: File): Unit = {
      println("\nProcessing RTIMAGE file: " + file.getAbsolutePath)

      // write spreadsheet headers.
      if (spreadsheet.isEmpty) {
        asp("Machine")
        asp("Acquisition")
        asp("Gantry Angle")
        asp("Raw Result X")
        asp("Raw Result Y")
        asp("Col Result X")
        asp("Col Result Y")
        asp("Raw-Col X")
        asp("Raw-Col Y")
        asp("Raw-Col XY")
        asf("Dir")
      }

      val al = new DicomFile(file).attributeList.get
      val outDir = outputDir(file)

      val rawResult = BBbyEPIDImageAnalysis.findBB(al, -1, columnarNoiseCorrection = false)
      val colResult = BBbyEPIDImageAnalysis.findBB(al, -1, Some(outDir))

      println("outDir: " + outDir.getAbsolutePath)
      println("rawResult.isRight: " + rawResult.isRight + "     colResult.isRight: " + colResult.isRight)
      if (colResult.isLeft && rawResult.isLeft) { // TODO rm
        println("mv " + file.getName + " ../fail")
      }

      if (colResult.isRight && rawResult.isLeft) { // TODO rm
        println("mv " + file.getName + " ../marginal")
      }

      if (colResult.isLeft && rawResult.isLeft) { // TODO rm
        println("mv " + file.getName + " ../fail")
      }

      if (colResult.isLeft)
        Trace.trace("hey col")

      if (rawResult.isLeft)
        Trace.trace("hey raw")

      if (colResult.isRight && rawResult.isRight) {
        colResult.isRight should be(true)

        rawResult.isRight should be(true)

        val deltaX = rawResult.right.get.bbByEpid.epidImageX_mm - colResult.right.get.bbByEpid.epidImageX_mm
        val deltaY = rawResult.right.get.bbByEpid.epidImageY_mm - colResult.right.get.bbByEpid.epidImageY_mm
        val xy = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
        println(
          "result   : " + fmt(rawResult.right.get.bbByEpid.epidImageX_mm) + "," + fmt(rawResult.right.get.bbByEpid.epidImageY_mm) +
            "    colResult: " + fmt(colResult.right.get.bbByEpid.epidImageX_mm) + "," + fmt(colResult.right.get.bbByEpid.epidImageY_mm) +
            "    delta    : " + fmt(deltaX) + ", " + fmt(deltaY) + "   XY: " + fmt(xy) +
            "    " + outDir.getName
        )

        // write spreadsheet content.
        if (true) {
          val timeDate = {
            val dateTime = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime)
            dateFormat.format(dateTime.get)
          }

          asp(al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString)
          asp(timeDate)
          asp(Util.angleRoundedTo90(Util.gantryAngle(al)))

          asp(rawResult.right.get.bbByEpid.epidImageX_mm)
          asp(rawResult.right.get.bbByEpid.epidImageY_mm)

          asp(colResult.right.get.bbByEpid.epidImageX_mm)
          asp(colResult.right.get.bbByEpid.epidImageY_mm)

          asp(deltaX)
          asp(deltaY)
          asp(xy)
          asf(outDir.getAbsolutePath)
        }
      }

      println
    }

    def testMarginalFile(file: File): Unit = {
      println(file) // TODO
    }

    def testFailFile(file: File): Unit = {
      println(file) // TODO
    }

    val start = System.currentTimeMillis()
    Trace.trace

    spreadsheet.clear()
    Util.listDirFiles(passInDir).filter(_.isFile).foreach(testPassFile)
    saveSpreadsheet(passInDir.getName)

    Util.listDirFiles(marginalInDir).filter(_.isFile).foreach(testMarginalFile)
    Util.listDirFiles(failInDir).filter(_.isFile).foreach(testFailFile)

    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed time in ms: " + elapsed)
    Trace.trace

    println("Wrote files to " + mainOutputDir.getAbsolutePath)
    (11 > 10) should be(true)
  }
}

object TestBBbyEPIDImageAnalysisColumnar {
  private val mainInDir = new File("src\\test\\resources\\TestBBbyEPIDImageAnalysisColumnar")

  private def renameRtimageFile(file: File): Unit = {
    if (file.isFile) {
      val al = new DicomFile(file).attributeList.get
      val seriesDateTimeText = Util.timeAsFileName(DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get)
      val machineName = al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString
      val gantryAngleText = "G" + Util.angleRoundedTo90(DicomUtil.findAllSingle(al, TagByName.GantryAngle).head.getDoubleValues.head).formatted("%03d")
      val newName = FileUtil.replaceInvalidFileNameCharacters(machineName + "_" + seriesDateTimeText + "_" + gantryAngleText + ".dcm", '_')
      val newFile = new File(file.getParentFile, newName)

      if (file.getAbsolutePath.equals(newFile.getAbsolutePath))
        println("File rename FAILED: File already has been renamed: " + file.getAbsolutePath)
      else {
        if (newFile.exists())
          println("Can not rename file.  Desination already exists.\n    old: " + file.getName + "\n    new: " + newFile.getName)
        else {
          val renamed = file.renameTo(newFile)
          if (renamed)
            println("File renamed\n    old: " + file.getName + "\n    new: " + newFile.getName)
          else
            println("File rename FAILED:\n    old: " + file.getName + "\n    new: " + newFile.getName)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    FileUtil.processFileTree(mainInDir, renameRtimageFile)
  }
}
