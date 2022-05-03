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
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
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

import java.awt.Color
import java.awt.Rectangle
import java.awt.image.BufferedImage
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

  private val magnification = 10

  val mainOutputDir = new File(new File("target"), mainInDir.getName)
  Utility.deleteFileTree(mainOutputDir)
  mainOutputDir.mkdirs

  // list of directories that contain a EPID set for analysis
  println("Using main input directory: " + mainInDir.getAbsolutePath)

  "TestBBbyEPIDImageAnalysisColumnar" should "find BB" in {

    def outputDir(inFile: File, prefix: String = "") = {
      val outputDirName = inFile.getName.replaceAll(".dcm", "").replaceAll(".DCM", "")
      val subOutputDir = new File(mainOutputDir, inFile.getParentFile.getName)
      Util.mkdirs(subOutputDir)
      val dir = new File(subOutputDir, prefix + outputDirName)
      Util.deleteFileTreeSafely(dir)
      Util.mkdirs(dir)
      dir
    }

    def fmt(d: Double): String = d.formatted("%15.10f")

    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss")
    val spreadsheet = new StringBuilder

    def saveSpreadsheet(name: String): Unit = {
      spreadsheet.append("CSV file written," + dateFormat.format(System.currentTimeMillis()) + "\n")
      val file = new File(mainOutputDir, name + ".csv")
      Util.writeFile(file, spreadsheet.toString())
    }

    case class TransIdentity(file: File) {
      val al: AttributeList = new DicomFile(file).attributeList.get
      val trans = new IsoImagePlaneTranslator(al)
      // Key parameters for translation.  Used to compare if two IsoImagePlaneTranslators are the same.
      override def toString: String = {
        val items = Seq(
          "image height pix: " + trans.height,
          "pixel size X: " + trans.pixelSizeX.formatted("%8.6f").replaceAll("0*$", ""),
          "pixel size Y: " + trans.pixelSizeY.formatted("%8.6f").replaceAll("0*$", ""),
          "RTImageSID          : " + al.get(TagByName.RTImageSID).getDoubleValues.head.formatted("%12.6f"), // round off to ignore sub-mm differences
          "RadiationMachineSAD : " + al.get(TagByName.RadiationMachineSAD).getDoubleValues.head.formatted("%12.6f") // round off to ignore sub-mm differences
        )
        items.mkString("\n    ", "\n    ", "\n")
      }
    }

    /**
      * Show a colored image of the center of the RTIMAGE with a sub-image showing the map of
      * pixels used to find the BB.  The sub-image is shown on a checkered background with the
      * green pixels indicating which are part of the BB.
      *
      * The purpose of this is to provide a side by side comparison of the model of the BB with
      * real examples of BBs.  This tells us that the model is appropriately sized.
      *
      * @param rtimageFile RTIMAGE DICOM file
      * @param outDir Write files here.
      */
    def showImage(rtimageFile: File, outDir: File): Unit = {
      val ti = TransIdentity(rtimageFile)
      val pointList = BBbyEPIDImageAnalysis.testListOfPointsWithinBB(ti.trans)

      val outTextFile = new File(outDir, "pointList.txt")

      val text =
        "Source file: " + ti.file.getAbsolutePath + "\n" +
          ti.toString + "\n" +
          "List of points profiling BB for this resolution:\n    " + pointList.map(p => p.x.formatted("%2d") + "," + p.y.formatted("%2d")).mkString("\n    ") + "\n"
      Util.writeFile(outTextFile, text)

      println("Wrote file " + outTextFile.getAbsolutePath)

      val width = pointList.map(_.x).max + 3
      val height = pointList.map(_.y).max + 3
      val bufImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

      // make image checked
      for (x <- 0 until bufImg.getWidth; y <- 0 until bufImg.getHeight) yield {
        bufImg.setRGB(x, y, if ((x + y) % 2 == 0) Color.black.getRGB else Color.gray.getRGB)
      }

      pointList.foreach(p => bufImg.setRGB(p.x + 1, p.y + 1, Color.green.getRGB))

      val bigImage = ImageUtil.magnify(bufImg, magnification)

      val di = new DicomImage(ti.al)
      val fraction = 0.465
      val centerRect = new Rectangle(Util.d2i(di.width * fraction), Util.d2i(di.height * fraction), Util.d2i(di.width * (1 - fraction * 2)), Util.d2i(di.height * (1 - fraction * 2)))
      val sub = new DicomImage(ti.al).getSubimage(centerRect)
      val img = ImageUtil.magnify(sub.toDeepColorBufferedImage(0.01), magnification)

      val xOffset = (img.getWidth / 2) - (bigImage.getWidth * 2)
      val yOffset = (img.getHeight - bigImage.getHeight) / 2
      for (x <- 0 until bigImage.getWidth; y <- 0 until bigImage.getHeight) {
        img.setRGB(x + xOffset, y + yOffset, bigImage.getRGB(x, y))
      }

      val file = new File(outDir, outDir.getName + "origCenter.png")
      Util.writePng(img, file)
      println("Wrote file " + file.getAbsolutePath)

    }

    case class Col(title: String, toText: (BBbyEPIDImageAnalysis.Result, BBbyEPIDImageAnalysis.Result) => String) {}

    var prevMach = "None"

    /**
      * Test an RTIMAGE file that is known to be valid.
      * @param rtimageFile RTIMAGE file.
      */
    def testPassFile(rtimageFile: File): Unit = {
      println("\nProcessing RTIMAGE file: " + rtimageFile.getAbsolutePath)

      val al = new DicomFile(rtimageFile).attributeList.get
      val outDir = outputDir(rtimageFile)

      val colSeq: Seq[Col] = Seq(
        Col("Machine", (o, _) => o.al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString),
        Col("Acquisition", (o, _) => dateFormat.format(DicomUtil.getTimeAndDate(o.al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get)),
        Col("Gantry Angle", (o, _) => Util.angleRoundedTo90(Util.gantryAngle(o.al)).toString),
        Col("Old X mm", (o, _) => o.bbByEpid.epidImageX_mm.toString),
        Col("Old Y mm", (o, _) => o.bbByEpid.epidImageY_mm.toString),
        Col("New X mm", (_, n) => n.bbByEpid.epidImageX_mm.toString),
        Col("New Y mm", (_, n) => n.bbByEpid.epidImageY_mm.toString),
        Col("Old-New X", (o, n) => (o.bbByEpid.epidImageX_mm - n.bbByEpid.epidImageX_mm).toString),
        Col("Old-New Y", (o, n) => (o.bbByEpid.epidImageY_mm - n.bbByEpid.epidImageY_mm).toString),
        Col(
          "Old New XY",
          (o, n) => {
            val x = o.bbByEpid.epidImageX_mm - n.bbByEpid.epidImageX_mm
            val y = o.bbByEpid.epidImageY_mm - n.bbByEpid.epidImageY_mm
            Math.sqrt(x * x + y * y).toString
          }
        ),
        Col("Old Pixel Mean CU", (o, _) => o.bbByEpid.pixelMean_cu.toString),
        Col("Old BB Mean CU", (o, _) => o.bbMean_cu.toString),
        Col("Old Pixel StdDev CU", (o, _) => o.bbByEpid.pixelStandardDeviation_cu.toString),
        Col("Old BB StdDev Mult CU", (o, _) => o.bbByEpid.bbStdDevMultiple.toString),
        Col("New Pixel Mean CU", (_, n) => n.bbByEpid.pixelMean_cu.toString),
        Col("New BB Mean CU", (_, n) => n.bbMean_cu.toString),
        Col("New Pixel Mean CU", (_, n) => n.bbByEpid.pixelStandardDeviation_cu.toString),
        Col("New BB StdDev Mult CU", (_, n) => n.bbByEpid.bbStdDevMultiple.toString),
        Col("Pix Size X mm", (o, _) => o.al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.head.toString),
        Col("Pix Size Y mm", (o, _) => o.al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues()(1).toString),
        Col("Dir", (_, _) => outDir.getAbsolutePath)
      )

      // write spreadsheet headers.
      if (spreadsheet.isEmpty) spreadsheet.append(colSeq.map(c => c.title).mkString(",") + "\n")

      showImage(rtimageFile, outDir)

      Util.writeFile(new File(outDir, "RTIMAGE.txt"), DicomUtil.attributeListToString(al)) // write text version to file for diagnosing problems.

      val oldResult = BBbyEPIDImageAnalysis.findBBold(al, outputPK = -1)
      val newResult = BBbyEPIDImageAnalysis.findBB_columnCorrectedGrowBB(al, outputPK = -1)
      // val colResult = BBbyEPIDImageAnalysis.findBB(al, -1, Some(outDir))

      println("outDir: " + outDir.getAbsolutePath)
      println("rawResult.isRight: " + oldResult.isRight + "     colResult.isRight: " + newResult.isRight)
      if (newResult.isLeft && oldResult.isLeft) { // TODO rm
        println("mv " + rtimageFile.getName + " ../fail")
      }

      if (newResult.isRight && oldResult.isLeft) { // TODO rm
        println("mv " + rtimageFile.getName + " ../marginal")
      }

      if (newResult.isLeft && oldResult.isLeft) { // TODO rm
        println("mv " + rtimageFile.getName + " ../fail")
      }

      if (newResult.isLeft)
        Trace.trace("hey col")

      if (oldResult.isLeft)
        Trace.trace("hey raw")

      if (newResult.isRight && oldResult.isRight) {
        newResult.isRight should be(true)

        oldResult.isRight should be(true)

        val deltaX = oldResult.right.get.bbByEpid.epidImageX_mm - newResult.right.get.bbByEpid.epidImageX_mm
        val deltaY = oldResult.right.get.bbByEpid.epidImageY_mm - newResult.right.get.bbByEpid.epidImageY_mm
        val xy = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
        println(
          "result   : " + fmt(oldResult.right.get.bbByEpid.epidImageX_mm) + "," + fmt(oldResult.right.get.bbByEpid.epidImageY_mm) +
            "    colResult: " + fmt(newResult.right.get.bbByEpid.epidImageX_mm) + "," + fmt(newResult.right.get.bbByEpid.epidImageY_mm) +
            "    delta    : " + fmt(deltaX) + ", " + fmt(deltaY) + "   XY: " + fmt(xy) +
            "    " + outDir.getName
        )

        if (newResult.right.get.rawSearchArea.isDefined) {
          val enlarged = ImageUtil.magnify(newResult.right.get.rawSearchArea.get.toDeepColorBufferedImage(0.01), magnification)
          val file = new File(outDir, "rawSearchArea.png")
          Util.writePng(enlarged, file)
        }

        if (newResult.right.get.processedSearchArea.isDefined) {
          val enlarged = ImageUtil.magnify(newResult.right.get.processedSearchArea.get.toDeepColorBufferedImage(0.01), magnification)
          val file = new File(outDir, "processedSearchArea.png")
          Util.writePng(enlarged, file)
        }

        if (newResult.right.get.rawSearchArea.isDefined && newResult.right.get.bbPointList.isDefined) {
          val image = newResult.right.get.processedSearchArea.get.toDeepColorBufferedImage(0.01)
          newResult.right.get.bbPointList.get.foreach(p => image.setRGB(p.x, p.y, Color.black.getRGB))
          val enlarged = ImageUtil.magnify(image, magnification)
          val file = new File(outDir, "processedSearchAreaWithBBFromProcessedMarked.png")
          Util.writePng(enlarged, file)
        }

        if (oldResult.right.get.rawSearchArea.isDefined && oldResult.right.get.bbPointList.isDefined) {
          val image = oldResult.right.get.rawSearchArea.get.toDeepColorBufferedImage(0.01)
          oldResult.right.get.bbPointList.get.foreach(p => image.setRGB(p.x, p.y, Color.black.getRGB))
          val enlarged = ImageUtil.magnify(image, magnification)
          val file = new File(outDir, "rawSearchAreaWithBBFromRawMarked.png")
          Util.writePng(enlarged, file)
        }

        // write spreadsheet content.  Put a blank line between machines to improve view-ability.
        val rowText = colSeq.map(c => c.toText(oldResult.right.get, newResult.right.get)).mkString(",") + "\n"
        val mach = oldResult.right.get.al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString()
        if (!mach.equals(prevMach)) spreadsheet.append("\n")
        spreadsheet.append(rowText)
        prevMach = mach

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
  }
  (11 > 10) should be(true)

}

object TestBBbyEPIDImageAnalysisColumnar {
  val mainInDir = new File("src\\test\\resources\\TestBBbyEPIDImageAnalysisColumnar")

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
