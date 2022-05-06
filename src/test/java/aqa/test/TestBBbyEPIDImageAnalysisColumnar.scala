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
import org.aqa.Logging
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
      * @param outDir      Write files here.
      */
    def showImage(rtimageFile: File, outDir: File): Unit = {
      val ti = TransIdentity(rtimageFile)
      val pointList = BBbyEPIDImageAnalysis.testListOfPointsWithinBB(ti.trans)

      val outTextFile = new File(outDir, "pointList.txt")

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

    case class Col(title: String, toText: (BBbyEPIDImageAnalysis.Result, BBbyEPIDImageAnalysis.Result) => Any) {}

    var outDir: File = null

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
      Col("Old Pixel Co-eff of Var", (o, _) => (o.bbByEpid.pixelStandardDeviation_cu / o.bbByEpid.pixelMean_cu).toString),
      Col("Old BB Mean CU", (o, _) => o.bbMean_cu.toString),
      Col("Old Pixel StdDev CU", (o, _) => o.bbByEpid.pixelStandardDeviation_cu.toString),
      Col("Old BB StdDev Multiple CU", (o, _) => o.bbByEpid.bbStdDevMultiple.toString),
      Col("New Pixel Mean CU", (_, n) => n.bbByEpid.pixelMean_cu.toString),
      Col("New Pixel Co-eff of Var", (_, n) => (n.bbByEpid.pixelStandardDeviation_cu / n.bbByEpid.pixelMean_cu).toString),
      Col("New BB Mean CU", (_, n) => n.bbMean_cu.toString),
      Col("New Pixel Mean StdDev", (_, n) => n.bbByEpid.pixelStandardDeviation_cu.toString),
      Col("New BB StdDev Multiple CU", (_, n) => n.bbByEpid.bbStdDevMultiple.toString),
      Col("Pix Size X mm", (o, _) => o.al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.head.toString),
      Col("Pix Size Y mm", (o, _) => o.al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues()(1).toString),
      Col("Dir", (_, _) => outDir.getAbsolutePath)
    )

    var prevMach = "None"

    def addRowToSpreadsheet(oldResult: BBbyEPIDImageAnalysis.Result, newResult: BBbyEPIDImageAnalysis.Result): Unit = {
      val deltaX = oldResult.bbByEpid.epidImageX_mm - newResult.bbByEpid.epidImageX_mm
      val deltaY = oldResult.bbByEpid.epidImageY_mm - newResult.bbByEpid.epidImageY_mm
      val xy = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
      println(
        "result   : " + fmt(oldResult.bbByEpid.epidImageX_mm) + "," + fmt(oldResult.bbByEpid.epidImageY_mm) +
          "    colResult: " + fmt(newResult.bbByEpid.epidImageX_mm) + "," + fmt(newResult.bbByEpid.epidImageY_mm) +
          "    delta    : " + fmt(deltaX) + ", " + fmt(deltaY) + "   XY: " + fmt(xy) +
          "    " + outDir.getName
      )

      // write spreadsheet content.  Put a blank line between machines to improve view-ability.
      val rowText = colSeq.map(c => c.toText(oldResult, newResult).toString).mkString(",") + "\n"
      val mach = oldResult.al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString()
      if (!mach.equals(prevMach)) spreadsheet.append("\n")
      spreadsheet.append(rowText)
      prevMach = mach
    }

    /**
      * Test an RTIMAGE file that is known to be valid.
      *
      * @param rtimageFile RTIMAGE file.
      */
    def testFile(rtimageFile: File, oldStatus: Boolean, newStatus: Boolean): Unit = {
      println("\nProcessing RTIMAGE file: " + rtimageFile.getAbsolutePath)

      val al = new DicomFile(rtimageFile).attributeList.get
      outDir = outputDir(rtimageFile)

      showImage(rtimageFile, outDir)
      val fullImage = new DicomImage(al)
      val fullBufImage = fullImage.toDeepColorBufferedImage(0.01)
      val fullImageFile = new File(outDir, "fullImage.png")
      Util.writePng(fullBufImage, fullImageFile) // write full sized image

      Util.writeFile(new File(outDir, "RTIMAGE.txt"), DicomUtil.attributeListToString(al)) // write text version to file for diagnosing problems.

      val oldResult = BBbyEPIDImageAnalysis.findBBold(al, outputPK = -1)
      val newResult = BBbyEPIDImageAnalysis.findBB_columnCorrectedGrowBB(al, outputPK = -1)
      // val colResult = BBbyEPIDImageAnalysis.findBB(al, -1, Some(outDir))

      // write spreadsheet headers.
      if (oldStatus && newStatus && spreadsheet.isEmpty) spreadsheet.append(colSeq.map(c => c.title).mkString(",") + "\n")

      println("outDir: " + outDir.getAbsolutePath)
      println("rawResult.ok: " + oldResult.ok + "     colResult.ok: " + newResult.ok)

      if (newResult.ok && newResult.rawSearchArea.isDefined) {
        val enlarged = ImageUtil.magnify(newResult.rawSearchArea.get.toDeepColorBufferedImage(0.01), magnification)
        val file = new File(outDir, "rawSearchArea.png")
        Util.writePng(enlarged, file)

        val enlargedBw = ImageUtil.magnify(newResult.rawSearchArea.get.toBufferedImage(Color.white), magnification)
        val fileBw = new File(outDir, "rawSearchAreaBlackAndWhite.png")
        Util.writePng(enlargedBw, fileBw)
      }

      if (newResult.ok && newResult.processedSearchArea.isDefined) {
        val enlarged = ImageUtil.magnify(newResult.processedSearchArea.get.toDeepColorBufferedImage(0.01), magnification)
        val file = new File(outDir, "processedSearchArea.png")
        Util.writePng(enlarged, file)

        val enlargedBw = ImageUtil.magnify(newResult.processedSearchArea.get.toBufferedImage(Color.white), magnification)
        val fileBw = new File(outDir, "processedSearchAreaBlackAndWhite.png")
        Util.writePng(enlargedBw, fileBw)
      }

      if (newResult.ok && newResult.rawSearchArea.isDefined && newResult.bbPointList.isDefined) {
        val image = newResult.processedSearchArea.get.toDeepColorBufferedImage(0.01)

        println("image size: " + image.getWidth + " x " + image.getHeight())
        println("coordinates: " + newResult.bbPointList.get.mkString("  "))
        val coordinates = newResult.bbPointList.get.filter(p => p.x >= 0 && p.y >= 0 && p.x < image.getWidth() && p.y < image.getHeight())
        val outOfBounds = newResult.bbPointList.get.diff(coordinates)
        if (outOfBounds.nonEmpty) println("List of out of bound coordinates: " + outOfBounds.mkString("  "))

        newResult.bbPointList.get.foreach(p => image.setRGB(p.x, p.y, Color.black.getRGB))
        val enlarged = ImageUtil.magnify(image, magnification)
        val file = new File(outDir, "processedSearchAreaWithBBFromProcessedMarked.png")
        Util.writePng(enlarged, file)
      }

      if (oldResult.ok) {
        val diagnostics = oldResult.diagnostics
        Util.writeFile(new File(outDir, "oldDiagnostics.txt"), diagnostics)
        println(diagnostics)
      }

      if (newResult.ok) {
        val diagnostics = newResult.diagnostics
        Util.writeFile(new File(outDir, "newDiagnostics.txt"), diagnostics)
        println(diagnostics)
      }

      if (newResult.ok)
        if (oldResult.ok && oldResult.rawSearchArea.isDefined && oldResult.bbPointList.isDefined) {
          val image = oldResult.rawSearchArea.get.toDeepColorBufferedImage(0.01)

          println("image size: " + image.getWidth + " x " + image.getHeight())
          println("coordinates: " + oldResult.bbPointList.get.mkString("  "))
          val coordinates = oldResult.bbPointList.get.filter(p => p.x >= 0 && p.y >= 0 && p.x < image.getWidth() && p.y < image.getHeight())
          val outOfBounds = oldResult.bbPointList.get.diff(coordinates)
          if (outOfBounds.nonEmpty) println("List of out of bound coordinates: " + outOfBounds.mkString("  "))

          coordinates.foreach(p => image.setRGB(p.x, p.y, Color.black.getRGB))
          val enlarged = ImageUtil.magnify(image, magnification)
          val file = new File(outDir, "rawSearchAreaWithBBFromRawMarked.png")
          Util.writePng(enlarged, file)
        }

      // if both the old and new methods work, then add a row to the CSV file to facilitate comparison.
      if (newResult.ok && oldResult.ok) addRowToSpreadsheet(oldResult, newResult)

      println(
        outDir.getName +
          " old status:    expected: " + oldStatus + "    old status actual: " + oldResult.ok +
          " new status:    expected: " + newStatus + "    new status actual: " + newResult.ok
      )

      if (!oldResult.ok.toString.equals(oldStatus.toString))
        println(outDir.getName + " old status is wrong.  expected: " + oldStatus + "    actual: " + oldResult.ok + "    outDir: " + outDir.getAbsolutePath)
      else
        println(outDir.getName + " old status is correct.  expected: " + oldStatus + "    actual: " + oldResult.ok + "    outDir: " + outDir.getAbsolutePath)

      if (!newResult.ok.toString.equals(newStatus.toString))
        println(outDir.getName + " new status is wrong.  expected: " + newStatus + "    actual: " + newResult.ok + "    outDir: " + outDir.getAbsolutePath)
      else
        println(outDir.getName + " new status is correct.  expected: " + newStatus + "    actual: " + newResult.ok + "    outDir: " + outDir.getAbsolutePath)

      if (oldResult.ok.toString.equals(oldStatus.toString) && newResult.ok.toString.equals(newStatus.toString))
        println(outDir.getName + " both old and new statuses are correct: old: " + oldResult.ok + "   new: " + newResult.ok)

      // statuses should match the expected statuses
      oldResult.ok should be(oldStatus)
      newResult.ok should be(newStatus)

      println
    }

    val start = System.currentTimeMillis()
    Trace.trace

    spreadsheet.clear()

    Util.listDirFiles(passInDir).filter(_.isFile).foreach(file => testFile(file, oldStatus = true, newStatus = true))

    Util.listDirFiles(marginalInDir).filter(_.isFile).foreach(file => testFile(file, oldStatus = false, newStatus = true))

    Util.listDirFiles(failInDir).filter(_.isFile).foreach(file => testFile(file, oldStatus = false, newStatus = false))

    saveSpreadsheet(passInDir.getName)

    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed time in ms: " + elapsed)
    Trace.trace

    println("Wrote files to " + mainOutputDir.getAbsolutePath)
  }
  (11 > 10) should be(true)

}

object TestBBbyEPIDImageAnalysisColumnar extends Logging {
  val mainInDir = new File("src\\test\\resources\\TestBBbyEPIDImageAnalysisColumnar")

  private def renameRtimageFile(file: File): Unit = {
    try {
      if (file.isFile && DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
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
            println("Can not rename file.  Destination already exists.\n    old: " + file.getName + "\n    new: " + newFile.getName)
          else {
            val renamed = file.renameTo(newFile)
            if (renamed)
              println("File renamed\n    old: " + file.getName + "\n    new: " + newFile.getName)
            else
              println("File rename FAILED:\n    old: " + file.getName + "\n    new: " + newFile.getName)
          }
        }
      }
    } catch {
      case t: Throwable => println("Could not rename file: " + file.getAbsolutePath + " :: " + fmtEx(t))
    }
  }

  def main(args: Array[String]): Unit = {
    FileUtil.processFileTree(mainInDir, renameRtimageFile)
  }
}
