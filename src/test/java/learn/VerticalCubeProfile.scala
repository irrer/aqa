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

package learn

import edu.umro.ImageUtil.ImageUtil
import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomVolume
import edu.umro.ImageUtil.DicomImage
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import javax.vecmath.Point2i
import edu.umro.ScalaUtil.Trace
import javax.vecmath.Point3d
import java.text.SimpleDateFormat
import java.util.Date
import javax.vecmath.Point2d

object VerticalCubeProfile {

  // Config values
  val DailyQAPhantomCubeSize_mm = 50.0
  val DailyQACBCTPhantomMinHorizontalPlaneSum_cu = 100000.0
  val DailyQACBCTFlatnessMinimum = 0.05
  //val DailyQACBCTBucketCount = 100
  val DailyQACBCTVoxPercentTolerance = 10.0

  /** Max number of CTs analyzed per patient. */
  val ctPerPatient = 1000

  /** Height of output graph in pixels. */
  val pngHeight = 400

  val outDir = new File("""target\VerticalCubeProfile""")

  def readDicomFile(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def horizontalSlice(entireVolume: DicomVolume, sliceIndex: Int): IndexedSeq[IndexedSeq[Float]] = {
    (0 until entireVolume.xSize).map(x => (0 until entireVolume.zSize).map(z => entireVolume.getXYZ(x, sliceIndex, z)))
  }

  def horizontalSliceToSum(entireVolume: DicomVolume, sliceIndex: Int): Float = horizontalSlice(entireVolume, sliceIndex).flatten.sum

  def yCubeSize_vox(al: AttributeList): Int = {
    val ySize = al.get(TagFromName.PixelSpacing).getDoubleValues()(1)
    val cubeLen_vox = (DailyQAPhantomCubeSize_mm / ySize).round.toInt
    println("Y cube size in voxels : " + cubeLen_vox)
    cubeLen_vox
  }

  def addProfile(buf: BufferedImage, color: Color, points: Seq[Float]) = {
    val min = points.min
    val range = points.max - min
    val graphics = ImageUtil.getGraphics(buf)
    graphics.setColor(color)
    val scale = buf.getHeight / range
    val maxY = buf.getHeight - 1
    val rgb = color.getRGB

    val prev = new Point2i(-1, -1)
    def putPoint(p: Float, i: Int) = {
      val x = i
      val y: Int = {
        buf.getHeight - ((p - min) * scale).round.toInt match {
          case yy if yy < 0 => 0
          case yy if yy > maxY => maxY
          case yy => yy
        }
      }
      buf.setRGB(x, y, rgb)
      if (prev.getX >= 0) graphics.drawLine(prev.getX, prev.getY, x, y)

      prev.setX(x)
      prev.setY(y)
    }

    points.take(buf.getWidth).zipWithIndex.map(pi => putPoint(pi._1, pi._2))
  }

  def profileToFlatness(profile: Seq[Float], cubeLen_vox: Int): Seq[Float] = {
    val seg = cubeLen_vox / 2

    def flatness(i: Int): Float = {
      val list = profile.drop(i).take(seg)
      val stdDev = ImageUtil.stdDev(list)
      val avg = list.sum / list.size
      val s = list.size
      if (avg == 0)
        0.toFloat
      else
        (stdDev / avg).toFloat
    }

    val flatList = (0 until profile.size - seg).map(i => flatness(i))
    flatList
  }

  def profileToRange(profile: Seq[Float], cubeLen_vox: Int): Seq[Float] = {
    val seg = cubeLen_vox / 2

    def range(i: Int): Float = {
      val list = profile.drop(i).take(seg)
      val avg = list.sum / list.size
      if (avg == 0)
        0.toFloat
      else
        (list.max - list.min) / avg
    }

    val rangeList = (0 until profile.size - seg).map(i => range(i))
    val min = rangeList.min
    val max = rangeList.max
    rangeList
  }

  def toBucket(entireVolume: DicomVolume, sliceIndex: Int, bucketCount: Int) = {
    val voxList = horizontalSlice(entireVolume, sliceIndex).flatten.sorted.dropRight(37)
    val min = voxList.min
    val max = voxList.max

    val range = max - min

    val list = voxList.map(v => (((v - min) / range) * bucketCount).floor.toInt).groupBy(b => b).map(b => (b._1, b._2.size))
    val hist = (0 until bucketCount).toSeq.map(i => if (list.contains(i)) list(i) else 0)
    val s = hist.size
    if (hist.size != bucketCount) {
      println("hey !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! expected: " + bucketCount + "    got: " + s)
      hist.take(bucketCount)
    } else
      hist
  }

  def percentOfCubeVoxels2(entireVolume: DicomVolume, sliceIndex: Int, voxSize_mm: Point3d): Double = {
    val expected: Int = ((DailyQAPhantomCubeSize_mm / voxSize_mm.getX) * (DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)).round.toInt

    //    // Make a list voxel buckets.  Each bucket has a count of the number of voxels
    //    // in it.
    //    val bucketList = toBucket(entireVolume, sliceIndex, DailyQACBCTBucketCount)
    //
    //    // Determine the number of voxels expected, based on the dimensions of the cube.
    //
    //    val foundX = bucketList.drop(bucketList.size / 2).sum

    val voxelList = horizontalSlice(entireVolume, sliceIndex).flatten
    val mid = (voxelList.max - voxelList.min) / 2
    val found = voxelList.filter(v => v > mid).size

    val percent = (found * 100.0) / expected
    println("==== sliceIndex: " + sliceIndex.formatted("%4d") + "    expected: " + expected + "    found: " + found + "    pct found: " + percent)
    percent
  }

  def histogram(buf: BufferedImage, entireVolume: DicomVolume, sliceIndex: Int, voxSize_mm: Point3d): Seq[Float] = {
    val bucketList = toBucket(entireVolume, sliceIndex, buf.getWidth)
    println("buckets: " + sliceIndex.formatted("%4d") + " : " + bucketList.mkString("  "))
    val b = bucketList.tail.map(_.toFloat) // drop the first one because it is always a bunch of zeroes that flattens the rest of the graph
    b
  }

  def findCenter(entireVolume: DicomVolume, profile: Seq[Float], cubeLen_vox: Int, voxSize_mm: Point3d): Int = {
    val seg = cubeLen_vox / 2

    def flatness(i: Int): Float = {
      val list = profile.drop(i).take(seg)
      val stdDev = ImageUtil.stdDev(list)
      val avg = list.sum / list.size

      val f = if (avg == 0)
        0.toFloat
      else
        (stdDev / avg).toFloat
      f
    }

    def percentOfCubeVoxels(sliceIndex: Int): Double = {
      val expected: Int = ((DailyQAPhantomCubeSize_mm / voxSize_mm.getX) * (DailyQAPhantomCubeSize_mm / voxSize_mm.getZ)).round.toInt

      //    // Make a list voxel buckets.  Each bucket has a count of the number of voxels
      //    // in it.
      //    val bucketList = toBucket(entireVolume, sliceIndex, DailyQACBCTBucketCount)
      //
      //    // Determine the number of voxels expected, based on the dimensions of the cube.
      //
      //    val foundX = bucketList.drop(bucketList.size / 2).sum

      val voxelList = horizontalSlice(entireVolume, sliceIndex).flatten
      val mid = (voxelList.max - voxelList.min) / 2
      val found = voxelList.filter(v => v > mid).size

      val percent = (found * 100.0) / expected
      println("==== sliceIndex: " + sliceIndex.formatted("%4d") + "    expected: " + expected + "    found: " + found + "    pct found: " + percent)
      percent
    }

    def hasCubeVoxels(sliceIndex: Int): Boolean = {
      val pct = percentOfCubeVoxels(sliceIndex)
      val diff = (100.0 - pct).abs
      diff < DailyQACBCTVoxPercentTolerance
    }

    def isGood(i: Int) = {
      val a = (profile(i) > DailyQACBCTPhantomMinHorizontalPlaneSum_cu)
      val b = (flatness(i) < DailyQACBCTFlatnessMinimum)
      val c = hasCubeVoxels(i + (seg / 2))
      val d = {
        val img = new DicomImage(horizontalSlice(entireVolume, i))
        val x = ImageUtil.centerOfMass(img.columnSums)
        val y = ImageUtil.centerOfMass(img.rowSums)
        new Point2d(x, y)
      }
      println("isGood   " + i.formatted("%4d") +
        " minHorz: " + a +
        "    flatness: " + b +
        "    hasCubeVoxels: " + c + "    pct: " + percentOfCubeVoxels(i) +
        "    center of mass: " + d.getX.formatted("%9.3f") + " , " + d.getY.formatted("%9.3f"))
    }

    (0 until profile.size - seg).toSeq.map(i => isGood(i))
    val top = (0 until profile.size - seg).toSeq.find(i => (profile(i) > DailyQACBCTPhantomMinHorizontalPlaneSum_cu) && (flatness(i) < DailyQACBCTFlatnessMinimum) && hasCubeVoxels(i + (seg / 2)))

    val center =
      if (top.isDefined) {
        val t = top.get
        if (t < 50)
          println("top too high")
        val nextFlatnesses = (1 until seg).map(i => flatness(t + i)).map(f => f.formatted("%8.6f"))
        val cu = profile(t)
        println("==== top: " + t + "    cu: " + cu + "    flatness: " + flatness(t) + " :: " + nextFlatnesses.mkString("  "))
        val nextVals = profile.drop(t).take(seg).map(v => ((cu - v) / cu).abs).map(p => p.formatted("%9.4f"))
        println("==== nextVals: " + nextVals.mkString("  "))
        t
      } else {
        println("Could not find center")
        -1
      }
    val allFlat = (0 until profile.size - seg).toSeq.map(i => flatness(i)).map(p => p.formatted("%9.4f"))
    println("==== allFlat: " + allFlat.mkString("  "))

    center
  }

  def markCenter(buf: BufferedImage, center: Int): Unit = {
    val graphics = ImageUtil.getGraphics(buf)
    graphics.setColor(Color.white)
    if (center == -1) {
      val offset = 20
      graphics.drawOval(offset, buf.getHeight - offset, 5, 5)
    } else {
      graphics.drawLine(center, 0, center, 100)
      graphics.drawLine(center, buf.getHeight - 1, center, buf.getHeight - 10)
    }
  }

  def makeHistForAllSlices(imgDir: File, pngWidth: Int, entireVolume: DicomVolume, voxSize_mm: Point3d) = {
    val subDir = new File(imgDir, "hist")
    subDir.mkdirs
    def makeChart(y: Int) = {
      val bf = new BufferedImage(pngWidth, pngHeight, BufferedImage.TYPE_INT_RGB)
      for (x <- 0 until pngWidth; y <- 0 until pngHeight) bf.setRGB(x, y, 0) // set image to black
      val h = histogram(bf, entireVolume, y, voxSize_mm)
      addProfile(bf, Color.red, h)
      val max = horizontalSlice(entireVolume, y).flatten.max.toInt
      val fl = new File(subDir, y.formatted("%03d") + "_" + max.formatted("%05d") + ".png")
      Util.writePng(bf, fl)
    }
    (0 until entireVolume.ySize).map(y => makeChart(y))
  }

  def processCT(ctDir: File) = {
    println("=========================================== dir: " + ctDir.getAbsolutePath)
    val patOutDirName = ctDir.getParentFile.getName.replace('$', '_')
    val patOutDir = new File(outDir, patOutDirName)
    val imgDir = new File(patOutDir, ctDir.getName)

    val ctList = Util.sortByZ(ctDir.listFiles.map(ct => readDicomFile(ct)).toSeq)
    val voxSize_mm = Util.getVoxSize_mm(ctList) // the size of a voxel in mm

    val sliceStart = System.currentTimeMillis
    val imgList = ctList.map(al => new DicomImage(al))
    val entireVolume = new DicomVolume(imgList)
    val profile = (0 until entireVolume.ySize).toSeq.par.map(sliceIndex => horizontalSliceToSum(entireVolume: DicomVolume, sliceIndex)).toList
    println("Elapsed slice time: " + (System.currentTimeMillis - sliceStart))
    val cubeLen_vox = yCubeSize_vox(ctList.head)

    val pngWidth = ctList.head.get(TagFromName.Rows).getIntegerValues()(0)

    val buf = new BufferedImage(pngWidth, pngHeight, BufferedImage.TYPE_INT_RGB)
    for (x <- 0 until pngWidth; y <- 0 until pngHeight) buf.setRGB(x, y, 0) // set image to black

    // draw various lines
    addProfile(buf, Color.green, profile)

    //addProfile(buf, Color.yellow, profileToFlatness(profile, cubeLen_vox))

    //addProfile(buf, Color.red, profileToRange(profile, cubeLen_vox))

    //val percent = (0 until entireVolume.ySize).toSeq.par.map(sliceIndex => percentOfCubeVoxels2(entireVolume, sliceIndex, voxSize_mm)).toList.map(d => d.toFloat)
    //addProfile(buf, Color.lightGray, percent)

    val center = findCenter(entireVolume, profile, cubeLen_vox, voxSize_mm)
    markCenter(buf, center)
    val hist = histogram(buf, entireVolume, center + cubeLen_vox / 3, voxSize_mm)
    addProfile(buf, Color.red, hist)

    if (true) makeHistForAllSlices(imgDir, pngWidth, entireVolume, voxSize_mm)

    patOutDir.mkdirs
    val pngFile = new File(patOutDir, ctDir.getName + ".png")
    Util.writePng(buf, pngFile)
    println("Wrote file " + pngFile.getAbsolutePath)

    if (center == -1) {
      imgDir.mkdirs
      def saveHorzSlice(i: Int) = {
        val buf = (new DicomImage(horizontalSlice(entireVolume, i))).toBufferedImage(Color.white)
        val pngFile = new File(imgDir, i.formatted("%03d.png"))
        Util.writePng(buf, pngFile)
      }
      (0 until entireVolume.ySize).map(i => saveHorzSlice(i))
    }

    //val csvFile = new File(patOutDir, ctDir.getName + ".csv")

  }

  def processPatient(dir: File) = {
    println("processing patient " + dir.getName)
    val ctList = dir.listFiles.filter(dicom => dicom.getName.matches(".*_CT_.*")).sortBy(dicom => dicom.getName).takeRight(ctPerPatient).toSeq
    ctList.map(ct => processCT(ct))
  }

  def showLegend = {
    println
    println("green  : profile of horizontal plane pixel sums")
    println("yellow : stdDev/avg starting at given point for next 1/2 cube")
    println("red    : range for next 1/2 cube")
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis

    println("---------- Starting VerticalCubeProfile -------------------------------------------------------")
    showLegend
    //val dsDir = new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries""")
    val dsDir = new File("""\\uhroappwebspr1\e$\Program Files\UMRO\AQAClient\data\DICOMSeries""")

    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs

    if (!dsDir.isDirectory) {
      println("Can not access main input directory: " + dsDir.getAbsolutePath)
      System.exit(1)
    }
    val patDirList = dsDir.listFiles.filter(d => d.isDirectory && (d.getName.startsWith("$T") || d.getName.startsWith("BR1"))).toSeq

    println("patDirList: " + patDirList.map(d => d.getName).mkString("\n", "\n", "\n"))

    if (true) {
      val failList = Seq(
        """$TX4OBI2020Q2\2020-09-21T06-47-44_CT_160_1.2.246.352.61.2.5451780319018226224.16142076251553435779""")
      val failList0 = Seq(
        """$TX4OBI2020Q2\2020-09-21T06-47-44_CT_160_1.2.246.352.61.2.5451780319018226224.16142076251553435779""",
        """$TB3_OBI2020Q2\2020-08-04T07-04-16_CT_175_1.2.246.352.62.2.5362375747545364333.10472334818973089212""",
        """$TB3_OBI2020Q2\2020-08-04T07-06-04_CT_175_1.2.246.352.62.2.4916714739440032908.12839206091707120773""")
      val j = new File(dsDir, failList.head)
      failList.map(f => processCT(new File(dsDir, f)))
      System.exit(99)
    }

    patDirList.map(d => processPatient(d))

    showLegend
    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed ms : " + elapsed)

    val dateFormat = new SimpleDateFormat("m:ss.SSS")
    val formattedElapsed = {
      val d = elapsed + (5 * 60 * 60 * 1000)
      dateFormat.format(new Date(d))
    }
    println("Done.  Elapsed : " + formattedElapsed)

  }
}