package learn.psm

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import org.aqa.Util
import us.hebi.matlab.mat.format.Mat5

import java.awt.Rectangle
import java.io.File
import java.util.Date

object PSM {

  val outDir = new File("target\\psm")

  private def middleOf(image: DicomImage): String = {

    //   This is the middle of the image
    val size = 10
    val x = (image.width - size) / 2
    val y = (image.height - size) / 2

    val rect = new Rectangle(x, y, size, size)

    val sub = image.getSubimage(rect)

    sub.pixelsToText
  }

  private def psmToDicomImage(psmFile: File): DicomImage = {
    val entry = Mat5.readFromFile(psmFile).getEntries.iterator().next()
    val dimensions = entry.getValue.getDimensions
    val width = dimensions(0)
    val height = dimensions(1)
    val matrix = Mat5.readFromFile(psmFile).getMatrix("PSM")

    def toRow(y: Int): IndexedSeq[Float] = {
      val aa = (0 until width).map(x => {
        matrix.getDouble(y, x).toFloat // Note: The email sent to Michael Barnes on 2023 Dec 14 used y,x
      })
      aa
    }

    val pixelData = (0 until height).map(toRow)

    new DicomImage(pixelData)
  }

  /**
    * Normalize the given image so that the mean of the central pixels is 1.0.
    * @param brImage Image to normalize.
    * @return A normalized version of the calculated image.
    */

  private def normalizeImage(brImage: DicomImage): DicomImage = {
    val centerSize = 10
    val x = (brImage.width - centerSize) / 2
    val y = (brImage.height - centerSize) / 2
    val rectangle = new Rectangle(x, y, centerSize, centerSize)
    val centerImage = brImage.getSubimage(rectangle)

    val mean = centerImage.sum / (centerSize * centerSize)

    val scale = 1 / mean

    //val pixelData = ffImage.pixelData.zip(wdImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
    def rowOf(y: Int): IndexedSeq[Float] = {
      for (x <- 0 until brImage.width) yield brImage.get(x, y) * scale
    }

    val pixelData = for (y <- 0 until brImage.height) yield rowOf(y)
    new DicomImage(pixelData)
  }

  private def savePng(dicomImage: DicomImage, name: String): File = {
    val sorted = dicomImage.pixelData.flatten.sorted
    val min = sorted.drop(5).head
    val max = sorted.dropRight(5).last

    val buf = dicomImage.toDeepColorBufferedImage(min, max)
    val pngFile = new File(outDir, FileUtil.replaceInvalidFileNameCharacters(name, '_') + ".png")
    Util.writePng(buf, pngFile)
    println("Wrote file " + pngFile.getAbsolutePath)
    pngFile
  }

  private def makeDicom(image: DicomImage, template: AttributeList): AttributeList = {
    val al = DicomUtil.clone(template)

    def replace(tag: AttributeTag, value: String): Unit = {
      DicomUtil
        .findAllSingle(al, tag)
        .foreach(attr => {
          attr.removeValues()
          attr.addValue(value)
        })
    }

    val instanceUid = edu.umro.util.UMROGUID.getUID
    val seriesUid = edu.umro.util.UMROGUID.getUID
    val now = new Date()
    val dateText = DicomUtil.dicomDateFormat.format(now)
    val timeText = DicomUtil.dicomTimeFormat.format(now)

    replace(TagByName.MediaStorageSOPClassUID, instanceUid)
    replace(TagByName.SOPInstanceUID, instanceUid)
    replace(TagByName.SeriesInstanceUID, seriesUid)

    replace(TagByName.InstanceCreationDate, dateText)
    replace(TagByName.ContentDate, dateText)
    replace(TagByName.AcquisitionDate, dateText)
    replace(TagByName.SeriesDate, dateText)

    replace(TagByName.InstanceCreationTime, timeText)
    replace(TagByName.ContentTime, timeText)
    replace(TagByName.AcquisitionTime, timeText)
    replace(TagByName.SeriesTime, timeText)

    val pix = al.get(TagByName.PixelData).asInstanceOf[OtherWordAttribute]
    pix.removeValues()

    val shortArray = image.pixelData.flatten.map(p => p.round.toShort).toArray

    pix.setValues(shortArray)
    al
  }

  private case class Column(name: String, valueList: Seq[Float]) {}

  private def makeProfiles(name: String, image: DicomImage): Seq[Column] = {

    val numPix = 1
    val numPixD = numPix.toFloat

    val transverse = {
      val v = image.columnSums.map(p => p / image.height)
      Column(s"$name Transverse Full Image", v)
    }

    val transverseCenter = {
      val rect = new Rectangle(0, (image.height - numPix) / 2, image.width, numPix)
      val sub = image.getSubimage(rect)
      val v = sub.columnSums.map(p => p / numPixD)
      Column(s"$name Transverse Center $numPix Pixels", v)
    }

    val axial = {
      val v = image.rowSums.map(p => p / image.width)
      Column(s"$name Axial Full Image", v)
    }

    val axialCenter = {
      val rect = new Rectangle((image.width - numPix) / 2, 0, numPix, image.height)
      val sub = image.getSubimage(rect)
      val v = sub.rowSums.map(p => p / numPixD)
      Column(s"$name Axial Center $numPix Pixels", v)
    }

    val histogram = image.binnedHistogram(image.width)

    val histogramCount = Column(s"$name Histogram Count", histogram.map(_.count.toFloat))
    val histogramValue = Column(s"$name Histogram Value", histogram.map(_.value))

    Seq(transverse, transverseCenter, axial, axialCenter, histogramValue, histogramCount)
    // Seq(histogramValue, histogramCount) // just show histograms
  }

  private case class NamedImage(name: String, image: DicomImage) {}

  private def makeCsv(namedImageList: Seq[NamedImage]): Unit = {

    val all = namedImageList.flatMap(ni => makeProfiles(ni.name, ni.image))

    def showStats(ni: NamedImage): Unit = {

      def fmtD(d: Double): String = "%16.3f".format(d)
      def fmtF(f: Float): String = "%16.3f".format(f)

      val valueList = ni.image.pixelData.flatten.filter(_ > 0).sorted
      val mean = valueList.sum / valueList.size
      val stdDev = ImageUtil.stdDev(valueList)
      val range = valueList.last - valueList.head
      val distinctCount = valueList.distinct.size
      val median = valueList(valueList.size / 2)
      val q1 = valueList(valueList.size / 4)
      val q3 = valueList((valueList.size * 3) / 4)
      val q1_10 = valueList(valueList.size / 10)
      val q9_10 = valueList((valueList.size * 9) / 10)

      val numEnd = 5

      val text = {
        s"""Statistics:  ${"%24s".formatted(ni.name)}    mean: ${fmtD(mean)}    stdDev: ${fmtD(stdDev)}    median: ${fmtD(median)}""" +
          s"""    q1: ${fmtD(q1)}    q3: ${fmtD(q3)}    q1_10: ${fmtD(q1_10)}    q9_10: ${fmtD(q9_10)}""" +
          s"""    range: $range    distinct count: $distinctCount""" +
          s"""    min: ${valueList.take(numEnd).map(fmtF).mkString("  ")}    max: ${valueList.takeRight(numEnd).map(fmtF).mkString("  ")}"""
      }
      println(text)
    }

    namedImageList.foreach(showStats)

    val maxLine = all.map(_.valueList.size).max

    val csvText = {

      val header: String = all.map(_.name).mkString(",")

      val data: String = {

        def toText(col: Column, line: Int): String = {
          if (col.valueList.size > line) {
            val value = col.valueList(line)
            if (value.round == value)
              value.round.toString
            else {
              value.toString
            }
          } else
            "" // no value for this line
        }

        def makeRow(line: Int): String = all.map(col => toText(col, line)).mkString(",")

        val rowText = (0 until maxLine).map(makeRow).mkString("\n")
        rowText
      }

      s"$header\n$data"
    }

    val psmCsvFile = new File(outDir, "PSM.csv")
    Util.writeFile(psmCsvFile, csvText)
    println(s"Wrote file ${psmCsvFile.getAbsolutePath}")
  }

  private def makePixText(namedImageList: Seq[NamedImage]): Unit = {

    def imageToPixText(ni: NamedImage): Unit = {
      val fileName = FileUtil.replaceInvalidFileNameCharacters(ni.name, '_').replace(' ', '_') + "_Pixels.txt"
      val file = new File(outDir, fileName)
      val text = ni.image.pixelsToText
      Util.writeFile(file, text)
      println(s"Wrote pixel text file: ${file.getAbsolutePath}")
    }

    namedImageList.foreach(imageToPixText)
  }

  private def saveDicomAsText(dicomFile: DicomFile, name: String): Unit = {
    val text = DicomUtil.attributeListToString(dicomFile.attributeList.get)
    val outFile = new File(outDir, s"$name.txt")
    Util.writeFile(outFile, text)
    println("Wrote file " + outFile.getAbsolutePath)
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting ----------------------------------------------------------------------------")
    val start = System.currentTimeMillis()
    //noinspection SpellCheckingInspection
    val dir = new File("""src\test\resources\learnPSM""")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs

    val wdDicomFile = new DicomFile(new File(dir, "RI.zzz_EpidPerformanceCheck.WD-0.dcm"))
    val ffDicomFile = new DicomFile(new File(dir, "6x_FF.dcm"))

    saveDicomAsText(wdDicomFile, "WD")
    saveDicomAsText(ffDicomFile, "FF")

    val psmFile = new File(dir, "PSM_6x_CMN_230520.mat")

    println("\n---------------------\n")

    val wdImage = new DicomImage(wdDicomFile.attributeList.get)

    val ffImage = new DicomImage(ffDicomFile.attributeList.get)

    savePng(ffImage, "FF_original")

    val psmImage = psmToDicomImage(psmFile)

    savePng(psmImage, "PSM_original")

    val rawImage = {
      //val pixelData = ffImage.pixelData.zip(wdImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
      def rowOf(y: Int): IndexedSeq[Float] = {
        for (x <- 0 until wdImage.width) yield {
          val jf = ffImage.get(x, y)
          val jw = wdImage.get(x, y)

          if ((jf < 1) || (jw < 1)) {
            val jj = jf * jw
            Trace.trace(s"ff: $jf  *  wf: $jw  =  $jj")
          }
          val p = ffImage.get(x, y) * wdImage.get(x, y)
          p
        }
      }
      val pixelData = for (y <- 0 until wdImage.height) yield rowOf(y)
      new DicomImage(pixelData)
    }

    /**
     * normalize((ff * wd) / psm)
     */

    val brImage = {
      val zero: Float = 0
      //val pixelData = ffImage.pixelData.zip(wdImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
      def rowOf(y: Int): IndexedSeq[Float] = {
        for (x <- 0 until wdImage.width) yield {
          val a = rawImage.get(x, y)
          val b = psmImage.get(x, y)

          if (x == 213) {
            Trace.trace(s"wd x ff: $a  /  psm: $b")
          }

          if ((a == 0) || (b == 0)) {
            Trace.trace(s"wd x ff: $a  /  psm: $b")
            zero
          } else {
            val p = rawImage.get(x, y) / psmImage.get(x, y)
            p
          }
        }
      }
      val pixelData = for (y <- 0 until rawImage.height) yield rowOf(y)
      new DicomImage(pixelData)
    }

    val brNorm = normalizeImage(brImage)

    val brNormDicom = makeDicom(brNorm, ffDicomFile.attributeList.get)

    val roundTrip = new DicomImage(brNormDicom)

    println("\nFF:     " + middleOf(ffImage))
    println("\nWD:     " + middleOf(wdImage))
    println("\nRaw:     " + middleOf(rawImage))
    println("\nPSM:     " + middleOf(psmImage))
    println("\nBR:     " + middleOf(brImage))
    println("\nBR normalized:     " + middleOf(brNorm))
    println("\nroundTrip:     " + middleOf(roundTrip))

    savePng(wdImage, "WD")
    savePng(ffImage, "FF")
    savePng(rawImage, "Raw")
    savePng(psmImage, "PSM")
    savePng(brImage, "BR")
    savePng(brNorm, "BRNorm")
    savePng(roundTrip, "round_trip")

    val namedImageList = Seq(
      NamedImage("FF", ffImage),
      NamedImage("WD", wdImage),
      NamedImage("Raw", rawImage),
      NamedImage("BR", brImage),
      NamedImage("BRNorm", brNorm),
      NamedImage("PSM", psmImage)
    )

    makeCsv(namedImageList)
    makePixText(namedImageList)

    val brNormDicomFile = new File(outDir, "BRNorm.dcm")
    DicomUtil.writeAttributeListToFile(brNormDicom, brNormDicomFile, "PSM")

    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Done.  Elapsed ms: $elapsed")
  }

}
