package learn.psm

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import org.aqa.Util
import us.hebi.matlab.mat.format.Mat5

import java.awt.Rectangle
import java.io.File
import java.util.Date
import scala.xml.Elem

object PSM {

  val outDir = new File("target\\psm")

  private def middleOf(image: DicomImage): String = {

    val size = 10
    // val x = (image.width - size) / 2
    // val y = (image.height - size) / 2

    val x = 0
    val y = 0

    val rect = new Rectangle(x, y, size, size)

    val sub = image.getSubimage(rect)

    val distinctCount = image.pixelData.flatten.groupBy(p => p).size

    val minMaxMean = s"min: ${image.minPixelValue}    max: ${image.maxPixelValue}    mean: ${image.sum / (image.width * image.height)}    number of distinct values: $distinctCount"

    "\n" + minMaxMean + sub.pixelsToText
  }

  private def psmToDicomImage(psmFile: File): DicomImage = {
    val entry = Mat5.readFromFile(psmFile).getEntries.iterator().next()
    val dimensions = entry.getValue.getDimensions
    val width = dimensions(0)
    val height = dimensions(1)
    val matrix = Mat5.readFromFile(psmFile).getMatrix("PSM")

    if (true) {
      for (y <- 0 until 10) {
        print("\ny: " + y + "  ")
        for (x <- 0 until 10) {
          val text = matrix.getDouble(x, y).formatted("%4.4f")
          print(text + "  ")
        }
      }

      // System.exit(99)
    }

    def toRow(y: Int): IndexedSeq[Float] = {
      val aa = (0 until width).map(x => {
        matrix.getDouble(x, y).toFloat
      })
      aa
    }

    val pixelData = (0 until height).map(toRow)

    def fix(y: Int): IndexedSeq[Float] = {
      (0 until width).map(x => {
        if (pixelData(y)(x) <= 0) {
          val l = pixelData(y - 1)(x)
          val h = pixelData(y + 1)(x)
          (pixelData(y - 1)(x) + pixelData(y + 1)(x)) / 2
        } else pixelData(y)(x)
      })
    }
    val fixed = (0 until height).map(fix)

    new DicomImage(fixed)
  }

  /**
    * Normalize the given image so that all of the values will be in the range min -> max.
    * @param image Image to normalize.
    * @param min Min pixel value from a 'regular' RTIMAGE file.
    * @param max Max pixel value from a 'regular' RTIMAGE file.
    * @return
    */
  private def normalizeImage(image: DicomImage, min: Float, max: Float): DicomImage = {
    val scale = (max - min) / (image.maxPixelValue - image.minPixelValue)
    val offset = max - (image.maxPixelValue * scale)

    val pixelDataNormalized = image.pixelData.map(row => row.map(p => (p * scale) + offset))

    new DicomImage(pixelDataNormalized)
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

  private def centerPixelValues(image: DicomImage): Elem = {

    val size = 10
    val x = (image.width - size) / 2
    val y = (image.height - size) / 2

    val rect = new Rectangle(x, y, size, size)
    val sub = image.getSubimage(rect)

    <div>
    <h4>Center Pixel values</h4>
      <pre>
        {sub.pixelsToText}
      </pre>
    </div>
  }

  private def show(al: AttributeList, description: String): Elem = {
    val image = new DicomImage(al)
    val pngFile = savePng(image, description)

    <div>
      <h3>{description}</h3>
      Min pixel value: {image.minPixelValue}
      <br/>
      Max pixel value: {image.maxPixelValue}
      <br/>
      Mean pixel value: {image.pixelData.flatten.sum / (image.width * image.height)}
      <br/>
      {centerPixelValues(image)}
      <br/>
      <img src={pngFile.getName} />
    </div>
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

    val j = al.get(TagByName.PixelData)
    val pix = al.get(TagByName.PixelData).asInstanceOf[OtherWordAttribute]
    pix.removeValues()

    val shortArray = image.pixelData.flatten.map(p => p.round.toShort).toArray

    pix.setValues(shortArray)
    al
  }

  private case class Column(name: String, valueList: Seq[Float]) {}

  private def makeProfiles(al: AttributeList): Seq[Column] = {
    val image = new DicomImage(al)

    val numPix = 10
    val numPixD = numPix.toFloat

    val transverse = {
      val v = image.columnSums.map(p => p / image.height)
      Column("Transverse Full Image", v)
    }

    val transverseCenter = {
      val rect = new Rectangle(0, (image.height - numPix) / 2, image.width, numPix)
      val sub = image.getSubimage(rect)
      val v = sub.columnSums.map(p => p / numPixD)
      Column(s"Transverse Center $numPix Pixels", v)
    }

    val axial = {
      val v = image.rowSums.map(p => p / image.width)
      Column("Axial Full Image", v)
    }

    val axialCenter = {
      val rect = new Rectangle((image.width - numPix) / 2, 0, numPix, image.height)
      val sub = image.getSubimage(rect)
      val v = sub.rowSums.map(p => p / numPixD)
      Column(s"Axial Center $numPix Pixels", v)
    }

    val histogram = image.binnedHistogram(image.width)

    val histogramValue = Column("Histogram Value", histogram.map(_.value))
    val histogramCount = Column("Histogram Count", histogram.map(_.count.toFloat))

    Seq(transverse, transverseCenter, axial, axialCenter, histogramCount, histogramValue)
  }

  /*
    val xValueList = (1 to image.width).map(_.toDouble)
    val chart = new C3Chart( //
      xAxisLabel = "CU", //
      xDataLabel = "CU", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Transverse", "Axial"), //
      yDataLabel = "Level", //
      yValues = Seq(transverse, axial) //
    ) //
   */

  private def makeCsv(beforeAl: AttributeList, afterAl: AttributeList): Unit = {

    val before = makeProfiles(beforeAl)
    val after = makeProfiles(afterAl)

    val csvText = {
      before.map(b => "Before " + b.name).mkString(",")
      after.map(b => "After " + b.name).mkString(",")

      val header: String = before + "," + after

      val data: String = {
        val size = before.head.valueList.size
        val both = before ++ after

        def makeRow(index: Int): String = {
          def toText(value: Float): String = {
            if (value.round == value)
              value.round.toString
            else
              value.toString
          }
          both
            .map(c =>
              toText(
                {
                  Trace.trace(s"$index   ${c.name}  ${c.valueList.size}")
                  c.valueList(index)
                }
              )
            )
            .mkString(",")
        }

        val rowText = (0 until size).map(makeRow).mkString(",")
        rowText
      }

      s"$header,$data"
    }

    val psmCsvFile = new File(outDir, "PSM.csv")
    Util.writeFile(psmCsvFile, csvText)
    println(s"Wrote file ${psmCsvFile.getAbsolutePath}")

  }

  private def fixZeroes(image: DicomImage): DicomImage = {
    def makeRow(y: Int): IndexedSeq[Float] = {
      def fixPixel(x: Int): Float = {
        val p = image.get(x, y)
        if (p <= 0) {
          val mean = (image.get(x - 1, y) + image.get(x + 1, y)) / 2
          mean
        } else
          p
      }

      val row = (0 until image.width).map(fixPixel)
      row
    }
    val pixelData = (0 until image.height).map(makeRow)
    new DicomImage(pixelData)
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting ----------------------------------------------------------------------------")
    val dir = new File("""src\test\resources\learnPSM""")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs

    val wdDicomFile = new DicomFile(new File(dir, "RI.zzz_EpidPerformanceCheck.WD-0.dcm"))
    val beam6x_FFDicomFile = new DicomFile(new File(dir, "6x_FF.dcm"))

    val psmFile = new File(dir, "PSM_6x_CMN_230520.mat")

    println("\n---------------------\n")

    val wdImage = new DicomImage(wdDicomFile.attributeList.get)
    val beam6x_FFImage = new DicomImage(beam6x_FFDicomFile.attributeList.get)

    if (true) {
      for (x <- 0 until 1190)
        for (y <- 0 until 1190) {
          val p = beam6x_FFImage.get(x, y)
          if (p <= 0) {
            Trace.trace(s"zero pixel at x: $x    y: $y")
          }
        }
    }

    val psmImage = psmToDicomImage(psmFile)

    val wdXBeam6x_FF = {
      val pixelData = beam6x_FFImage.pixelData.zip(wdImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
      new DicomImage(pixelData)
    }

    val wdXbeam6x_ffDivPSM = {
      def div(pair: (Float, Float)): Float = {
        val result = pair._1 / pair._2
        val d1 = pair._1.toDouble
        val d2 = pair._2.toDouble
        val d = d1 / d2
        val diffD = result - d
        val f: Float = d.toFloat
        val diffF = result - f
        result
      }
      val pixelData = wdXBeam6x_FF.pixelData.zip(psmImage.pixelData).map(tw => tw._1.zip(tw._2).map(div))
      new DicomImage(pixelData)
    }

    val wdXbeam6x_ffDivPSMNorm = normalizeImage(wdXbeam6x_ffDivPSM, beam6x_FFImage.minPixelValue, beam6x_FFImage.maxPixelValue)

    val finalDicom = makeDicom(wdXbeam6x_ffDivPSMNorm, beam6x_FFDicomFile.attributeList.get)

    val roundTrip = new DicomImage(finalDicom)

    println("\nbeam6x_FF:     " + middleOf(beam6x_FFImage))
    println("\nwd:     " + middleOf(wdImage))
    println("\nwd x beam6x_FF:     " + middleOf(wdXBeam6x_FF))
    println("\npsm:     " + middleOf(psmImage))
    println("\n(wd x beam6x_FF) / psm:     " + middleOf(wdXbeam6x_ffDivPSM))
    println("\n(wd x beam6x_FF) / psm and then normalized:     " + middleOf(wdXbeam6x_ffDivPSMNorm))
    println("\nroundTrip:     " + middleOf(roundTrip))

    savePng(wdImage, "wdImage")
    savePng(beam6x_FFImage, "beam6x_FFImage")
    savePng(wdXBeam6x_FF, "wdXbeam6x_FF")
    savePng(psmImage, "psmImage")
    savePng(wdXbeam6x_ffDivPSMNorm, "psmCorrectedAndNormalized")
    savePng(roundTrip, "roundTrip")

    // makeCsv(beam6x_FFDicomFile.attributeList.get, finalDicom)

    val finalDicomFile = new File(outDir, "psmCorrectedAndNormalized.dcm")
    DicomUtil.writeAttributeListToFile(finalDicom, finalDicomFile, "PSM")

    makeProfiles(finalDicom)

    Trace.trace("Done.")
  }

}
