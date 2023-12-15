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

    val x = 95
    val y = 63

    val rect = new Rectangle(x, y, 15, 10)

    val sub = image.getSubimage(rect)

    val distinct = image.pixelData.flatten.distinct.sorted

    val withoutEnds = {
      val toDrop = 100
      // list of all pixels with the highs and lows removed.
      val list = image.pixelData.flatten.sorted.dropRight(toDrop).dropRight(toDrop)
      val min = if (list.nonEmpty) list.head else -1
      val max = if (list.nonEmpty) list.last else -1
      val mean: Float = if (list.nonEmpty) list.sum / list.size else -1
      val distinct: Float = if (list.nonEmpty) list.distinct.size else -1
      s"without: min: $min    max: $max    mean: $mean    distinct count: $distinct"
    }
    val minMaxMean =
      s"         min: ${image.minPixelValue}    max: ${image.maxPixelValue}    mean: ${image.sum / (image.width * image.height)}    distinct count: ${distinct.size}\n$withoutEnds"

    // "\n" + minMaxMean +
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
        matrix.getDouble(x, y).toFloat
      })
      aa
    }

    val pixelData = (0 until height).map(toRow)

    /*
    def fix(y: Int): IndexedSeq[Float] = {
      (0 until width).map(x => {
        if (false) { // fixes 0 pixels
          if (pixelData(y)(x) <= 0) {
            val l = pixelData(y - 1)(x)
            val h = pixelData(y + 1)(x)
            (pixelData(y - 1)(x) + pixelData(y + 1)(x)) / 2
          } else
            pixelData(y)(x)
        }
        pixelData(y)(x)
      })
    }
    val fixed = (0 until height).map(fix)
    new DicomImage(fixed)
     */

    new DicomImage(pixelData)
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

  private def makeProfiles(image: DicomImage): Seq[Column] = {

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

    val histogramCount = Column("Histogram Count", histogram.map(_.count.toFloat))
    val histogramValue = Column("Histogram Value", histogram.map(_.value))

    Seq(transverse, transverseCenter, axial, axialCenter, histogramValue, histogramCount)
  }

  private def makeCsv(beforeImage: DicomImage, afterImage: DicomImage): Unit = {

    val before = makeProfiles(beforeImage)
    val after = makeProfiles(afterImage)
    val both = before ++ after

    val maxLine = both.map(_.valueList.size).max

    val csvText = {
      val beforeHeader = before.map(b => "Before " + b.name)
      val afterHeader = after.map(b => "After " + b.name)

      val header: String = (beforeHeader ++ afterHeader).mkString(",")

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

        def makeRow(line: Int): String = both.map(col => toText(col, line)).mkString(",")

        val rowText = (0 until maxLine).map(makeRow).mkString("\n")
        rowText
      }

      s"$header\n$data"
    }

    val psmCsvFile = new File(outDir, "PSM.csv")
    Util.writeFile(psmCsvFile, csvText)
    println(s"Wrote file ${psmCsvFile.getAbsolutePath}")
  }

  private def fixPixels(image: DicomImage, isOk: Float => Boolean): DicomImage = {

    def fixPixel(x: Int, y: Int): Float = {
      val p = image.get(x, y)
      if (!isOk(p)) {
        val adjacentPixels = {
          for (
            xx <- x - 1 to x + 1; // pixels to left and right
            yy <- y - 1 to y + 1; // pixels above and below
            if //
            (xx >= 0) && // xx coordinate is valid
              (yy >= 0) && // yy coordinate is valid
              (xx < image.width) && // xx coordinate is valid
              (yy < image.height) && // yy coordinate is valid
              (image.get(xx, yy) > 0) // adjacent pixel is not also zero
          ) //
            yield image.get(xx, yy)
        }

        val mean = adjacentPixels.sum / adjacentPixels.size // mean of surrounding valid pixels
        mean
      } else
        p
    }

    def makeRow(y: Int): IndexedSeq[Float] = {
      val row = (0 until image.width).map(x => fixPixel(x, y))
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
    val beam6x_FFImage = {
      val img = new DicomImage(beam6x_FFDicomFile.attributeList.get)
      savePng(img, "beam6x_FF_original")
      fixPixels(img, x => x > 0)
    }

    val psmImage = {
      val img = psmToDicomImage(psmFile)
      savePng(img, "psm_original")
      fixPixels(img, x => x > 0)
    }

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

      val raw = new DicomImage(pixelData) // jjjjjjjjjjj
      val fixed = fixPixels(raw, x => x < 100)
      fixed
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

    savePng(wdImage, "wd")
    savePng(beam6x_FFImage, "beam6x_FF")
    savePng(wdXBeam6x_FF, "wdXbeam6x_FF")
    savePng(psmImage, "psm")
    savePng(wdXbeam6x_ffDivPSMNorm, "psmCorrectedAndNormalized")
    savePng(roundTrip, "roundTrip")

    makeCsv(beam6x_FFImage, wdXbeam6x_ffDivPSMNorm)

    val finalDicomFile = new File(outDir, "psmCorrectedAndNormalized.dcm")
    DicomUtil.writeAttributeListToFile(finalDicom, finalDicomFile, "PSM")

    Trace.trace("Done.")
  }

}
