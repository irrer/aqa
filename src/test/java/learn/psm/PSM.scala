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
import org.aqa.web.C3Chart
import org.aqa.webrun.phase2.wedge.WedgeHTML
import us.hebi.matlab.mat.format.Mat5

import java.awt.Rectangle
import java.io.File
import java.util.Date
import scala.xml.Elem

object PSM {

  val outDir = new File("target\\psm")

  private def middleOf(image: DicomImage): String = {

    val size = 10
    val x = (image.width - size) / 2
    val y = (image.height - size) / 2

    val rect = new Rectangle(x, y, size, size)

    val sub = image.getSubimage(rect)

    val minMaxMean = s"min: ${image.minPixelValue}    max: ${image.maxPixelValue}    mean: ${image.sum / (image.width * image.height)}"

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
        if (pixelData(y)(x) <= 0)
          (pixelData(y - 1)(x) + pixelData(y + 1)(x)) / 2
        else pixelData(y)(x)
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
    val scale = (image.maxPixelValue - image.minPixelValue) / (max - min)
    val offset = max - (min * scale)

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

  private def makeProfiles(al: AttributeList): (Elem, String) = {
    val image = new DicomImage(al)
    val xValueList = (1 to image.width).map(_.toDouble)

    val numPix = 10
    val numPixD = numPix.toDouble

    val transverse: Seq[Double] = {
      val rect = new Rectangle(0, (image.height - numPix) / 2, image.width, numPix)
      val sub = image.getSubimage(rect)
      sub.columnSums.map(p => p / numPixD)
    }

    val axial: Seq[Double] = {
      val rect = new Rectangle((image.width - numPix) / 2, 0, numPix, image.height)
      val sub = image.getSubimage(rect)
      sub.columnSums.map(p => p / numPixD)
    }

    WedgeHTML.lineColor
    val chart = new C3Chart( //
      xAxisLabel = "CU", //
      xDataLabel = "CU", //
      xValueList = xValueList, //
      yAxisLabels = Seq("Transverse", "Axial"), //
      yDataLabel = "Level", //
      yValues = Seq(transverse, axial) //
    ) //

    (chart.html, chart.javascript)
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
    val psmImage = psmToDicomImage(psmFile)

    val wdZBeam6x_FF = {
      val pixelData = beam6x_FFImage.pixelData.zip(wdImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 * pair._2))
      new DicomImage(pixelData)
    }

    val psmCorrected = {
      val pixelData = wdZBeam6x_FF.pixelData.zip(psmImage.pixelData).map(tw => tw._1.zip(tw._2).map(pair => pair._1 / pair._2))
      new DicomImage(pixelData)
    }

    val psmCorrectedAndNormalized = normalizeImage(psmImage, beam6x_FFImage.minPixelValue, beam6x_FFImage.maxPixelValue)

    val finalDicom = makeDicom(psmCorrectedAndNormalized, beam6x_FFDicomFile.attributeList.get)

    val roundTrip = new DicomImage(finalDicom)

    Trace.trace("\nwd:     " + middleOf(wdImage))
    Trace.trace("\nbeam6x_FF:     " + middleOf(beam6x_FFImage))
    Trace.trace("\nwd x beam6x_FF:     " + middleOf(wdZBeam6x_FF))
    Trace.trace("\npsm:     " + middleOf(psmImage))
    Trace.trace("\npsmCorrected:     " + middleOf(psmCorrectedAndNormalized))
    Trace.trace("\nroundTrip:     " + middleOf(roundTrip))

    savePng(wdImage, "wdImage")
    savePng(beam6x_FFImage, "beam6x_FFImage")
    savePng(wdZBeam6x_FF, "wdXbeam6x_FF")
    savePng(psmImage, "psmImage")
    savePng(psmCorrectedAndNormalized, "psmCorrectedAndNormalized")
    savePng(roundTrip, "roundTrip")

    val finalDicomFile = new File(outDir, "psmCorrectedAndNormalized.dcm")
    DicomUtil.writeAttributeListToFile(finalDicom, finalDicomFile, "PSM")

    makeProfiles(finalDicom)

    Trace.trace("Done.")
  }

}
