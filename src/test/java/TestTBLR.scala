
import scala.util.Random
import edu.umro.ImageUtil.DicomImage
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.IsoImagePlaneTranslator
import java.awt.Point
import org.aqa.Util

object TestTBLR {

  def main(args: Array[String]): Unit = {

    println("Starting")

    val dir = new File("""D:\tmp\aqa\msk_wl""")
    val dicomFile090 = new DicomFile(new File(dir, "aqa-check-090.dcm"))
    val dicomFile270 = new DicomFile(new File(dir, "aqa-check-270.dcm"))
    val image090 = new DicomImage(dicomFile090.attributeList.get)
    val image270 = new DicomImage(dicomFile270.attributeList.get)

    val translator090 = new IsoImagePlaneTranslator(dicomFile090.attributeList.get)
    val translator270 = new IsoImagePlaneTranslator(dicomFile270.attributeList.get)

    val pointZero = new Point(0, 0)
    val edgePercent = 0.5
    val expected = new MeasureTBLREdges.TBLR(592, 686, 598, 685).pix2iso(translator090)
    val tblr090 = MeasureTBLREdges.measure(image090, translator090, Some(expected), 90, image090, pointZero, edgePercent)
    val tblr270 = MeasureTBLREdges.measure(image270, translator270, Some(expected), 270, image270, pointZero, edgePercent)

    println("------------------------------------------------------------------")
    println("tblr090 iso: " + tblr090.measurementSet.pix2iso(translator090) + "    center: " + tblr090.measurementSet.pix2iso(translator090).center)
    println("tblr270 iso: " + tblr270.measurementSet.pix2iso(translator270) + "    center: " + tblr270.measurementSet.pix2iso(translator270).center)

    val png090 = new File(dir, "090.png")
    png090.delete
    Util.writePng(tblr090.bufferedImage, png090)

    val png270 = new File(dir, "270.png")
    png270.delete
    Util.writePng(tblr270.bufferedImage, png270)

    println("Done.  Images in " + dir.getAbsolutePath)

  }

}