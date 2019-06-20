
import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TagFromName

object PixelRow {

  def main(args: Array[String]): Unit = {
    println("starting")

    val file = new File("""D:\downloads\proffy.dcm""")

    val al = new AttributeList
    al.read(file)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val pixelData = al.get(TagFromName.PixelData).getShortValues

    val bound = 40
    val x = 395
    val y = 718

    val lo = (width * y) + x - bound
    val hi = (width * y) + x + bound

    def show(p: Int) = {
      println(pixelData(p))
    }

    (lo to hi).map(p => show(p))

    println("finished")
  }

}