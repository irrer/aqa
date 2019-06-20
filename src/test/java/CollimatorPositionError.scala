
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
import org.aqa.DicomFile
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ImageUtil.DicomImage

object CollimatorPositionError {

  def main(args: Array[String]): Unit = {
    println("starting")

    val file = new File("""D:\pf\eclipse\workspaceOxygen\aqa\target\CollimatorPositionError.dcm""")
    val df = new DicomFile(file)
    val di = new DicomImage(df.attributeList.get)

    val cols = di.columnSums

    println("\n" + cols.mkString("\n"))
    
    

    println("finished")
  }

}