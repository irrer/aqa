
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

object ListDICOM {

    class ReadStrategy extends ReadTerminationStrategy {
        override def terminate(attributeList: AttributeList, tag: AttributeTag, bytesRead: Long): Boolean = {
            val done =
                (tag.getGroup >= TagFromName.InstanceNumber.getGroup) &&
                    (tag.getElement > TagFromName.InstanceNumber.getElement)
            done
        }
    }

    val readStrategy = new ReadStrategy

    val dir = new File("""D:\tmp\velocity_crypt\AAAA\originals\encrypted\jj""")

    def readDicom(dicomFile: File) = {
        val al = new AttributeList
        al.read(dicomFile, readStrategy)
        al
    }

    def process(al: AttributeList) = {
        val OtherPatientID = al.get(TagFromName.OtherPatientIDs).getSingleStringValueOrNull
        val InstanceNumber = al.get(TagFromName.InstanceNumber).getSingleStringValueOrNull.toInt.formatted("%02d")
        val ReferringPhysicianName = al.get(TagFromName.ReferringPhysicianName).getSingleStringValueOrNull
        println(OtherPatientID + " " + InstanceNumber + " : " + ReferringPhysicianName)
    }

    def main(args: Array[String]): Unit = {
        println("starting")

        dir.listFiles.map(f => process(readDicom(f)))

        println("finished")
    }

}