import java.io.File
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList

object MakeCBCT {

  def main(args: Array[String]): Unit = {

    // offsets from center of volume
    val xOffset = 10
    val yOffset = 20
    val zOffset = 4

    val inDir = new File("""src\test\resources\TestCBCTAlign\TX2_CT_1""")
    println("Using input direcory: " + inDir.getAbsolutePath)
    val outDir = new File("""src\test\resources\TestCBCTAlign\CBCT_ONE_VOXEL""")
    outDir.mkdirs
    outDir.listFiles.map(f => f.delete)

    def getPosn(al: AttributeList): Double = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

    val attrListSeq = inDir.listFiles.map(f => (new DicomFile(f)).attributeList.get).toSeq.sortBy(al => getPosn(al))

    val xSize = attrListSeq.head.get(TagFromName.Columns).getIntegerValues()(0)
    val ySize = attrListSeq.head.get(TagFromName.Rows).getIntegerValues()(0)
    val zSize = attrListSeq.size

    val x = (xSize / 2) + xOffset
    val y = (ySize / 2) + yOffset
    val z = (zSize / 2) + zOffset

    def makeFile(index: Int) = {
      val al = attrListSeq(index)
      val instNo = al.get(TagFromName.InstanceNumber).getIntegerValues().head

      val pixAttr = (al.get(TagFromName.PixelData)) //.asInstanceOf[OtherWordAttribute]
      val pixels = pixAttr.getShortValues
      (0 until pixels.size).map(i => pixels(i) = 100.toShort)
      //      Trace.trace(pixels.size)
      //      val empty = Array.fill(pixels.size)(1000.toShort)
      if (index == (z - 1)) {
        val columns = al.get(TagFromName.Columns).getIntegerValues().head
        pixels((y * columns) + x) = 10000.toShort
      }
      //      pixAttr.setValues(empty)

      val name = ("CBCT_" + (index + 1).formatted("%3d") + ".dcm").replace(' ', '_')
      val file = new File(outDir, name)
      DicomUtil.writeAttributeList(al, file, "AQA")
      println("wrote: " + file.getAbsolutePath)
    }

    (0 until attrListSeq.size).map(index => makeFile(index))

  }

}