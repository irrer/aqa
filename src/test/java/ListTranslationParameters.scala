import java.io.File
import org.aqa.Util
import org.aqa.webrun.phase2.IsoImagePlaneTranslator

object ListTranslationParameters {
  def main(args: Array[String]): Unit = {

    val mainDirList = new File("""D:\tmp\aqa\Phase2\DICOM""")

    val dicomFileList = mainDirList.listFiles.map(f => f.listFiles).flatten.map(f => f.listFiles).flatten

    val transList = dicomFileList.map(f => Util.readDicomFile(f)).filter(ta => ta.isRight).map(ta => ta.right.get).map(al => new IsoImagePlaneTranslator(al))

    println("All translations:\n" + transList.mkString("\n"))
    println("done")
  }
}