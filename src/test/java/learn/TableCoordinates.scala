package learn

/**
 * All database code goes into the DAO (data access object) class which
 * is parameterized by a Slick driver that implements JdbScProfile.
 */

import FooFoo.driver.api._
import java.io.File
import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import javax.vecmath.Point3d
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName

object TableCoordinates {

  //  val dicomDir = new File("""\\hitsdv\e$\Program Files\UMRO\AQAClient\data\DICOMSeries""")
  //  val machDir = new File(dicomDir, "$TX1OBI2020Q2")
  //  val date = "2020-06-15"
  //
  //  val allDirs = machDir.listFiles.toSeq.filter(d => d.getName.startsWith(date))
  //  val ctDirs = allDirs.filter(d => d.getName.contains("_CT_"))
  //  val regDirs = allDirs.filter(d => d.getName.contains("_REG_"))
  //  val rtimageDirs = allDirs.filter(d => d.getName.contains("_RTIMAGE_"))

  def getImageReg = {
    val regFileName = """\\hitsdv\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TX1OBI2020Q2\2020-06-15T06-31-59_REG_1_1.2.246.352.62.2.5063023867019209376.13256553811924102041\1.2.246.352.62.5.5686846128965199793.13001658022355285390.dcm"""
    val regFile = (new File(regFileName))
    val regAl = new AttributeList
    regAl.read(regFile)
    val ir = new ImageRegistration(regAl)
    ir
  }

  def getCt = {
    val ctFileName = """\\hitsdv\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TX1OBI2020Q2\2020-06-15T06-29-33_CT_88_1.2.246.352.62.2.5754391385521110772.11373196643095964862\1.2.246.352.62.1.4646407397459328658.9861597818667490747.dcm"""
    val ctFile = new File(ctFileName)
    val ctAl = new AttributeList
    ctAl.read(ctFile)
    ctAl
  }

  def getRtimage = {
    val rtimageFileName = """\\hitsdv\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TX1OBI2020Q2\2020-06-15T06-33-43_RTIMAGE_2_1.2.246.352.62.2.5145009978551565169.12725330101551973816\1.2.246.352.62.1.5754042777995612105.6647414279741330823.dcm"""
    val rtimageFile = new File(rtimageFileName)
    val rtimageAl = new AttributeList
    rtimageAl.read(rtimageFile)
    rtimageAl
  }

  def main(args: Array[String]): Unit = {
    val imageReg = getImageReg
    val ctAl = getCt
    val rtimageAl = getRtimage

    def invTransform(point: Point3d) = {
      val inv = imageReg.getMatrix
      inv.invert
      val pt = point.clone.asInstanceOf[Point3d]
      inv.transform(pt)
      pt
    }

    val ctTableHeight = ctAl.get(TagFromName.TableHeight).getDoubleValues().head / 10
    val ctTableTopVerticalPosition = -ctTableHeight
    val ctTableTopLongitudinalPosition = ctAl.get(TagByName.TableTopLongitudinalPosition).getDoubleValues().head / 10
    val ctTableTopLateralPosition = ctAl.get(TagByName.TableTopLateralPosition).getDoubleValues().head / 10

    println("ctTableHeight                       : " + ctTableHeight)
    println
    println("ctTableTopVerticalPosition          : " + ctTableTopVerticalPosition)
    println("ctTableTopLongitudinalPosition      : " + ctTableTopLongitudinalPosition)
    println("ctTableTopLateralPosition           : " + ctTableTopLateralPosition)
    println

    val rtimageTableTopVerticalPosition = rtimageAl.get(TagByName.TableTopVerticalPosition).getDoubleValues().head / 10
    val rtimageTableHeight = -rtimageTableTopVerticalPosition
    val rtimageTableTopLongitudinalPosition = rtimageAl.get(TagByName.TableTopLongitudinalPosition).getDoubleValues().head / 10
    val rtimageTableTopLateralPosition = rtimageAl.get(TagByName.TableTopLateralPosition).getDoubleValues().head / 10

    println("rtimageTableTopVerticalPosition     : " + rtimageTableTopVerticalPosition)
    println("rtimageTableTopLongitudinalPosition : " + rtimageTableTopLongitudinalPosition)
    println("rtimageTableTopLateralPosition      : " + rtimageTableTopLateralPosition)
    println

    println("Vertical     : " + (rtimageTableHeight - ctTableHeight))
    println("Longitudinal : " + (rtimageTableTopLongitudinalPosition - ctTableTopLongitudinalPosition))
    println("Lateral      : " + (rtimageTableTopLateralPosition - ctTableTopLateralPosition))
    println

    //    println(imageReg.transform(new Point3d(0, 0, 0)))
    //    println(imageReg.transform(new Point3d(39.2923436395735, 0, 0)))
    //    println(imageReg.transform(new Point3d(0, 39.2923436395735, 0)))
    //    println(imageReg.transform(new Point3d(0, 0, 39.2923436395735)))
    //    println(imageReg.transform(new Point3d(6.73369765576041, 1259.26252360987, 39.2923436395735)))

    //println("inverted 0,0,0: " + invTransform(new Point3d(0, 0, 0)))

  }
}
