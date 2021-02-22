package learn

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.ImageRegistration

import java.awt.geom.Point2D
import java.io.File
import javax.vecmath.Point3d

object RtimageReg {


  def main(args: Array[String]): Unit = {

    val reg = {
      val al = new AttributeList
      //al.read(new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2020Q4\2021-02-21T12-19-53_REG_1_1.2.246.352.218.5353185752592836639.11982477740574327946\1.2.246.352.218.5123519567806337270.1792960677915113613.dcm"""))
      al.read(new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2020Q4\2021-01-11T06-00-17_REG_1_1.2.246.352.62.2.5065897486350864360.10847396209542464138\1.2.246.352.62.5.5672252106010238196.16430619410575433126.dcm"""))
      ImageRegistration(al)
    }


    val rtimage = {
      val al = new AttributeList
      // al.read(new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2020Q4\2021-02-21T12-19-47_RTIMAGE_2_1.2.246.352.62.2.5150655486898145957.2390264733442766740\1.2.246.352.62.1.5553569058613007174.14031500428385119422.dcm"""))
      // al.read(new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2020Q4\2021-02-21T12-19-47_RTIMAGE_2_1.2.246.352.62.2.5150655486898145957.2390264733442766740\1.2.246.352.62.1.4751740208772258906.15794050494969476754.dcm"""))
      al.read(new File("""\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2020Q4\2021-01-11T06-00-03_RTIMAGE_2_1.2.246.352.62.2.5751112453097979980.8767451584845814973\1.2.246.352.62.1.5106064291787872175.554446387753147016.dcm"""))
      al
    }

    println("GantryAngle: " + rtimage.get(TagByName.GantryAngle).getDoubleValues.head)

    val trans = new IsoImagePlaneTranslator(rtimage)

    val point_pix = new Point2D.Double(319.24996423721313, 319.6250431537628)
    println("point_pix: " + point_pix)

    val point2d_iso = trans.pix2Iso(point_pix)

    val point3d_iso = new Point3d(point2d_iso.getX, point2d_iso.getY, rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues()(2))
    println("point3d_iso : " + point3d_iso)

    val answer = reg.transform(point3d_iso)

    println("answer: " + answer)

  }

}
