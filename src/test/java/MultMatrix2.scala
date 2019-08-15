
import javax.vecmath.Point3d
import javax.vecmath.Matrix4d
import edu.umro.ImageUtil.ImageUtil
import org.aqa.DicomFile
import java.io.File
import org.aqa.IsoImagePlaneTranslator
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import javax.vecmath.Matrix4d
import javafx.geometry.Pos
import javax.vecmath.Point3d
import javax.vecmath.Point3d

/**
 * Compare position of BB in CBCT to RTPLAN isocenter.
 */
object MultMatrix2 {

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val image270 = new DicomFile((new File("""D:\tmp\aqa\CBCT\MQATX1OBIQA2019Q3\RI.MQATX1OBIQA2019Q3.MV_270_33a.dcm"""))).attributeList.get
    //println("Starting...")

    val planIsocenter = new Point3d(4.31257743667401, 162.748700784692, 64.8245614035088)
    println("planIsocenter: " + planIsocenter)

    val trans = new IsoImagePlaneTranslator(image270)
    val RTImagePosition = image270.get(TagFromName.RTImagePosition).getDoubleValues.toList
    println("RTImagePosition: " + RTImagePosition)

    val matrix = {
      val mtrx = Array(
        0.99999999927062, -1.722732359e-05, -3.408786193e-05, -4.3075639782974,
        1.7226136319e-05, 0.99999999924508, -3.482974132e-05, -162.74651712817,
        3.4088461926e-05, 3.4829154096e-05, 0.99999999881245, -64.830376735237,
        0, 0, 0, 1)
      new Matrix4d(mtrx)
    }

    def tryPoint(p: Point3d): Double = {
      val c = p.clone.asInstanceOf[Point3d]
      matrix.transform(c)
      val dist = c.distance(planIsocenter)
      dist
    }

    if (false) {
      val pts = { for (x <- 5 to 10; y <- 320 to 330; z <- 120 to 190) yield { new Point3d(x, y, z) } }
      val best = pts.minBy(p => tryPoint(p))
      println("best: " + best + " : " + tryPoint(planIsocenter))
    }

    /** Invert the matrix to find what the point should be in the image space. */
    if (true) {
      val invMatrix = {
        val im = matrix.clone.asInstanceOf[Matrix4d]
        im.invert
        im
      }
      val planInv = planIsocenter.clone.asInstanceOf[Point3d]
      invMatrix.transform(planInv)
      println("planInv: " + planInv)
    }

    val x_pix = 321
    val y_pix = 321

    println("XY as raw pixel coordinates: " + x_pix + ", " + y_pix)

    //    val x_mm = x_pix * 0.672
    //    val y_mm = y_pix * 0.672
    //    val x_pp = x_mm - 214.704
    //    val y_pp = y_mm + 214.704

    val iso = trans.pix2Iso(x_pix, y_pix)

    println("XY as isoplane coordinates: " + iso.getX + ", " + iso.getY)

    //    val x_pp = iso.getX + RTImagePosition(0)
    //    val y_pp = iso.getY + RTImagePosition(1)
    //
    //    val point = new Point3d(x_pp, y_pp, 0)

    val after = new Point3d(iso.getX, iso.getY, 0)
    matrix.transform(after)
    print("isoplane point after matrix translation:	 " + after + "    Distance to isocenter: " + after.distance(planIsocenter))

    //println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}