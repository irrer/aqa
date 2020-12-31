
import javax.vecmath.Point3d
import javax.vecmath.Matrix4d
import edu.umro.ImageUtil.ImageUtil

/**
 * Compare position of BB in CBCT to RTPLAN isocenter.
 */
object MultMatrix {

  def main(args: Array[String]): Unit = {
    println("Starting...")

    val planIsocenter = new Point3d(4.31257743667401, 162.748700784692, 64.8245614035088)

    val mtrx = Array(
      0.99999998456633, 4.0148244174e-05, -0.0001710422658, 10.1864508412671,
      -4.014257276e-05, 0.99999999864445, 3.3161255782e-05, 166.504194220801,
      0.0001710435969, -3.315438919e-05, 0.99999998482244, 59.0186991642416,
      0, 0, 0, 1)

    val matrix = new Matrix4d(mtrx)

    val point = new Point3d(-5.8947960, -3.7461186, 5.6676038)

    println("point before: " + point)
    matrix.transform(point)
    println("point after:	 " + point)

    val dist = point.distance(planIsocenter)
    println("Distance to isocenter: " + dist)

    if (true) {
      val aa = Array(0.9999988702791, 1.7390313489E-5, 0.00150304294732, 14.1426431498818, -1.74532925E-5, 0.99999999897039, 4.1887902036E-5, 169.988357962021, -0.0015030422173, -4.191408776E-5, 0.99999886955301, 71.761759310883, 0.0, 0.0, 0.0, 1.0)
      val bb = Array(0.9999999945459, 1.7448979044E-5, 1.029744256E-4, 14.1329209622889, -1.74532925E-5, 0.99999999897039, 4.1887902036E-5, 169.988357962021, -1.029736946E-4, -4.188969905E-5, 0.99999999382084, 71.7755152784078, 0.0, 0.0, 0.0, 1.0)

      val ma = new Matrix4d(aa)
      val mb = new Matrix4d(bb)

      val pa = new Point3d(-5.8947960, -3.7461186, 5.6676038)
      val pb = new Point3d(-5.8947960, -3.7461186, 5.6676038)

      ma.transform(pa)
      mb.transform(pb)

      println("pa: " + pa)
      println("pb: " + pb)
    }

    println("Done.")
  }

}