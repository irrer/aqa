
import javax.vecmath.Point3d
import javax.vecmath.Matrix4d

/**
 * Compare position of BB in CBCT to RTPLAN isocenter.
 */
object MultMatrix {

  def stdDev(list: Seq[Float]): Double = {
    val mean = list.sum / list.size
    val sumSq = list.map(d => (d - mean) * (d - mean)).sum
    val variance = sumSq / mean
    val sd = Math.sqrt(variance)
    sd
  }

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

    val list = (0 until 40).map(i => 1000.toFloat + i)
    println("stdDev: " + stdDev(list))

    println("Done.")
  }

}