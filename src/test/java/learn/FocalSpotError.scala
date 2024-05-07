package learn

object FocalSpotError {

  // distance in mm from radiation source to X jaw, Y jaw, and collimator.
  private val xJaw = 406.0
  private val yJaw = 319.0
  private val MLC = 490.0

  private val dEpid_mm = 1500.0

  private val aX = 1.0 / (((dEpid_mm - xJaw) / xJaw) - ((dEpid_mm - MLC) / MLC))
  private val aY = 1.0 / (((dEpid_mm - yJaw) / yJaw) - ((dEpid_mm - MLC) / MLC))

  private val errorRange = {
    val errorIncrement = 0.01
    val count = 20
    (-count until count).map(_ * errorIncrement)
  }

  private val noErrorRange = Seq(0.0)

  private val JawCenterXErrorRange = errorRange
  private val JawCenterYErrorRange = noErrorRange
  private val MLCCenterXErrorRange = noErrorRange
  private val MLCCenterYErrorRange = noErrorRange

  private def calcX(xError: Double): Double = {
    val focalSpotX = aX * xError
    focalSpotX
  }

  private def calcY(yError: Double): Double = {
    val focalSpotY = aY * yError
    focalSpotY
  }

  private def separator(): Unit = println("--------------------------------------------------------------------------")

  private def showExample(): Unit = {
    separator()
    val url = "https://automatedqualityassurance.org/results/INST_2/MACH_67/Focal_Spot_0.1_9/2024-02-12T16-48-51-563_2656/output_2024-02-14T12-13-50-686/matlabMV6.txt"
    println(s"Example from $url")
    val JawCenterX = 0.8268062771015501
    val JawCenterY = 0.2263539006549706
    val MLCCenterX = 0.8663711615849685
    val MLCCenterY = 0.2563680686059779

    val errorX = JawCenterX - MLCCenterX
    val errorY = JawCenterY - MLCCenterY

    println(s"== X : $errorX --> ${calcX(errorX)}")
    println(s"== Y : $errorY --> ${calcY(errorY)}")

    separator()
  }

  def showUMich(): Unit = {
    val xLo = -0.118189694
    val xHi = 0.20239700631825386

    val yLo = -0.326805608
    val yHi = -0.158194613

    println(s"== X low  : $xLo --> ${calcX(xLo)}")
    println(s"== X high : $xHi --> ${calcX(xHi)}")

    println(s"== Y low  : $yLo --> ${calcY(yLo)}")
    println(s"== Y high : $yHi --> ${calcY(yHi)}")


  }

  def main(args: Array[String]): Unit = {
    // errorRange.foreach(error => println(s"$error,${calcX(error)},${calcY(error)}"))
    // separator()
    printf("aX: %20.16f\n", aX)
    printf("aY: %20.16f\n", aY)
    separator()
    // showExample()
    showUMich()
  }

}
