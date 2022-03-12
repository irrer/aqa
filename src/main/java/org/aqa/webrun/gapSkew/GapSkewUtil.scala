package org.aqa.webrun.gapSkew

import org.aqa.Config

object GapSkewUtil {

  /**
    * Format a number so that it shows at least 2 significant (non-zero) digits in
    * the mantissa.  The last digit is correctly rounded.
    * @param d Number to format.
    * @return Formatted number.
    */
  def fmt2(d: Double): String = {
    if (d.abs > 1)
      d.formatted("%30.2f").trim
    else {
      val mantissa = d.formatted("%30.20f").trim.replaceAll(""".*\.""", "")
      val firstSig = mantissa.indices.find(i => mantissa(i) != '0')
      val lastSig = {
        if (firstSig.isDefined)
          firstSig.get + 2
        else
          2
      }
      val text = d.formatted("%30." + lastSig + "f").trim
      text
    }
  }

  val colorFail = "#E00034"
  val colorWarn = "yellow"
  val colorPass = "#1DC32B"

  def statusColor(angle: Double): String = {
    val text = 0 match {
      case _ if angle.abs > Config.GapSkewAngleFail_deg => colorFail
      case _ if angle.abs > Config.GapSkewAngleWarn_deg => colorWarn
      case _                                            => colorPass
    }
    text
  }

  def main(args: Array[String]): Unit = {
    val testList = Seq(1.0, -3.0, 100.000005555, 0.003221, -100.654654654, 0.000348300045, 243243654.87554)
    testList.foreach(d => {
      println("d: " + d.formatted("%30.20f") + " >>" + fmt2(d) + "<<")
    })
  }
}
