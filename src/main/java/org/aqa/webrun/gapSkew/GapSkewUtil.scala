package org.aqa.webrun.gapSkew

object GapSkewUtil {

  /**
    * Format a number so that it shows at least 2 significant (non-zero) digits in
    * the mantissa.  The last digit is correctly rounded.
    * @param d Number to format.
    * @return Formatted number.
    */
  def fmt2(d: Double): String = {
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

  def main(args: Array[String]): Unit = {
    val testList = Seq(1.0, -3.0, 100.000005555, 0.003221, -100.654654654, 0.000348300045, 243243654.87554)
    testList.foreach(d => {
      println("d: " + d.formatted("%30.20f") + " >>" + fmt2(d) + "<<")
    })
  }
}
