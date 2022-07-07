package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
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

  val colorFail = "#FFAAAA"
  val colorWarn = "#FFFFAA"
  val colorPass = "#AAFFAA"

  val colorNone = "#888888"

  def statusColor(angle: Double): String = {
    val text = 0 match {
      case _ if angle.abs > Config.GapSkewAngleFail_deg => colorFail
      case _ if angle.abs > Config.GapSkewAngleWarn_deg => colorWarn
      case _                                            => colorPass
    }
    text
  }

  /**
    * Get the positions of the Y jaws in mm.  Looks for both Y and ASYMY.
    *
    * @return Y1 and Y2 values.
    */
  def yRtimageJawPositions_mm(rtimage: AttributeList): Seq[Double] = {
    val bl = DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceSequence)
    val alList = bl.flatMap(l => DicomUtil.alOfSeq(l.asInstanceOf[SequenceAttribute]))
    def isY(al: AttributeList) = {
      Seq("Y", "ASYMY").contains(al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString())
    }
    val yBl = alList.filter(isY).head
    yBl.get(TagByName.LeafJawPositions).getDoubleValues
  }

  def main(args: Array[String]): Unit = {
    val testList = Seq(1.0, -3.0, 100.000005555, 0.003221, -100.654654654, 0.000348300045, 243243654.87554)
    testList.foreach(d => {
      println("d: " + d.formatted("%30.20f") + " >>" + fmt2(d) + "<<")
    })
  }
}
