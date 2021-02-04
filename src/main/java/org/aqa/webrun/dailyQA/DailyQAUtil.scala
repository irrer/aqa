package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.ScalaUtil.DicomUtil

object DailyQAUtil {


  /**
   * Get numerical values from an attribute list, scaling by the amount given.
   *
   * @param al  Attribute list containing values.
   * @param tag Attribute tag of value.
   * @param scale Amount to scale value.
   * @return String representation of scaled values.
   */
  def getValues(al: AttributeList, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    try {
      val at = DicomUtil.findAllSingle(al, tag).head
      def getLong() = at.getLongValues.map(l => (l * scale).round)
      val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
      val numList = vr match {
        case _ if ValueRepresentation.isIntegerStringVR(vr) => at.getIntegerValues.map(i => (i * scale).round.toInt)
        case _ if ValueRepresentation.isLongStringVR(vr) => getLong()

        case _ if ValueRepresentation.isSignedLongVR(vr) => getLong()
        case _ if ValueRepresentation.isSignedShortVR(vr) => getLong()

        case _ if ValueRepresentation.isUnsignedLongVR(vr) => getLong()
        case _ if ValueRepresentation.isUnsignedShortVR(vr) => at.getIntegerValues.map(i => (i * scale).round.toShort)

        case _ if ValueRepresentation.isFloatDoubleVR(vr) => at.getDoubleValues.map(n => n * scale)
        case _ if ValueRepresentation.isFloatSingleVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ if ValueRepresentation.isDecimalStringVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ => throw new RuntimeException("Unrecognized value representation: " + new String(vr))
      }
      numList.map(n => n.toString)
    }
    catch {
      case _: Throwable => Seq("NA", "NA", "NA")
    }
  }
}
