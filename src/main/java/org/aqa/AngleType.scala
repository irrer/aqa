package org.aqa

import com.pixelmed.dicom.AttributeList

/**
 * For classifying angles.
 */
object AngleType extends Enumeration {
  val horizontal = Value
  val vertical = Value

  /**
   * Only allow angles that are within 5 degrees of right angles.
   */
  def classifyAngle(angle: Double): Option[AngleType.Value] = {
    val rounded = Util.angleRoundedTo90(angle)
    val canonicalAngle = ((angle.round.toInt + 3600) % 360)
    val angTyp = (((rounded - canonicalAngle).abs < 5), canonicalAngle) match {
      case (true, 0) => Some(AngleType.vertical)
      case (true, 180) => Some(AngleType.vertical)
      case (true, 90) => Some(AngleType.horizontal)
      case (true, 270) => Some(AngleType.horizontal)
      case _ => None
    }
    angTyp
  }

  /**
   * Return true if the attribute list's gantry angle is of the specified type.
   */
  def isAngleType(angle: Double, angleType: AngleType.Value): Boolean = {
    classifyAngle(angle) match {
      case Some(at) => at.toString.equals(angleType.toString())
      case _ => false
    }
  }

  /**
   * Return true if the attribute list's gantry angle is of the specified type.
   */
  def isAngleType(al: AttributeList, angleType: AngleType.Value): Boolean = isAngleType(Util.gantryAngle(al), angleType)

}