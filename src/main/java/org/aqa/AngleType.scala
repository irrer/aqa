/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import com.pixelmed.dicom.AttributeList

/**
  * For classifying angles.
  */
object AngleType extends Enumeration with Logging {
  val horizontal: Value = Value
  val vertical: Value = Value

  /**
    * Calculate the error from this angle to the nearest orthogonal angle in degrees.
    * @param angle Angle in degrees.
    * @return Absolute value of difference.
    */
  def orthogonalAngleError(angle: Double): Double = {
    val rounded = Util.angleRoundedTo90(angle)
    val a = (rounded - ((angle + (360 * 10)) % 360.0)).abs
    val b = (a - 360).abs
    Math.min(a, b)
  }

  /**
    * Only allow angles that are within 5 degrees of right angles.
    */
  def classifyAngle(angle: Double): Option[AngleType.Value] = {

    if (orthogonalAngleError(angle) < Config.BBbyEPIDMaxAllowedGantryErrorFromOrthogonal_deg) {
      val rounded = Util.angleRoundedTo90(angle)
      val angTyp = rounded match {
        case 0   => Some(AngleType.vertical)
        case 180 => Some(AngleType.vertical)
        case 90  => Some(AngleType.horizontal)
        case 270 => Some(AngleType.horizontal)
        case _ =>
          logger.warn(s"Unable to classify angle as vertical or horizontal: $angle")
          None // should never happen
      }
      angTyp
    } else {
      logger.warn(s"Angle error exceeded tolerance of ${Config.BBbyEPIDMaxAllowedGantryErrorFromOrthogonal_deg} degrees from orthogonal: $angle")
      None
    }
  }

  /**
    * Return true if the attribute list's gantry angle is of the specified type.
    */
  def isAngleType(angle: Double, angleType: AngleType.Value): Boolean = {
    classifyAngle(angle) match {
      case Some(at) => at.toString.equals(angleType.toString)
      case _        => false
    }
  }

  /**
    * Return true if the attribute list's gantry angle is of the specified type.
    */
  def isAngleType(al: AttributeList, angleType: AngleType.Value): Boolean = isAngleType(Util.gantryAngle(al), angleType)

  def isVert(angleType: AngleType.Value): Boolean = angleType.toString.equals(vertical.toString)
  def isHorz(angleType: AngleType.Value): Boolean = !isVert(angleType)

  /**
    * For testing only.  Try classifying a variety of angles.
    * @param args not used.
    */
  def main(args: Array[String]): Unit = {

    var a = -730.0

    val good = Set(0, 90, 180, 270)

    while (a < 730) {
      val aText = a.formatted("%8.2f")
      val result = classifyAngle(a)
      val resultText = result match {
        case Some(AngleType.vertical)   => "V"
        case Some(AngleType.horizontal) => "H"
        case _                          => " "
      }

      val ra = {
        val ar = (a.round + 3600) % 360

        if ((a.round == a) && good.contains(ar.toInt))
          " == " + ar.formatted("%4d")
        else
          ""
      }

      println(s"$aText :: $resultText  $ra")
      a = a + 0.25
    }

  }

}
