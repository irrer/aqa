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

  def isVert(angleType: AngleType.Value) = angleType.toString.equals(vertical.toString)
  def isHorz(angleType: AngleType.Value) = !isVert(angleType)
}