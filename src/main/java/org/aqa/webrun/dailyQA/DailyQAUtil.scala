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
