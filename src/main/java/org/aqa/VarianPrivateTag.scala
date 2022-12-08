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

import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.CodeStringAttribute
import com.pixelmed.dicom.DecimalStringAttribute
import com.pixelmed.dicom.IntegerStringAttribute
import com.pixelmed.dicom.LongStringAttribute
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.ShortStringAttribute
import com.pixelmed.dicom.UniqueIdentifierAttribute

object VarianPrivateTag extends Logging {

  val VarianCreator3243 = new AttributeTag(0x3243, 0x0010) //  LO
  val BeamSecondaryName = new AttributeTag(0x3243, 0x1009) //  LO
  val VarianCreator3249 = new AttributeTag(0x3249, 0x0010) //  LO
  val MaximumTreatmentTime = new AttributeTag(0x3249, 0x1000) //  DS
  val ReferencedPrimaryDoseRefUID = new AttributeTag(0x3249, 0x1010) //  UI
  val VarianCreator3253 = new AttributeTag(0x3253, 0x0010) //  LO
  val ExtendedInterfaceData = new AttributeTag(0x3253, 0x1000) //  OB
  val ExtendedInterfaceLength = new AttributeTag(0x3253, 0x1001) //  IS
  val ExtendedInterfaceFormat = new AttributeTag(0x3253, 0x1002) //  CS
  val VarianCreator3267 = new AttributeTag(0x3267, 0x0010) //  LO
  val ReferencedPatientVolumeID = new AttributeTag(0x3267, 0x1000) //  SH
  val VarianCreator3285 = new AttributeTag(0x3285, 0x0010) //  LO
  val PrimaryFluenceModeSequence = new AttributeTag(0x3285, 0x1000) //  SQ
  val FluenceMode = new AttributeTag(0x3285, 0x1001) //  CS
  val FluenceModeID = new AttributeTag(0x3285, 0x1002) //  SH
  val VarianCreator3287 = new AttributeTag(0x3287, 0x0010) //  LO
  val PlanIntegritySequence = new AttributeTag(0x3287, 0x1000) //  SQ
  val PlanIntegrityHash = new AttributeTag(0x3287, 0x1001) //  LO
  val PlanIntegrityHashVersion = new AttributeTag(0x3287, 0x1002) //  SH

  val tagList = Seq(
    VarianCreator3243,
    BeamSecondaryName,
    VarianCreator3249,
    MaximumTreatmentTime,
    ReferencedPrimaryDoseRefUID,
    VarianCreator3253,
    ExtendedInterfaceData,
    ExtendedInterfaceLength,
    ExtendedInterfaceFormat,
    VarianCreator3267,
    ReferencedPatientVolumeID,
    VarianCreator3285,
    PrimaryFluenceModeSequence,
    FluenceMode,
    FluenceModeID,
    VarianCreator3287,
    PlanIntegritySequence,
    PlanIntegrityHash,
    PlanIntegrityHashVersion
  )

  def newExtendedInterfaceFormat(text: String) = {
    val attr = new CodeStringAttribute(ExtendedInterfaceFormat)
    attr.addValue(text);
    attr
  }

  def newFluenceMode(text: String) = {
    val attr = new CodeStringAttribute(FluenceMode)
    attr.addValue(text)
    attr
  }

  def newMaximumTreatmentTime(text: String) = {
    val attr = new DecimalStringAttribute(MaximumTreatmentTime)
    attr.addValue(text)
    attr
  }

  def newExtendedInterfaceLength(text: String) = {
    val attr = new IntegerStringAttribute(ExtendedInterfaceLength)
    attr.addValue(text)
    attr
  }

  def newBeamSecondaryName(text: String) = {
    val attr = new LongStringAttribute(BeamSecondaryName)
    attr.addValue(text)
    attr
  }

  def newPlanIntegrityHash(text: String) = {
    val attr = new LongStringAttribute(PlanIntegrityHash)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3243(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3243)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3249(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3249)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3253(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3253)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3267(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3267)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3285(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3285)
    attr.addValue(text)
    attr
  }

  def newVarianCreator3287(text: String) = {
    val attr = new LongStringAttribute(VarianCreator3287)
    attr.addValue(text)
    attr
  }

  def newExtendedInterfaceData = {
    val attr = new OtherByteAttribute(ExtendedInterfaceData)
  }

  def newFluenceModeID(text: String) = {
    val attr = new ShortStringAttribute(FluenceModeID)
    attr.addValue(text)
    attr
  }

  def newPlanIntegrityHashVersion(text: String) = {
    val attr = new ShortStringAttribute(PlanIntegrityHashVersion)
    attr.addValue(text)
    attr
  }

  def newReferencedPatientVolumeID(text: String) = {
    val attr = new ShortStringAttribute(ReferencedPatientVolumeID)
    attr.addValue(text)
    attr
  }

  def newPlanIntegritySequence = {
    val attr = new SequenceAttribute(PlanIntegritySequence)
  }

  def newPrimaryFluenceModeSequence = {
    val attr = new SequenceAttribute(PrimaryFluenceModeSequence)
  }

  def newReferencedPrimaryDoseRefUID(text: String) = {
    val attr = new UniqueIdentifierAttribute(ReferencedPrimaryDoseRefUID)
    attr.addValue(text)
    attr
  }
}
