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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util

object EdgesFromPlan extends Logging {

  /**
    * Describe one beam's position.
    *
    * @param limit_mm Position in isoplane in mm.
    * @param isJaw If true, then the jaw is positioned at this place.  The collimator may also be here, but
    *              it will be somewhat obscured by teh jaw.
    */
  case class BeamLimit(limit_mm: Double, isJaw: Boolean) {}

  /**
    * Describe a pair of beam limits.
    * @param top The one nearer the top of the image.
    * @param bottom The one nearer the bottom of the image.
    */
  case class EndPair(top: BeamLimit, bottom: BeamLimit) {}

  /**
    * Get the positions of ends of the leaves in mm in isoplane.  Also
    * indicate whether they are defined by leaf or jaw.
    *
    * The complicated part is that both the jaws and MLC may or may not
    * be present, and each jaw may or may not obscure the MLC.  If a jaw
    * obscures (or matches) the MLC, the the jaw 'wins', and the edge is
    * labeled as being defined by the jaw.
    *
    * It is important to differentiate
    */
  def edgesFromPlan(attributeList: AttributeList, rtplan: AttributeList): EndPair = {
    Trace.trace("BeamName: " + Phase2Util.getBeamNameOfRtimage(rtplan, attributeList) + "     BeamNumber: " + Util.beamNumber(attributeList))
    val beamSequence = Phase2Util.getBeamSequence(rtplan, Util.beamNumber(attributeList))
    val cps = DicomUtil.seqToAttr(beamSequence, TagByName.ControlPointSequence).head
    val beamLimitList = DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)

    case class Limiter(top: Double, bottom: Double, name: String) {}

    def toLimiter(al: AttributeList): Limiter = {
      val valueList = al.get(TagByName.LeafJawPositions).getDoubleValues
      val devType = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
      Limiter(valueList.min, valueList.max, devType)
    }

    val limiterList = beamLimitList.map(toLimiter)
    val jawOp = limiterList.find(l => l.name.equalsIgnoreCase("X") || l.name.equalsIgnoreCase("ASYMX"))
    val mlcOp = limiterList.find(_.name.equalsIgnoreCase("MLCX"))

    val endPair = (jawOp, mlcOp) match {
      case (Some(jaw), Some(mlc)) =>
        val top = {
          if (jaw.top <= mlc.top) // jaw is at or beyond MLC
            BeamLimit(jaw.top, isJaw = true)
          else
            BeamLimit(mlc.top, isJaw = false) // edge is completely defined by MLC
        }
        val bottom = {
          if (jaw.bottom >= mlc.bottom) // jaw is at or beyond MLC
            BeamLimit(jaw.top, isJaw = true)
          else
            BeamLimit(mlc.top, isJaw = false) // edge is completely defined by MLC
        }
        EndPair(top, bottom)
      case (None, Some(mlc)) => EndPair(BeamLimit(mlc.top, isJaw = false), BeamLimit(mlc.bottom, isJaw = false))
      case (Some(jaw), None) => EndPair(BeamLimit(jaw.top, isJaw = true), BeamLimit(jaw.bottom, isJaw = true))
      case (None, None) =>
        throw new RuntimeException("Could not find leaf or jaw pair ")
    }

    logger.info("Leaf ends positioned at " + endPair)
    endPair
  }

}
