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
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.GapSkew
import org.aqa.webrun.phase2.Phase2Util

object EdgesFromPlan extends Logging {

  /**
    * Describe one beam's position.
    *
    * @param limit_mm Position in isoplane in mm.
    * @param edgeType If true, then the jaw is positioned at this place.  The collimator may also be here, but
    *              it will be somewhat obscured by teh jaw.
    */
  case class BeamLimit(limit_mm: Double, edgeType: String) {
    override def toString: String = {
      "limit_mm: " + limit_mm.formatted("%8.2f") + "    isJaw: " + edgeType.toString.formatted("%5s")
    }
  }

  /**
    * Describe a pair of beam limits.
    * @param X1 X1 jaw or collimator edge
    * @param X2 X2 jaw or collimator edge
    */
  case class EndPair(X1: BeamLimit, X2: BeamLimit) {
    override def toString: String = {
      "X1: " + X1 + "    X2: " + X2
    }
  }

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
    val beamSequence = Phase2Util.getBeamSequence(rtplan, Util.beamNumber(attributeList))
    val cps = DicomUtil.seqToAttr(beamSequence, TagByName.ControlPointSequence).head
    val beamLimitList = DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)

    case class Limiter(X1: Double, X2: Double, name: String) {}

    def toLimiter(al: AttributeList): Limiter = {
      val valueList = al.get(TagByName.LeafJawPositions).getDoubleValues
      val devType = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
      Limiter(valueList.head, valueList.last, devType)
    }

    val limiterList = beamLimitList.map(toLimiter)
    val jawOp = limiterList.find(l => l.name.equalsIgnoreCase("X") || l.name.equalsIgnoreCase("ASYMX"))
    val mlcOp = limiterList.find(_.name.equalsIgnoreCase("MLCX"))

    val endPair = (jawOp, mlcOp) match {
      case (Some(jaw), Some(mlc)) =>
        val X1 = {
          if (jaw.X1 > mlc.X1)
            BeamLimit(jaw.X1, edgeType = GapSkew.edgeTypeJaw) // edge is jaw
          else
            BeamLimit(mlc.X1, edgeType = GapSkew.edgeTypeMlc) // edge is mlc
        }
        val X2 = {
          if (jaw.X2 < mlc.X2)
            BeamLimit(jaw.X2, edgeType = GapSkew.edgeTypeJaw) // edge is jaw
          else
            BeamLimit(mlc.X2, edgeType = GapSkew.edgeTypeMlc) // edge is mlc
        }
        EndPair(X1, X2)
      case (None, Some(mlc)) => EndPair(BeamLimit(mlc.X1, edgeType = GapSkew.edgeTypeMlc), BeamLimit(mlc.X2, edgeType = GapSkew.edgeTypeMlc))
      case (Some(jaw), None) => EndPair(BeamLimit(jaw.X1, edgeType = GapSkew.edgeTypeJaw), BeamLimit(jaw.X2, edgeType = GapSkew.edgeTypeJaw))
      case (None, None) =>
        throw new RuntimeException("Could not find leaf or jaw pair ")
    }

    logger.info(
      "BeamName: " + Phase2Util.getBeamNameOfRtimage(rtplan, attributeList).get +
        "    BeamNumber: " + Util.beamNumber(attributeList).formatted("%2d") +
        "    Leaf ends positioned at " + endPair
    )
    endPair
  }

}
