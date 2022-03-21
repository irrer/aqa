/*
 * Copyright 2022 Regents of the University of Michigan
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
import org.aqa.db.GapSkew
import org.aqa.webrun.gapSkew.GapSkewUtil._
import org.aqa.webrun.phase2.Phase2Util

object EdgesFromPlan extends Logging {

  /**
    * Describe one beam's position.
    *
    * @param position_mm Position in isoplane in mm.
    * @param edgeType    If true, then the jaw is positioned at this place.  The collimator may also be here, but
    *                    it will be somewhat obscured by teh jaw.
    */
  case class BeamLimit(position_mm: Double, edgeType: GapSkew.EdgeType) {
    override def toString: String = {
      "limit_mm: " + fmt2(position_mm) + "    isJaw: " + edgeType.toString.formatted("%6s")
    }
  }

  /** Describe a pair of beam limits with their orientation in the image. */
  case class OrientedEdgePair(topOrLeft: Option[BeamLimit], bottomOrRight: Option[BeamLimit]) {
    private val all = Seq(topOrLeft, bottomOrRight).flatten

    Trace.trace(all.map(_.toString).mkString("\n", "\n", "\n"))
    if (all.map(_.edgeType.bank).distinct.size == 1)
      Trace.trace("hey")
    // def bank1 = all.find(_.edgeType.is1).get
    // def bank2 = all.find(e => !e.edgeType.is1).get
  }

  private def getPos(beamLim: Seq[AttributeList], bankNumber: Int, nameList: Seq[String]): Option[Double] = {
    def bld(al: AttributeList): String = {
      val at = al.get(TagByName.RTBeamLimitingDeviceType)
      at.getSingleStringValueOrEmptyString().trim.toUpperCase()
    }
    val posSeq = beamLim.find(al => nameList.contains(bld(al)))
    if (posSeq.isDefined) {
      val list = posSeq.get.get(TagByName.LeafJawPositions).getDoubleValues
      val p = if (bankNumber == 1) list.head else list.last
      Some(p)
    } else None
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
  def edgesFromPlan(rtimage: AttributeList, rtplan: AttributeList): OrientedEdgePair = {
    val beamNumber = Util.beamNumber(rtimage)
    val beamName = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage).get
    val col = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))
    logger.info("Leaf Gap Skew processing BeamName: " + beamName + "    BeamNumber: " + beamNumber + "   Col Angle rounded: " + col)
    val beamSequence = Phase2Util.getBeamSequence(rtplan, Util.beamNumber(rtimage))
    // get the first control point that contains RTBeamLimitingDeviceType
    val cps = DicomUtil.seqToAttr(beamSequence, TagByName.ControlPointSequence).filter(al => DicomUtil.findAllSingle(al, TagByName.RTBeamLimitingDeviceType).nonEmpty).head

    // convert them to a list of edge pairs
    val beamLimitList = DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)

    val xJawNames = Seq("X", "ASYMX")
    val yJawNames = Seq("Y", "ASYMY")
    val xMlcNames = Seq("MLCX") // note: MLCY is not supported by the treatment machines

    val jawX1 = getPos(beamLimitList, 1, xJawNames).get
    val jawX2 = getPos(beamLimitList, 2, xJawNames).get
    val jawY1 = getPos(beamLimitList, 1, yJawNames).get
    val jawY2 = getPos(beamLimitList, 2, yJawNames).get
    val mlcX1 = getPos(beamLimitList, 1, xMlcNames)
    val mlcX2 = getPos(beamLimitList, 2, xMlcNames)

    // true if beam is more restricted by top and bottom than right to left
    val topBottom: Boolean = {
      val mlcSize = {
        (mlcX1, mlcX2) match {
          case (Some(mx1), Some(mx2)) =>
            Some(mx2 - mx1)
          case _ =>
            None // not valid
        }
      }
      val jawXSize = Some(jawX2 - jawX1)
      val jawYSize = Some(jawY2 - jawY1)

      val vert = (col == 90) || (col == 270)

      val minXSize =
        if (vert)
          Seq(jawXSize, mlcSize).flatten.min
        else
          jawXSize.get

      val minYSize =
        if (vert)
          jawYSize.get
        else
          Seq(jawYSize, mlcSize).flatten.min

      minXSize <= minYSize
    }

    // Note: Not all possible combinations of Jaw and MLC use are covered here (for
    // example collimator angle at 0).  The design can be expanded to handle them,
    // but until there are test cases it is best not to write code.
    val orientedEdgePair = (jawX1, jawX2, jawY1, jawY2, mlcX1, mlcX2, col, topBottom) match {
      case (jx1, jx2, _, _, Some(mx1), Some(mx2), 90, true) =>
        val top =
          if (jx2 < mx2)
            BeamLimit(jx2, GapSkew.EdgeType.X1_Jaw_Horz)
          else
            BeamLimit(mx2, GapSkew.EdgeType.X2_MLC_Horz)

        val bottom =
          if (jx1 > mx1)
            BeamLimit(jx1, GapSkew.EdgeType.X2_Jaw_Horz)
          else
            BeamLimit(mx1, GapSkew.EdgeType.X1_MLC_Horz)

        OrientedEdgePair(Some(top), Some(bottom))

      case (jx1, jx2, _, _, Some(mx1), Some(mx2), 270, true) =>
        val top =
          if (jx2 < mx2)
            BeamLimit(jx2, GapSkew.EdgeType.X1_Jaw_Horz)
          else
            BeamLimit(mx2, GapSkew.EdgeType.X2_MLC_Horz)

        val bottom =
          if (jx1 > mx1)
            BeamLimit(jx1, GapSkew.EdgeType.X2_Jaw_Horz)
          else
            BeamLimit(mx1, GapSkew.EdgeType.X1_MLC_Horz)
        OrientedEdgePair(Some(top), Some(bottom))

      // no MLC, just jaws, topBottom: true
      case (jx1, jx2, _, _, None, None, _, true) =>
        val top = BeamLimit(jx2, GapSkew.EdgeType.X1_Jaw_Horz)
        val bottom = BeamLimit(jx1, GapSkew.EdgeType.X2_Jaw_Horz)
        OrientedEdgePair(Some(top), Some(bottom))

      // no MLC, just jaws, topBottom: false
      case (_, _, jy1, jy2, None, None, _, false) =>
        val left = BeamLimit(jy1, GapSkew.EdgeType.Y1_Jaw_Horz)
        val right = BeamLimit(jy2, GapSkew.EdgeType.Y2_Jaw_Horz)
        OrientedEdgePair(Some(left), Some(right))

      case _ =>
        throw new RuntimeException("Unhandled jaw/MLC orientation configuration.")
    }

    logger.info("Leaf Gap Skew processing BeamName: " + beamName + "    BeamNumber: " + beamNumber + "   Col Angle rounded: " + col + "    Leaf ends positioned at " + orientedEdgePair)

    orientedEdgePair
  }

}
