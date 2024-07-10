package org.aqa.customizeRtPlan.phase3plan

/*
 * Copyright 2024 Regents of the University of Michigan
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

import org.aqa.web.WebUtil.ValueMapT

import scala.annotation.tailrec

case class SubProcedureList(metaData: SPMetaData, beamList: Seq[Beam]) {

  private def makeSubProcedureList: Seq[SubProcedure] = {

    val list = Seq(
      new SPCollimatorCentering(metaData, beamList),
      new SPFocalSpot(metaData, beamList),
      new SPSymFlatConst(metaData, beamList),
      new SPLeafPosition(metaData, beamList),
      new SPVmat(metaData, beamList),
      new SPCollimatorPosition(metaData, beamList),
      new SPCenterDose(metaData, beamList),
      new SPWedge(metaData, beamList)
    )

    val claimed = list.flatMap(sub => sub.selectionList.flatMap(_.beamList)).map(_.beamName).toSet

    val unclaimed = beamList.filterNot(beam => claimed.contains(beam.beamName))

    val otherSubProc = new SPOther(metaData, unclaimed)

    list :+ otherSubProc
  }

  /** List of Phase3 sub-procedures. */
  val subProcedureList: Seq[SubProcedure] = makeSubProcedureList

  private val htmlIdMap = subProcedureList.flatMap(_.selectionList).map(sel => (sel.htmlId, sel)).toMap

  /**
    * Return the list of sub procedures that are selecting the beam.
    *
    * @param beam     For this beam.
    * @param valueMap User selections.
    * @return List of sub procedures that are selecting the beam.
    */
  private def beamUseList(beam: Beam, valueMap: ValueMapT): Seq[SubProcedure] = {
    val list = subProcedureList.flatMap(_.selectionList).filter(sel => sel.isSelected(valueMap)).map(_.subProcedure).groupBy(_.name).map(_._2.head)
    list.toSeq
  }

  /**
    * Get the distinct list of all beams, sorted by name.
    * @return List of beams.
    */
  def allBeams: Seq[Beam] = {
    val list = subProcedureList.flatMap(_.selectionList.flatMap(_.beamList))
    list.groupBy(_.beamName).map(_._2.head).toSeq.sortBy(_.beamName)
  }

  /**
    * Determine if the beam is used by the sub-procedure.
    *
    * @param beam         For this beam.
    * @param subProcedure For this sub procedure.
    * @param valueMap     User selections.
    * @return True if the beam is used by the sub-procedure.
    */
  def subProcedureUsesBeam(beam: Beam, subProcedure: SubProcedure, valueMap: ValueMapT): Boolean = {
    beamUseList(beam, valueMap).exists(_.name.equals(subProcedure.name))
  }

  /**
    * Find the selection that matches the given HTML id.
    *
    * @param htmlId Look for this id.
    * @return matching one.
    */
  def findByHtmlId(htmlId: String): Option[Selection] = {
    htmlIdMap.get(htmlId)
    //subProcedureList.flatMap(_.selectionList).find(s => s.htmlIdMatches(htmlId))
  }

  /**
    * Return the list of selections that have all of their beams on the list.
    *
    * @param beamList List of beams to be put in plan.
    * @return List of selections.
    */
  def getSelectionsFromBeams(beamList: Iterable[Beam]): Seq[Selection] = {
    def isSelected(beam: Beam) = {
      val isSel = beamList.exists(b => b.beamName.equals(beam.beamName))
      isSel
    }

    def allBeamsSelected(sel: Selection): Boolean = {
      sel.beamList.map(isSelected).reduce(_ && _)
    }

    val selectionList = subProcedureList.flatMap(_.selectionList).filter(allBeamsSelected)
    selectionList
  }

}

object SubProcedureList {

  private def beamListsAreTheSame(a: Seq[Beam], b: Seq[Beam]): Boolean = {

    def distinctSortedBeamNameList(beamList: Seq[Beam]): String = beamList.map(_.beamName).distinct.sorted.mkString(" @@@@ ")

    distinctSortedBeamNameList(a).equals(distinctSortedBeamNameList(b))
  }

  /**
    * Make a sub procedure list.  This requires recursively creating the list to make sure all of
    * the sub procedures are aware of the beams each sub procedure created.
    * @param metaData Machine based data.
    * @return Sub procedure list.
    */
  def makeSubProcedureList(metaData: SPMetaData): SubProcedureList = {

    @tailrec
    def make(beamList: Seq[Beam]): SubProcedureList = {

      val spList = SubProcedureList(metaData, beamList)

      if (beamListsAreTheSame(spList.allBeams, beamList))
        spList
      else
        make(spList.allBeams)

    }

    make(metaData.prototypeBeamList)

  }

}
