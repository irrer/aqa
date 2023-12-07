package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.ValueMapT

import scala.annotation.tailrec

case class SubProcedureList(metaData: SPMetaData, beamList: Seq[Beam]) {

  private def makeSubProcedureList: Seq[SubProcedure] = {

    Seq(
      new SPFocalSpot(metaData, beamList),
      new SPSymFlatConst(metaData, beamList),
      new SPLeafPosition(metaData, beamList),
      new SPWedge(metaData, beamList),
      new SPCenterDose(metaData, beamList)
    )
  }

  /** List of Phase3 sub-procedures. */
  val subProcedureList: Seq[SubProcedure] = makeSubProcedureList

  /**
    * Return the list of sub procedures that are selecting the beam.
    *
    * @param beam     For this beam.
    * @param valueMap User selections.
    * @return List of sub procedures that are selecting the beam.
    */
  def beamUseList(beam: Beam, valueMap: ValueMapT): Seq[SubProcedure] = {
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
    subProcedureList.flatMap(_.selectionList).find(s => s.htmlIdMatches(htmlId))
  }

  /**
    * Return the list of selections that have all of their beams on the list.
    *
    * @param beamList List of beams to be put in plan.
    * @return List of selections.
    */
  def getSelectionsFromBeams(beamList: Iterable[Beam]): Seq[Selection] = {

    def isSelected(beam: Beam) = beamList.exists(b => b.beamName.equals(beam.beamName))

    def allBeamsSelected(sel: Selection): Boolean = {
      sel.beamList.map(isSelected).reduce(_ && _)
    }

    subProcedureList.flatMap(_.selectionList).filter(allBeamsSelected)

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
