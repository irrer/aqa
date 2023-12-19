package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config
import org.aqa.Util

import scala.xml.Elem

class SPSymFlatConst(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Symmetry Flatness"

  override val abbreviation: String = "Sym Flat"
  def toCdBeam(beam: Beam): Elem = ???

  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    val margin = Config.SymmetryAndFlatnessDiameter_mm / 2
    val max = {
      val m = Config.SymmetryAndFlatnessPointList.flatMap(p => Seq(p.x_mm, p.y_mm)).flatMap(c => Seq(c + margin, c - margin))
      m.max - m.min
    }

    def ok(beam: Beam) = Util.minCenteredFieldBeam(beam.prototypeBeam, max)
    val list = beamList.filter(ok).map(toSelection)
    list
  }

  override def setBeamList(beamList: Seq[Beam]): Elem = {
    beamList.map(toCdBeam)
    ???
  }

  override def update(checkboxIdList: Seq[String]): Seq[Beam] = {
    ???
  }

  override def getBeamList: Seq[Beam] = selectionList.flatMap(_.beamList)

  override def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList] = {
    ???
  }

}
