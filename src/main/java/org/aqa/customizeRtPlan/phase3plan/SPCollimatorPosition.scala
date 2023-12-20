package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Util

import scala.xml.Elem

class SPCollimatorPosition(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Position"

  override val abbreviation: String = "Coll Posn"

  /** The beam must describe a rectangle at this this tall and wide. */
  private val minSize_mm = 10.0

  private def colPosSelectionList: Seq[Selection] = {
    val list = beamList.filter(beam => Util.minCenteredFieldBeam(beam.prototypeBeam, minSize_mm))
    list.map(beam => Selection(this, beam.beamName, Seq(beam)))
  }

  override def selectionList: Seq[Selection] = colPosSelectionList

  override def setBeamList(beamList: Seq[Beam]): Elem = {
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
