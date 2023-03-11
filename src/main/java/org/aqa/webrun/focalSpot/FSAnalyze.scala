package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util

case class FSAnalyze(rtplan: AttributeList, rtimageList: Seq[AttributeList]) {

  private def NominalBeamEnergyOf(rtimage: AttributeList): Double = {
    val beam = Util.getBeamOfRtimage(plan = rtplan, rtimage).get
    val NominalBeamEnergy = DicomUtil.findAllSingle(beam, TagByName.NominalBeamEnergy).head.getDoubleValues.head
    NominalBeamEnergy
  }

  private def isValidSet(measureList: Seq[FSMeasure]): Boolean = {
    // @formatter:off
    measureList.size == 4                                                  &&
    measureList.exists(m => measureList.exists(m => m.isJaw && m.is090 )) &&
    measureList.exists(m => measureList.exists(m => m.isJaw && m.is270 )) &&
    measureList.exists(m => measureList.exists(m => m.isMLC && m.is090 )) &&
    measureList.exists(m => measureList.exists(m => m.isMLC && m.is270 ))
    // @formatter:on
  }

  private def toSet(measureList: Seq[FSMeasure]): FSSet = {
    FSSet(
      jaw090 = measureList.find(m => m.isJaw && m.is090).get,
      jaw270 = measureList.find(m => m.isJaw && m.is270).get,
      mlc090 = measureList.find(m => m.isMLC && m.is090).get,
      mlc270 = measureList.find(m => m.isMLC && m.is270).get
    )
  }

  // measure edges for each image
  val measureList = rtimageList.map(rtimage => FSMeasure(rtplan, rtimage))

  // make sets of four based on energy
  val setList: Seq[FSSet] = {
    val groupList = measureList.groupBy(m => NominalBeamEnergyOf(m.rtimage)).values.toSeq
    groupList.filter(isValidSet).map(toSet)
  }

}
