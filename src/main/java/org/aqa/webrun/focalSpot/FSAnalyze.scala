package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList

case class FSAnalyze(rtplan: AttributeList, rtimageList: Seq[AttributeList]) {

  private val maxEpidRange_mm = 1.0

  private def isValidSet(measureList: Seq[FSMeasure]): Boolean = {
    def epidRange: Double = {
      val list = measureList.map(_.RTImageSID_mm)
      list.max - list.min
    }
    // @formatter:off
    measureList.size == 4                       &&
    measureList.exists(m => m.isJaw && m.is090) &&
    measureList.exists(m => m.isJaw && m.is270) &&
    measureList.exists(m => m.isMLC && m.is090) &&
    measureList.exists(m => m.isMLC && m.is270) &&
    epidRange < maxEpidRange_mm   // must have similar epid positions.
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

  //noinspection ScalaWeakerAccess
  // measure edges for each image
  val measureList: Seq[FSMeasure] = rtimageList.map(rtimage => FSMeasure(rtplan, rtimage))

  // make sets of four based on energy
  val setList: Seq[FSSet] = {
    val groupList = measureList.groupBy(_.NominalBeamEnergy).values.toSeq
    groupList.filter(isValidSet).map(toSet)
  }

}
