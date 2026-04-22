package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.Util

case class Analysis( //
    dicomImage: DicomImage,
    rtimage: AttributeList,
                     xAoiBorders:   Seq[  XAoiBorders],
    yLeafBoundaries: LeafBoundaries,
    leafEndBySinglePixel: LeafEndBySinglePixel,
    stakittList: Seq[StakittResult],
    planBorders: PlanBorders
) extends Logging {

  //
}

object Analysis extends Logging {

  /**
    * Check that the number of edges found match the count in the plan, and also that
    * the number of leaf boundaries on the left and right are the same.  If anything
    * is wrong, then throw an exception.
    *
    * TODO Instead of an exception, return a bad ProcedureStatus
    *
    * @param planAOIBorders Boundaries from rtplan.
    * @param xAOIBorders List of leaf ends found in the image.
    * @param yLeafBoundaries List of leaf boundaries (sides) found in the image.
    */
  private def validateBoundaries(
      planAOIBorders: PlanBorders,
      xAOIBorders: Seq[Double],
      yLeafBoundaries: LeafBoundaries
  ): Unit = {

    if (yLeafBoundaries.yPointListLo_pix.adjusted_pix.size != yLeafBoundaries.yPointListHi_pix.adjusted_pix.size) {
      val msg = s"Visually found ${yLeafBoundaries.yPointListLo_pix.adjusted_pix.size} lo leaf boundaries (sides) but ${yLeafBoundaries.yPointListHi_pix.adjusted_pix.size} hi leaf boundaries."
      logger.error(msg)
      throw new RuntimeException(msg)
    }

    if (xAOIBorders.size != planAOIBorders.xLeafEndList.size) {
      val msg = s"Visually found ${xAOIBorders.size} leaf ends, but plan indicates that there should be ${planAOIBorders.xLeafEndList.size}"
      logger.error(msg)
      throw new RuntimeException(msg)
    }

    if (yLeafBoundaries.yPointListLo_pix.adjusted_pix.size != planAOIBorders.yLeafBoundaryList.size) {
      val msg = s"Visually found ${yLeafBoundaries.yPointListLo_pix.adjusted_pix.size} leaf boundaries (sides), but plan indicates that there should be ${planAOIBorders.yLeafBoundaryList.size}"
      logger.error(msg)
      throw new RuntimeException(msg)
    }

  }

  def analyze(extendedData: ExtendedData, rtimage: AttributeList, rtplan: AttributeList): Analysis = {

    val dicomImage = new DicomImage(rtimage)
    val trans = new IsoImagePlaneTranslator(rtimage)

    val planBorders = PlanBorders.make(rtimage, rtplan)
    val xAOIBorders = LeafEnds.xPointList(dicomImage)
    val yLeafBoundaries = LeafBoundaries(rtimage, xAOIBorders)

    validateBoundaries(planBorders, xAOIBorders, yLeafBoundaries)

    val aoiList = StakittAOI.makeAOIs(xAOIBorders, yLeafBoundaries, rtimage, planBorders.xLeafEndList)
    val xAoiPairList = XAoiBorders.makeXPairList(xAOIBorders)
    val leafEndBySinglePixel = LeafEndBySinglePixel(xAoiPairList, yLeafBoundaries, trans, dicomImage)

    val beamName = Util.getBeamNameOfRtimage(rtplan, rtimage).get
    val rtimageSOP = Util.sopOfAl(rtimage)

    val stakittList = StakittResult.constructStakittList( //
      aoiList,
      leafEndBySinglePixel,
      trans,
      extendedData,
      beamName,
      rtimageSOP,
      planBorders
    )

    val analysis = Analysis(dicomImage, rtimage, xAoiPairList, yLeafBoundaries, leafEndBySinglePixel, stakittList, planBorders)

    analysis
  }

}
