package org.aqa.webrun.stakitt

import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Logging
import org.aqa.db.Stakitt
import org.aqa.webrun.ExtendedData
import org.aqa.Config

object ConstructStakitt extends Logging {

  /**
    * Given all the necessary components, make rows for the database.
    * @param stakittAOI AOI for a leaf end.
    * @param leafEndBySinglePixel Position for the leaf's end for every row of pixels.
    * @param trans mm <--> pixel translation
    * @param extendedData metadata
    * @param beamName Name of beam
    * @param rtimageSOP UID of RTIMAGE
    * @param planBorders Expected edge positions.
    * @return DB rows.
    */
  def constructStakitt( //
      stakittAOI: StakittAOI,
      leafEndBySinglePixel: LeafEndBySinglePixel,
      trans: IsoImagePlaneTranslator,
      extendedData: ExtendedData,
      beamName: String,
      rtimageSOP: String,
      planBorders: PlanBorders
  ): StakittResult = {
    val edgePosition_pix = stakittAOI.measureLeafEnd(leafEndBySinglePixel)
    val edgePosition_mm = trans.pix2IsoCoordX(edgePosition_pix)

    val stakitt = Stakitt( //
      stakittPK = None,
      outputPK = extendedData.outputPK,
      SOPInstanceUID = rtimageSOP,
      beamName = beamName,
      leafIndex = stakittAOI.yIndex + 1,
      leafPositionIndex = stakittAOI.xIndex + 1,
      measuredEndPosition_mm = edgePosition_mm,
      measuredMinorSide_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y) - Config.StakittVerticalMargin_mm,
      measuredMajorSide_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y + stakittAOI.rectangle.height) + (Config.StakittVerticalMargin_mm * 2),
      plannedEndPosition_mm = stakittAOI.plannedXEdge_mm,
      plannedMinorSide_mm = planBorders.yLeafBoundaryList(stakittAOI.yIndex),
      plannedMajorSide_mm = planBorders.yLeafBoundaryList(stakittAOI.yIndex + 1)
    )

    val result = StakittResult(stakitt, stakittAOI)
    result
  }

}
