package org.aqa.webrun.stakitt

import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.db.Stakitt
import org.aqa.Config
import org.aqa.Logging
import org.aqa.webrun.ExtendedData

import java.awt.geom.Point2D

case class StakittResult(stakitt: Stakitt, stakittAOI: StakittAOI) {}

object StakittResult extends Logging {

  /**
    * Given an AOI, measure the leaf end and make a row for the database.
    *
    * @param stakittAOIList List of all AOIs
    * @param leafEndBySinglePixel Position for the leaf's end for every row of pixels.
    * @param trans translate mm to and from pixel coordinates
    * @param extendedData Metadata for database
    * @param beamName Name of this beam
    * @param rtimageSOPUID RTIMAGE UID
    * @param planBorders Expected leaf positions specified in RTPLAN.
    * @param collimatorCentering_mm Coordinates of center of collimator.
    *
    * @return DB rows, each with AOI.
    */
  def constructStakittList( //
      stakittAOIList: Seq[StakittAOI],
      leafEndBySinglePixel: LeafEndBySinglePixel,
      trans: IsoImagePlaneTranslator,
      extendedData: ExtendedData,
      beamName: String,
      rtimageSOPUID: String,
      planBorders: PlanBorders,
      collimatorCentering_mm: Point2D.Double
  ): Seq[StakittResult] = {

    def constructOne(stakittAOI: StakittAOI): StakittResult = {
      val edgePosition_pix = stakittAOI.measureLeafEnd(leafEndBySinglePixel)
      val edgePosition_mm = trans.pix2IsoCoordX(edgePosition_pix)

      val stakitt = Stakitt( //
        stakittPK = None,
        outputPK = extendedData.outputPK,
        SOPInstanceUID = rtimageSOPUID,
        beamName = beamName,
        leafIndex = stakittAOI.yIndex + 1,
        leafPositionIndex = stakittAOI.xIndex + 1,
        measuredEndPosition_mm = edgePosition_mm,
        measuredY2Side_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y) - Config.StakittVerticalMargin_mm,
        measuredY1Side_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y + stakittAOI.rectangle.height) + (Config.StakittVerticalMargin_mm * 2),
        collimatorCenterOffsetX_mm = collimatorCentering_mm.getX,
        collimatorCenterOffsetY_mm = collimatorCentering_mm.getY,
        plannedEndPosition_mm = stakittAOI.plannedXEdge_mm,
        plannedY2Side_mm = planBorders.yLeafBoundaryList(stakittAOI.yIndex),
        plannedY1Side_mm = planBorders.yLeafBoundaryList(stakittAOI.yIndex + 1)
      )

      val result = StakittResult(stakitt, stakittAOI)
      result
    }

    stakittAOIList.map(constructOne)
  }

}
