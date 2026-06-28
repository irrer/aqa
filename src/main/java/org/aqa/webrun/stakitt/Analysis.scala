package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.run.ProcedureStatus.ProcedureStatus

/**
  * Container for results of analysis.
  * @param dicomImage Stakitt image.
  * @param rtimage Stakitt DICOM.
  * @param xAoiBorders Low and high limits in X axis for AOIs.
  * @param yLeafBoundaries Measured leaf boundaries.
  * @param leafEndBySinglePixel Measured position of leaf ends for each row of pixels.
  * @param stakittList List of DB records.
  * @param planBorders Planned leaf ends and leaf boundaries.
  */
case class Analysis( //
    dicomImage: DicomImage,
    rtimage: AttributeList,
    xAoiBorders: Seq[XAoiBorders],
    yLeafBoundaries: LeafBoundaries,
    leafEndBySinglePixel: LeafEndBySinglePixel,
    stakittList: Seq[StakittResult],
    planBorders: PlanBorders
) extends Logging {

  /** Results sorted into rows, with each row sorted by index.. */
  val resultRows: Seq[Seq[StakittResult]] = stakittList.groupBy(_.stakittAOI.yIndex).values.toSeq.sortBy(_.head.stakittAOI.yIndex).map(_.sortBy(_.stakittAOI.xIndex))

  /** Results sorted into columns, with each column sorted by index.. */
  val resultColumns: Seq[Seq[StakittResult]] = stakittList.groupBy(_.stakittAOI.xIndex).values.toSeq.sortBy(_.head.stakittAOI.xIndex).map(_.sortBy(_.stakittAOI.yIndex))

  val x1BankResultList: Seq[StakittResult] = stakittList.filter(r => (r.stakittAOI.xIndex % 2) == 0)
  val x2BankResultList: Seq[StakittResult] = stakittList.filter(r => (r.stakittAOI.xIndex % 2) == 1)

  private def findPair(result: StakittResult): Option[StakittGap] = {
    val bXIndex = result.stakittAOI.xIndex + 1
    val yIndex = result.stakittAOI.yIndex
    val mate = x2BankResultList.find(r => (r.stakittAOI.yIndex == yIndex) && (r.stakittAOI.xIndex == bXIndex))
    if (mate.isDefined)
      Some(StakittGap(result, mate.get))
    else
      None
  }

  private val gapList: Seq[StakittGap] = x1BankResultList.flatMap(a => findPair(a))

  /** Gaps sorted by column then row */
  val gapColumns: Seq[Seq[StakittGap]] = {
    gapList
      .groupBy(_.x1.stakitt.plannedEndPosition_mm)
      .values
      .toSeq
      .sortBy(_.head.x1.stakitt.plannedEndPosition_mm)
      .map(col => col.sortBy(_.x1.stakitt.plannedMajorSide_mm))
  }

  /** Gaps sorted by row then column */
  val gapRows: Seq[Seq[StakittGap]] = {
    gapList
      .groupBy(_.x1.stakitt.plannedMinorSide_mm)
      .values
      .toSeq
      .sortBy(_.head.x1.stakitt.plannedMinorSide_mm)
      .map(col => col.sortBy(_.x1.stakitt.plannedEndPosition_mm))
  }
}

object Analysis extends Logging {

  /**
    * Used to report errors.
    * @param status Failure status.
    * @param msg Description of error.
    */
  case class Failure(status: ProcedureStatus, msg: String, rtimage: AttributeList) {}

  /**
    * Check that the number of edges found match the count in the plan, and also that
    * the number of leaf boundaries on the left and right are the same.  If anything
    * is wrong, then throw an exception.
    *
    * @param planAOIBorders Boundaries from rtplan.
    * @param xAOIBorders List of leaf ends found in the image.
    * @param yLeafBoundaries List of leaf boundaries (sides) found in the image.
    * @return Failure if there is a problem, otherwise None
    */
  private def validateBoundaries(
      planAOIBorders: PlanBorders,
      xAOIBorders: Seq[Double],
      yLeafBoundaries: LeafBoundaries,
      rtimage: AttributeList
  ): Option[Failure] = {

    val rightLeftMismatch = if (yLeafBoundaries.yPointListLo_pix.adjusted_pix.size != yLeafBoundaries.yPointListHi_pix.adjusted_pix.size) {
      val msg = s"Image analysis found ${yLeafBoundaries.yPointListLo_pix.adjusted_pix.size} lo leaf boundaries (sides) but ${yLeafBoundaries.yPointListHi_pix.adjusted_pix.size} hi leaf boundaries."
      logger.error(msg)
      Some(Failure(ProcedureStatus.invalidData, msg, rtimage))
    } else
      None

    val leafEndCountMismatch = if (xAOIBorders.size != planAOIBorders.xLeafEndList.size) {
      val msg = s"Image analysis found ${xAOIBorders.size} leaf ends, but plan indicates that there should be ${planAOIBorders.xLeafEndList.size}"
      logger.error(msg)
      Some(Failure(ProcedureStatus.invalidData, msg, rtimage))
    } else
      None

    val imageVersesPlanMismatch = if (yLeafBoundaries.yPointListLo_pix.adjusted_pix.size != planAOIBorders.yLeafBoundaryList.size) {
      val msg = s"Image analysis found ${yLeafBoundaries.yPointListLo_pix.adjusted_pix.size} leaf boundaries (sides), but plan indicates that there should be ${planAOIBorders.yLeafBoundaryList.size}"
      logger.error(msg)
      Some(Failure(ProcedureStatus.invalidData, msg, rtimage))
    } else
      None

    Seq(rightLeftMismatch, leafEndCountMismatch, imageVersesPlanMismatch).flatten.headOption
  }

  /**
    * Analyze one Stakitt image.
    * @param extendedData Metadata.
    * @param rtimage DICOM image.
    * @param rtplan DICOM plan.
    * @return result or error
    */
  def analyze(extendedData: ExtendedData, rtimage: AttributeList, rtplan: AttributeList): Either[Failure, Analysis] = {

    val dicomImage = new DicomImage(rtimage)
    val trans = new IsoImagePlaneTranslator(rtimage)

    val planBorders = PlanBorders.make(rtimage, rtplan)
    val xAOIBorders = LeafEnds.xPointList(dicomImage)
    val yLeafBoundaries = LeafBoundaries(rtimage, xAOIBorders)

    val failure = validateBoundaries(planBorders, xAOIBorders, yLeafBoundaries, rtimage)

    if (failure.isDefined)
      Left(failure.get) // further analysis at this point would yield wonky results
    else {
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

      Right(analysis)
    }
  }

}
