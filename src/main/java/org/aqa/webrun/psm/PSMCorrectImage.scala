package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import org.aqa.db.PSMBeam

import scala.annotation.tailrec

case class PSMCorrectImage(psmList: Seq[PSMBeam]) {

  /**
    * Put the beams into a 2-dimensional array that is the same as their spacial layout.
    * @return Spatially sorted beams.
    */
  private def layoutSpatially: Seq[Seq[PSMBeam]] = {

    case class Row(beamList: Seq[PSMBeam]) {
      def xSorted: Seq[PSMBeam] = beamList.sortBy(_.xCenter_mm)
    }

    @tailrec
    def build(beamList: Seq[PSMBeam], rowList: Seq[Row] = Seq()): Seq[Row] = {

      /** Centers (either X or Y) must be this close in mm to be considered to be in the same row or column. */
      val tolerance_mm = 2.5

      /**
       * Determine if beams are in the same column.
       * @param a one beam
       * @param b the other beam
       * @return True if they are close together in the Y axis.
       */
      def yProximal(a: PSMBeam, b: PSMBeam): Boolean = (a.yCenter_mm - b.yCenter_mm).abs < tolerance_mm

      if (beamList.isEmpty)
        rowList
      else {
        val sortedByY = beamList.sortBy(_.yCenter_mm)
        val first = beamList.minBy(_.yCenter_mm)

        val newRow = Row(sortedByY.takeWhile(beam => yProximal(first, beam)))

        build(beamList.drop(newRow.beamList.size), rowList :+ newRow)
      }
    }

    val rowList = build(psmList)

    val sorted = rowList.sortBy(_.beamList.head.yCenter_mm).map(_.xSorted)

    sorted
  }

  def correctImage(image: DicomImage): DicomImage = {
    val sorted = layoutSpatially

    new DicomImage(IndexedSeq(IndexedSeq(1.0.toFloat))) // TODO
  }

}
