package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.aqa.db.PSMBeam
import org.aqa.Logging

import scala.annotation.tailrec

case class PSMCorrectImage(psmList: Seq[PSMBeam]) extends Logging {

  Trace.trace()

  /**
    * Put the beams into a 2-dimensional array that is the same as their spacial layout.
    * @return Spatially sorted beams.
    */
  private def layoutSpatially(): Seq[Seq[PSMBeam]] = {

    case class Row(beamList: Seq[PSMBeam]) {
      def xSorted: Seq[PSMBeam] = beamList.sortBy(_.xCenter_mm)

      override def toString: String = {
        def b2S(beam: PSMBeam): String = s"""${beam.xCenter_mm.round.formatted("%3d")},${beam.yCenter_mm.round.formatted("%3d")} :: ${beam.mean_cu.formatted("%5.2f")}"""
        s"${xSorted.map(b2S).mkString("     ")}"
      }
    }

    @tailrec
    def build(beamList: Seq[PSMBeam], rowList: Seq[Row] = Seq()): Seq[Row] = {

      /** Centers (either X or Y) must be this close in mm to be considered to be in the same row or column. */
      val tolerance_mm = 5.0

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
        val inOut = beamList.groupBy(b => yProximal(b, beamList.head))
        val row = Row(inOut(true).sortBy(_.xCenter_mm))
        val out: Seq[PSMBeam] = if (inOut.contains(false)) inOut(false) else Seq()
        build(out, rowList :+ row)
      }

    }

    val rowList = build(psmList)

    logger.info("List of sorted values:\n" + rowList.mkString("\n"))

    val sorted = rowList.sortBy(_.beamList.head.yCenter_mm).map(_.xSorted)

    sorted
  }

  def correctImage(image: DicomImage): DicomImage = {
    val sorted = layoutSpatially()

    new DicomImage(IndexedSeq(IndexedSeq(1.0.toFloat))) // TODO
  }

}
