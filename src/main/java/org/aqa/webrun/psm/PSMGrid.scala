package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import org.aqa.db.PSMBeam

import java.awt.image.BufferedImage
import java.awt.Point
import javax.vecmath.Point2d

/**
 * Organize beams into a two-dimensional grid.
 * @param resultList List of beam measurements.
 */
case class PSMGrid(resultList: Seq[PSMBeamAnalysisResult]) {

  /** Centers (either X or Y) must be this close in mm to be considered to be in the same row or column. */
  private val tolerance_mm = 5.0

  private def xProximal(a: PSMBeamAnalysisResult, b: PSMBeamAnalysisResult): Boolean = (a.psmBeam.xCenter_mm - b.psmBeam.xCenter_mm).abs < tolerance_mm

  private def yProximal(a: PSMBeamAnalysisResult, b: PSMBeamAnalysisResult): Boolean = (a.psmBeam.yCenter_mm - b.psmBeam.yCenter_mm).abs < tolerance_mm

  private val xCoordinates = resultList.foldLeft(Seq[PSMBeamAnalysisResult]())((seq, r) => if (seq.exists(s => xProximal(s, r))) seq else seq :+ r).sortBy(_.psmBeam.xCenter_mm)

  private val yCoordinates = resultList.foldLeft(Seq[PSMBeamAnalysisResult]())((seq, r) => if (seq.exists(s => yProximal(s, r))) seq else seq :+ r).sortBy(_.psmBeam.yCenter_mm)

  /** Maximum number of results in X direction. */
  val width: Int = xCoordinates.size

  /** Maximum number of results in Y direction. */
  val height: Int = yCoordinates.size

  private def findResult(x: Int, y: Int): Option[PSMBeamAnalysisResult] = {
    resultList.find(r => xProximal(r, xCoordinates(x)) && yProximal(r, yCoordinates(y)))
  }

  /**
    * A two-dimensional array containing all results, positionally sorted vertically and horizontally.  If there
    * is no result for the given position, then it will be None.
    */
  val grid: Seq[Seq[Option[PSMBeamAnalysisResult]]] = {
    def makeRow(y: Int): Seq[Option[PSMBeamAnalysisResult]] = (0 until width).map(x => findResult(x, y))

    (0 until height).map(makeRow)
  }

  def get(x: Int, y: Int): Option[PSMBeamAnalysisResult] = grid(y)(x)

  /** True if there are no empty spots in the grid. */
  val canBeInterpolated: Boolean = grid.flatten.flatten.size == (width * height)

  // ----------------------------------------------------------------------------------------------------------------------------------------

  /** The beam that */
  private val centerPsmBeam: PSMBeam = {

    val zero = new Point2d(0, 0)

    val c = resultList.minBy(p => p.psmBeam.center.distance(zero))
    c.psmBeam
  }

  /**
    * The grid coordinates of the center point.
    */
  private val centerGridPoint: Point = {
    def coordinatesMatch(x: Int, y: Int): Boolean = {
      get(x, y) match {
        case Some(r) => (r.psmBeam.xCenter_mm == centerPsmBeam.xCenter_mm) && (r.psmBeam.yCenter_mm == centerPsmBeam.yCenter_mm)
        case _       => false
      }
    }

    val pointList = for (x <- 0 until width; y <- 0 until height; if coordinatesMatch(x, y)) yield new Point(x, y)
    pointList.head
  }

  // @formatter:off
  private val topDiff    =          centerGridPoint.y
  private val bottomDiff = height - centerGridPoint.y - 1
  private val leftDiff   =          centerGridPoint.x
  private val rightDiff  = width  - centerGridPoint.x - 1

  private val diff = Seq(topDiff, bottomDiff, leftDiff, rightDiff).min

  val centerBeam: PSMBeam = get(centerGridPoint.x       , centerGridPoint.y       ).get.psmBeam
  val topBeam:    PSMBeam = get(centerGridPoint.x       , centerGridPoint.y - diff).get.psmBeam
  val bottomBeam: PSMBeam = get(centerGridPoint.x       , centerGridPoint.y + diff).get.psmBeam
  val leftBeam:   PSMBeam = get(centerGridPoint.x - diff, centerGridPoint.y       ).get.psmBeam
  val rightBeam:  PSMBeam = get(centerGridPoint.x + diff, centerGridPoint.y       ).get.psmBeam

  private val vertSpan: Double = (bottomBeam.yCenter_mm - topBeam. yCenter_mm).abs
  private val horzSpan: Double = (rightBeam .xCenter_mm - leftBeam.xCenter_mm).abs
  // @formatter:on

  val span: Double = Math.min(vertSpan, horzSpan)

  // ----------------------------------------------------------------------------------------------------------------------------------------

  override def toString: String = {

    val fmt = "%6.2f"

    def rToText(r: Option[PSMBeamAnalysisResult]): String = {
      if (r.isDefined) {
        val beam = r.get.psmBeam

        beam.xCenter_mm.formatted(fmt) + ", " +
          beam.yCenter_mm.formatted(fmt) + ": " +
          beam.mean_cu.formatted(fmt)
      } else
        "                      "
    }

    def doRow(row: Seq[Option[PSMBeamAnalysisResult]]): String = {
      row.map(rToText).mkString(" | ")
    }

    val xList = "x Coordinates: " + xCoordinates.map(_.psmBeam.xCenter_mm.formatted(fmt)).mkString("  ")
    val yList = "y Coordinates: " + yCoordinates.map(_.psmBeam.yCenter_mm.formatted(fmt)).mkString("  ")

    val arrayText = grid.map(doRow).mkString("\n")

    Seq(xList, yList, arrayText).mkString("\n")
  }

}

object PSMGrid {

  /**
   * Make a result, faking parameters not needed.
   *
   * @param psmBeam Contains the real data.
   * @return A fully defined result.
   */
  private def beamToResult(psmBeam: PSMBeam): PSMBeamAnalysisResult = PSMBeamAnalysisResult(psmBeam, new AttributeList, new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), Map())

  def makePSMGrid(psmBeamList: Seq[PSMBeam]): PSMGrid = PSMGrid(psmBeamList.map(PSMGrid.beamToResult))
}
