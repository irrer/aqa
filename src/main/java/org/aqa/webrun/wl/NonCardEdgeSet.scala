package org.aqa.webrun.wl

import org.aqa.AQALine

import javax.vecmath.Point2d

/**
 * Represent the four edges of a WL image with a non-cardinal collimator angle.
 * @param X1 X1 edge
 * @param X2 X2 edge
 * @param Y1 Y1 edge
 * @param Y2 Y2 edge
 */
case class NonCardEdgeSet(
    X1: WLNonCardEdge,
    X2: WLNonCardEdge,
    Y1: WLNonCardEdge,
    Y2: WLNonCardEdge
) {
  /**
   * Center of the four edges.
   */
  val center: Point2d = {
    val xLine = AQALine.makeLine(X1.edgeCenter, X2.edgeCenter)
    val yLine = AQALine.makeLine(Y1.edgeCenter, Y2.edgeCenter)

    xLine.intersection(yLine)
  }

  override def toString: String = {
    def fmt(d: Double) = "%20.10f".format(d).trim
    def fmtP(p: Point2d) = fmt(p.getX) + ", " + fmt(p.getY)
    def fmtE(e: WLNonCardEdge) = fmtP(e.edgeCenter)

    "\nX1: " + fmtE(X1) +
      "\nX2: " + fmtE(X2) +
      "\nY1: " + fmtE(Y1) +
      "\nY2: " + fmtE(Y2) +
      "\nCenter: " + fmt(center.getX) + ", " + fmt(center.getY)
  }
}
