/*
 * Copyright 2025 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.wl.nonCardinal

import org.aqa.AQALine

import javax.vecmath.Point2d

/**
  * Represent the four edges of a WL image with a non-cardinal collimator angle.
  * @param X1 X1 edge
  * @param X2 X2 edge
  * @param Y1 Y1 edge
  * @param Y2 Y2 edge
  */
case class WLNonCardEdgeSet(
    X1: WLNonCardEdge,
    X2: WLNonCardEdge,
    Y1: WLNonCardEdge,
    Y2: WLNonCardEdge
) {

  private def meanLineOf(edge1: WLNonCardEdge, edge2: WLNonCardEdge): AQALine = {
    val center1 = edge1.edgeLine.centerPoint
    val center2 = edge2.edgeLine.centerPoint

    val x = (center1.getX + center2.getX) / 2
    val y = (center1.getY + center2.getY) / 2

    new AQALine(new Point2d(x, y), edge1.line.perpendicularAngle)
  }

  /** Line parallel to ana halfway between collimator edges X1 and X2.  */
  val xMeanLine: AQALine = meanLineOf(X1, X2)

  /** Line parallel to ana halfway between collimator edges Y1 and Y2.  */
  val yMeanLine: AQALine = meanLineOf(Y1, Y2)

  /**
    * Center of the four edges.
    */
  val center_pix: Point2d = {
    val c = xMeanLine.intersection(yMeanLine)
    c
  }

  val edgeList: Seq[WLNonCardEdge] = Seq(X1, X2, Y1, Y2)

  /** Where lines intersect. */
  val x1y1: Point2d = X1.edgeLine.intersection(Y1.edgeLine)

  /** Where lines intersect. */
  val x1y2: Point2d = X1.edgeLine.intersection(Y2.edgeLine)

  /** Where lines intersect. */
  val x2y1: Point2d = X2.edgeLine.intersection(Y1.edgeLine)

  /** Where lines intersect. */
  val x2y2: Point2d = X2.edgeLine.intersection(Y2.edgeLine)

  /** list of intersection points */
  val intersectList: Seq[Point2d] = Seq(x1y1, x1y2, x2y1, x2y2)

  override def toString: String = {
    def fmt(d: Double) = "%20.10f".format(d).trim
    def fmtP(p: Point2d) = fmt(p.getX) + ", " + fmt(p.getY)
    def fmtE(e: WLNonCardEdge) = fmtP(e.edgeCenter)

    "\nX1: " + fmtE(X1) +
      "\nX2: " + fmtE(X2) +
      "\nY1: " + fmtE(Y1) +
      "\nY2: " + fmtE(Y2) +
      "\nCenter: " + fmt(center_pix.getX) + ", " + fmt(center_pix.getY)
  }
}
