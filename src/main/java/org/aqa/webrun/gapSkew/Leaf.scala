package org.aqa.webrun.gapSkew

import org.aqa.Util

import javax.vecmath.Point2d

/**
  * Describe one leaf position and the profile surrounding it.
  * @param position_mm Position of leaf in mm in isoplane.
  * @param xPosition_mm Left side of measurement in mm in isoplane.
  * @param width_mm Width of measurement (distance across leaf) in mm in isoplane.
  * @param profile X,Y graph to plot showing the profile.
  */
case class Leaf(position_mm: Double, xPosition_mm: Double, width_mm: Double, profile: Seq[ProfilePoint]) {
  override def toString =
    position_mm +
      "   x, width (mm): " + xPosition_mm + ", " + width_mm +
      "    profile: " + profile.map(p => Util.fmtDbl(p.y_mm) + ", " + Util.fmtDbl(p.cu)).mkString("    ")
}
