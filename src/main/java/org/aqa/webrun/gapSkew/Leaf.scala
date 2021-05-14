package org.aqa.webrun.gapSkew

import org.aqa.Util

import javax.vecmath.Point2d

/**
  * Describe one leaf position and the profile surrounding it.
  * @param position Position of leaf.
  * @param profile X,Y graph to plot showing the profile.
  */
case class Leaf(position: Double, profile: Seq[Point2d]) {
  override def toString = position + " : " + profile.map(p => Util.fmtDbl(p.getX) + ", " + Util.fmtDbl(p.getY)).mkString("    ")
}
