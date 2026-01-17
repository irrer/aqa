package org.aqa.webrun.wl

import org.aqa.Logging

case class WLEdgeSet(top: WLEdge, bottom: WLEdge, left: WLEdge, right: WLEdge) extends Logging {
  def unTop: Double = top.pos_pix
  def unBottom: Double = bottom.pos_pix + (bottom.bounds.y - top.bounds.y)
  def unLeft: Double = left.pos_pix
  def unRight: Double = right.pos_pix + (right.bounds.x - left.bounds.x)

  /** Absolute position (relative to image) of center of box in X axis in pixels. */
  val centerX_pix: Double = (left.absoluteEdge_pix + right.absoluteEdge_pix) / 2
  /** Absolute position (relative to image) of center of box in Y axis in pixels. */
  val centerY_pix: Double = (top.absoluteEdge_pix + bottom.absoluteEdge_pix) / 2

  /** Absolute position (relative to image) of center of box in X axis in mm. */
  val centerX_mm: Double = (left.absoluteEdge_mm + right.absoluteEdge_mm) / 2
  /** Absolute position (relative to image) of center of box in Y axis in mm. */
  val centerY_mm: Double = (top.absoluteEdge_mm + bottom.absoluteEdge_mm) / 2


  override def toString: String = {
    import org.aqa.Util.fmtDbl
    s"top: ${fmtDbl(unTop)}    bottom: ${fmtDbl(unBottom)}    left: ${fmtDbl(unLeft)}    right: ${fmtDbl(unRight)}    "
  }
}
