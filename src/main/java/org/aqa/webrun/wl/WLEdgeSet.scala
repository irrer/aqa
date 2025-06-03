package org.aqa.webrun.wl

import org.aqa.Logging

case class WLEdgeSet(top: WLEdge, bottom: WLEdge, left: WLEdge, right: WLEdge)extends Logging {
  def unTop: Double = top.pos_pix
  def unBottom: Double = bottom.pos_pix + (bottom.bounds.y - top.bounds.y)
  def unLeft: Double = left.pos_pix
  def unRight: Double = right.pos_pix + (right.bounds.x - left.bounds.x)

}
