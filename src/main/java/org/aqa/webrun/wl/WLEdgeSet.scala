package org.aqa.webrun.wl

case class WLEdgeSet(top: WLEdge, bottom: WLEdge, left: WLEdge, right: WLEdge) {
  private val list = Seq(top, bottom, left, right)

  val status: WLImageStatus.Value = {
    val failures = list.filter(_.edge.isLeft)
    if (failures.isEmpty)
      WLImageStatus.Passed
    else
      failures.head.edge.left.get
  }

  def unTop: Double = top.pos
  def unBottom: Double = bottom.pos + (bottom.bounds.y - top.bounds.y)
  def unLeft: Double = left.pos
  def unRight: Double = right.pos + (right.bounds.x - left.bounds.x)

}
