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
}
