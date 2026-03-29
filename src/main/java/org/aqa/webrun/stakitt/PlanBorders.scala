package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.phase2.Phase2Util

case class PlanBorders(rtimage: AttributeList, rtplan: AttributeList) extends Logging {

  private val trans = new IsoImagePlaneTranslator(rtimage)

  private val dicomImage = new DicomImage(rtimage)

  // beam's attribute list
  private val beam: AttributeList = {
    val beamNumber = DicomUtil.findAllTag(rtplan, TagByName.BeamNumber).head.getIntegerValues.head
    Phase2Util.getBeamSequence(rtplan, beamNumber)
  }

  // private val beamName: String = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage).get

  private case class AOIBorder(lo: Double, hi: Double) {}

  private case class AOIBorderList(staggeredLo: AOIBorder, smooth: Seq[AOIBorder], staggeredHi: AOIBorder) {}

  /**
    * Make a list of planned edges, including the lo and hi staggered edges.
    * @return List of edges.
    */
  private def findPlanXBorderList(plan: AttributeList): AOIBorderList = {
    // make a list of all leaf ends
    val sortedEdgeList = DicomUtil.findAllTag(beam, TagByName.LeafJawPositions).flatMap(_.getDoubleValues).distinct.sorted

    val loStaggered: AOIBorder = {
      // left-hand edge of the EPID
      val lo = trans.pix2IsoCoordX(0)

      // midway between staggered and straight edge
      val hi = (sortedEdgeList(1) + sortedEdgeList(2)) / 2

      AOIBorder(lo, hi)
    }

    val hiStaggered: AOIBorder = {
      val rev = sortedEdgeList.reverse
      // midway between staggered and straight edge
      val lo = (rev(1) + rev(2)) / 2

      // right-hand edge of the EPID
      val hi = trans.pix2IsoCoordX(0)

      AOIBorder(lo, hi)
    }

    // number of AOIs in the middle
    val count = (sortedEdgeList.size - 6) / 2

    // given an index, make the X borders to be used for AOIs.
    def makeAOIBorder(i: Int): AOIBorder = {
      val ii = (i * 2) + 1
      val lo = (sortedEdgeList(ii + 0) + sortedEdgeList(ii + 1)) / 2
      val hi = (sortedEdgeList(ii + 2) + sortedEdgeList(ii + 2)) / 2
      AOIBorder(lo, hi)
    }

    val list = (0 until count).map(makeAOIBorder)

    AOIBorderList(loStaggered, list, hiStaggered)
  }

  private def planYBorderList = {

  }

}
