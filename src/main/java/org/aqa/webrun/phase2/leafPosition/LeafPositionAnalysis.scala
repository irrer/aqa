package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.db.LeafPosition
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import java.awt.geom.Point2D
import org.aqa.webrun.phase2.MeasureTBLREdges

object LeafPositionAnalysis extends Logging {

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  private def leafSides(horizontal: Boolean, attributeList: AttributeList, image: DicomImage): Seq[Double] = {
    val profile = if (horizontal) image.columnSums else image.rowSums
    val translator = new IsoImagePlaneTranslator(attributeList)

    // distance in pixels from the furthest sides of the leaves
    val leafSideRange = {
      val x1x2y1y2 = MeasureTBLREdges.imageCollimatorPositions(attributeList)
      val point1 = translator.iso2Pix(new Point2D.Double(x1x2y1y2.X1, x1x2y1y2.Y1))
      val point2 = translator.iso2Pix(new Point2D.Double(x1x2y1y2.X2, x1x2y1y2.Y2))
      val pair = if (horizontal) Seq(point1.getY, point2.getY) else Seq(point1.getX, point2.getX)
      pair.sorted
    }

    val indexedProfile = profile.zipWithIndex.filter(ip => ip._2 > leafSideRange(0) && ip._2 < leafSideRange(1))
    val oneEighth = (leafSideRange(1) - leafSideRange(0)) / 8

    def isInLoRange(i: Int): Boolean = {
      (i > (leafSideRange(0) + oneEighth)) &&
        (i < (leafSideRange(0) + oneEighth * 3))
    }

    def isInHiRange(i: Int): Boolean = {
      (i > (leafSideRange(0) + oneEighth * 5)) &&
        (i < (leafSideRange(0) + oneEighth * 7))
    }

    val hiA = indexedProfile.filter(ip => isInLoRange(ip._2)).maxBy(ip => ip._1)
    val hiB = indexedProfile.filter(ip => isInHiRange(ip._2)).maxBy(ip => ip._1)
    val highPoint = Math.max(hiA._1, hiB._1)

    def isBetweenHiAHiB(i: Int) = (i >= hiA._2) && (hiB._2 >= i)

    def loPoint = indexedProfile.filter(ip => isBetweenHiAHiB(ip._2)).minBy(pi => pi._1)._1
    ???
  }

  private def measureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage): LeafPosition = {

    // true if collimator is horizontal
    val horizontal = (Util.angleRoundedTo90(attributeList.get(TagFromName.BeamLimitingDeviceAngle).getDoubleValues.head).toInt % 180) == 0

    ???
  }

  /**
   * Hook for testing
   */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage): LeafPosition = measureBeam(beamName, outputPK, attributeList, image)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, LeafPositionResult] = {
    ???
  }

}