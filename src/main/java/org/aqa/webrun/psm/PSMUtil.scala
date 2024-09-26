package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config

import java.awt.geom.Point2D
import javax.vecmath.Point2i
import scala.annotation.tailrec

object PSMUtil {

  /**
    * Get a list of pixel coordinates that are within the PSM radius of the given center.
    * @param rtimage for this DICOM image.
    * @param center_pix Center in pixel coordinates.
    * @return List of pixel coordinates.
    */
  def pixelCoordinatesWithinRadius(rtimage: AttributeList, center_pix: Point2D.Double): Seq[Point2i] = {
    val trans = new IsoImagePlaneTranslator(rtimage)

    val dicomImage = new DicomImage(rtimage)

    val center_iso = trans.pix2Iso(center_pix)

    def isNear(x: Int, y: Int): Boolean = {
      val point_pix = new Point2D.Double(x, y)
      val point_iso = trans.pix2Iso(point_pix)
      center_iso.distance(point_iso) <= Config.PSMRadius_mm
    }

    val xDist_pix = trans.iso2PixDistX(Config.PSMRadius_mm) + 3
    val yDist_pix = trans.iso2PixDistY(Config.PSMRadius_mm) + 3

    val list =
      for (
        x <- (center_pix.getX - xDist_pix).toInt to (center_pix.getX + xDist_pix).toInt;
        y <- (center_pix.getY - yDist_pix).toInt to (center_pix.getY + yDist_pix).toInt;
        if (x >= 0) && (y >= 0) && (x < dicomImage.width) && (y < dicomImage.height) && isNear(x, y)
      ) yield new Point2i(x, y)

    list
  }

  /**
    * Group the results as an array.
    * @param resultList List of beam results.
    * @return
    */
  def sortByXYLocation(resultList: Seq[PSMBeamAnalysisResult]): Seq[Seq[PSMBeamAnalysisResult]] = {
    val tolerance_mm = 5.0

    @tailrec
    def findRow(done: Seq[Seq[PSMBeamAnalysisResult]], todo: Seq[PSMBeamAnalysisResult]): Seq[Seq[PSMBeamAnalysisResult]] = {

      def belongsInRow(resultList: Seq[PSMBeamAnalysisResult], result: PSMBeamAnalysisResult): Boolean = {
        val yList = resultList.map(_.psmBeam.yCenter_mm)
        val mean = yList.sum / yList.size

        (result.psmBeam.yCenter_mm - mean) < tolerance_mm
      }

      def isRow(a: Seq[PSMBeamAnalysisResult], b: Seq[PSMBeamAnalysisResult]): Boolean = {
        val aSop = a.map(_.psmBeam.SOPInstanceUID)
        val bSop = b.map(_.psmBeam.SOPInstanceUID)
        val is = aSop.intersect(bSop).nonEmpty
        is
      }

      if (todo.isEmpty)
        done
      else {
        val next = todo.head
        done.find(row => belongsInRow(row, next)) match {
          case Some(r) =>
            val doneHavingRowRemoved = done.filterNot(row => isRow(row, r))
            val newDone = doneHavingRowRemoved :+ (r :+ next)
            findRow(newDone, todo.tail)
          case _ => findRow(done :+ Seq(next), todo.tail)
        }
      }
    }

    // get the list of rows,
    val rowList =
      findRow(Seq(), resultList). // groups results into rows (vertical separation)
      map(_.sortBy(_.psmBeam.xCenter_mm)). // sort each row in X direction
      sortBy(_.head.psmBeam.yCenter_mm) // sort each row in Y direction

    rowList
  }

}
