package org.aqa.webrun.phase2

import org.aqa.Logging
import com.pixelmed.dicom.AttributeList
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Point
import scala.collection.mutable.ArrayBuffer
import org.aqa.IsoImagePlaneTranslator

/**
 * Analyze DICOM files for symmetry and flatness.
 */

object SymmetryAndFlatnessAnalysisPixelMap extends Logging {

  private class SymmetryAndFlatnessAnalysisPixelMapMaker(val translator: IsoImagePlaneTranslator) extends Logging {

    /**
     * Make a map that contains the list of pixels for each point in an image that should be used to measure it's value.
     */
    private def makePixelMap: Map[SymmetryAndFlatnessPoint, IndexedSeq[Point]] = {

      val circleRadiusInPixels = translator.circleRadiusInPixels

      def pixelMapOfPoint(point: SymmetryAndFlatnessPoint) = {
        val centerInPixels = translator.iso2Pix(point.asPoint)

        val expandedRadiusInPixels = circleRadiusInPixels + 2
        val xLo = Math.max((centerInPixels.getX - expandedRadiusInPixels).round.toInt, 0)
        val xHi = Math.min((centerInPixels.getX + expandedRadiusInPixels).round.toInt, translator.width - 1)
        val yLo = Math.max((centerInPixels.getY - expandedRadiusInPixels).round.toInt, 0)
        val yHi = Math.min((centerInPixels.getY + expandedRadiusInPixels).round.toInt, translator.height - 1)
        val list = for (x <- (xLo to xHi); y <- (yLo to yHi); if (new Point2D.Double(x, y).distance(centerInPixels) <= circleRadiusInPixels)) yield { new Point(x, y) }
        if (list.size < 10) {
          // this should never happen
          val msg = "SymmetryAndFlatnessAnalysis.pixelMapOfPoint Point list is too small.  Number of pixels: " + list.size
          logger.error(msg)
          throw new RuntimeException(msg)
        }
        list
      }

      Config.SymmetryAndFlatnessPointList.map(p => (p, pixelMapOfPoint(p))).toMap
    }

    val pixelMap = makePixelMap
  }

  private val cache = ArrayBuffer[SymmetryAndFlatnessAnalysisPixelMapMaker]()

  def getPixelMap(al: AttributeList): Map[SymmetryAndFlatnessPoint, IndexedSeq[Point]] = cache.synchronized {
    val translator = new IsoImagePlaneTranslator(al)

    cache.find(s => s.translator.equalTo(translator)) match {
      case Some(f) => f.pixelMap
      case _ => {
        val f = new SymmetryAndFlatnessAnalysisPixelMapMaker(translator)
        cache += f
        f.pixelMap
      }
    }
  }

}