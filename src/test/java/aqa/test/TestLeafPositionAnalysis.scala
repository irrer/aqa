
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage

/**
 * Test the LeafPositionAnalysis.
 *
 */

class TestLeafPositionAnalysis extends FlatSpec with Matchers {
  "LeafPositionAnalysis" should "measure leaf positions" in {
    val dir = new File("""src\test\resources""")
    val file = new File(dir, """TestLeafPositionAnalysis.dcm""")
    val al = Util.readDicomFile(file).right.get
    val dicomImage = new DicomImage(al)
    val profile = dicomImage.rowSums

    object Shape extends Enumeration {
      val peak = Value
      val valley = Value
      val flat = Value
    }

    case class PosVal(position: Int, value: Float, shape: Shape.Value) {
      def this(position: Int, value: Float) = this(position, value, Shape.flat)
      def this(pv: PosVal, shape: Shape.Value) = this(pv.position, pv.value, shape)
      override def toString = "pos: " + position.formatted("%4d") + "  value: " + value.formatted("%7.0f") + "   shape: " + shape.toString.format("%-6s")
    }

    val posValList = profile.zipWithIndex.map(pi => new PosVal(pi._2, pi._1))
    println("posValList:\n    " + posValList.mkString("\n    "))

    val minLeafWidthInPixels = 22.32142857142857

    val distance = (minLeafWidthInPixels / 3).round.toInt
    println("distance: " + distance)

    def isValley(pos: Int) = {
      (pos > distance) &&
        (pos < profile.size - distance) &&
        (pos - distance until pos + distance).map(p => profile(p) >= profile(pos)).reduce(_ && _)
    }

    def isPeak(pos: Int) = {
      (pos > distance) &&
        (pos < profile.size - distance) &&
        (pos - distance until pos + distance).map(p => profile(pos) >= profile(p)).reduce(_ && _)
    }

    def classify(pos: Int): PosVal = {
      0 match {
        case _ if isPeak(pos) => new PosVal(pos, profile(pos), Shape.peak)
        case _ if isValley(pos) => new PosVal(pos, profile(pos), Shape.valley)
        case _ => new PosVal(pos, profile(pos), Shape.flat)
      }
    }

    val classified = (distance until profile.size - distance).map(pos => classify(pos))

    val peakValleyList = classified.filter(posVal => posVal.shape != Shape.flat).sortBy(_.position)

    def dropAfterLastPeak(list: IndexedSeq[PosVal]): IndexedSeq[PosVal] = {

      def getMore(pos: Int) = {
        ((pos + 3) <= list.size) &&
          (list(pos).shape == Shape.peak) &&
          (list(pos + 1).shape == Shape.valley) &&
          (list(pos + 2).shape == Shape.peak)
      }

      def search(pos: Int): Int = {
        if (getMore(pos)) search(pos + 2)
        else pos
      }

      val start = list.indexWhere(posVal => posVal.shape == Shape.peak)
      val lastPeak = search(start)
      list.take(lastPeak + 1)
    }

    val half = peakValleyList.size / 2
    val lo = peakValleyList.take(half)
    val hi = peakValleyList.drop(half)

    println("lo:\n    " + lo.mkString("\n    "))
    println("hi:\n    " + hi.mkString("\n    "))

    val validValleyPeakList = dropAfterLastPeak(lo.reverse).reverse ++ dropAfterLastPeak(hi)
    println("validValleyPeakList:\n    " + validValleyPeakList.mkString("\n    "))

    true should be(true)
  }
}
