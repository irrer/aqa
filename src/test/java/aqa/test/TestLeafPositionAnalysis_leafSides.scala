
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import edu.umro.ScalaUtil.Trace
import org.opensourcephysics.numerics.CubicSpline

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestLeafPositionAnalysis_leafSides extends FlatSpec with Matchers {

  val ridge1 = Seq(
    (315, 114), (316, 219), (317, 219), (318, 219), (319, 220), (320, 219), (321, 220), (322, 218), (323, 220), (324, 219),
    (325, 219), (326, 220), (327, 220), (328, 218), (329, 220), (330, 219), (331, 220), (332, 219), (333, 220), (334, 219),
    (335, 220), (336, 219), (337, 220), (338, 219), (339, 219), (340, 219), (341, 220), (342, 218), (343, 219), (344, 218),
    (345, 219), (346, 218), (347, 220), (348, 219), (349, 219), (350, 220), (351, 220), (352, 219), (353, 220), (354, 219),
    (355, 219), (356, 219), (357, 219), (358, 219), (359, 219), (360, 218), (361, 220), (362, 218), (363, 219), (364, 219),
    (365, 219), (366, 219), (367, 220), (368, 219), (369, 219), (370, 219), (371, 219), (372, 219), (373, 220), (374, 219),
    (375, 220), (376, 219), (377, 219), (378, 218), (379, 219), (380, 219), (381, 220), (382, 218), (383, 220), (384, 219),
    (385, 220), (386, 219), (387, 220), (388, 218), (389, 219), (390, 219), (391, 220), (392, 220), (393, 220), (394, 219),
    (395, 220), (396, 219), (397, 220), (398, 218), (399, 219), (400, 219), (401, 220), (402, 220), (403, 220), (404, 218),
    (405, 219), (406, 218), (407, 220), (408, 219), (409, 220), (410, 219), (411, 220), (412, 219), (413, 220), (414, 219),
    (415, 220), (416, 219), (417, 220), (418, 219), (419, 220), (420, 220), (421, 219), (422, 219), (423, 220), (424, 218),
    (425, 219), (426, 218), (427, 219), (428, 219), (429, 220), (430, 219), (431, 220), (432, 219), (433, 220), (434, 219),
    (435, 219), (436, 219), (437, 220), (438, 219), (439, 220), (440, 218), (441, 220), (442, 220), (443, 219), (444, 219),
    (445, 220), (446, 219), (447, 220), (448, 219), (449, 220), (450, 219), (451, 220), (452, 219), (453, 220), (454, 218),
    (455, 219), (456, 219), (457, 220), (458, 219), (459, 220), (460, 219), (461, 219), (462, 218), (463, 220), (464, 219))

  "TestLeafPositionAnalysis_leafSides" should "show precise leaf locations leaf end positions in isoplan in pixels" in {
    val dir = new File("""src\test\resources""")
    val outDir = new File("""target\TestLeafPositionAnalysis""")
    Util.deleteFileTreeSafely(outDir)
    outDir.mkdirs

    val file = new File(dir, """TestLeafPositionAnalysis.dcm""")
    println("Reading DICOM file : " + file.getAbsolutePath)
    val imageAttrList = Util.readDicomFile(file).right.get
    val dicomImage = new DicomImage(imageAttrList)
    println("dicomImage SOPInstanceUID: " + Util.sopOfAl(imageAttrList))
    //println("dicomImage imageAttrList: " + imageAttrList.toString.replace('\0', ' '))
    if (true) {
      Trace.trace("dicomImage at 500, 500  => 520, 520: \n")
      for (y <- 500 until 550) {
        //val text = (0 until dicomImage.width).map(x => dicomImage.get(x, y).round.toInt.formatted("%6d")).mkString("  ")
        val textArray = (500 until 550).toList.map(x => dicomImage.get(x, y).toInt.formatted("%5d"))
        //val text = textArray.toList.mkString("  ").toUpperCase
        //Trace.trace("    " + text)
        System.out.println(textArray.mkString)
      }
      println
    }

    val planFile = new File(dir, """TestLeafPositionAnalysisPlan.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get
    val beamName = "PF Stat 0"
    val preciseLeafSideList_pix = LeafPositionAnalysis.testLeafSides(true, beamName, imageAttrList, dicomImage, planAl)

    val coarseList_pix = LeafPositionAnalysis.testCoarseLeafSides(true, imageAttrList, 5, 10, dicomImage)
    println("Number of coarse ridges found: " + coarseList_pix.size)
    println("list of coarse ridges:\n    " + coarseList_pix.mkString("    "))

    def fmtDblList(ld: Seq[Double]) = ld.map(d => d.formatted("%7.3f"))

    if (true) {
      val search = 10
      val rowSums = dicomImage.rowSums
      val comList = coarseList_pix.map(cp => {
        val y = cp.toInt
        val range = (y - search until y + search)
        val torque = range.map(r => ((r + 0.5) * rowSums(r))).sum
        val mass = range.map(r => rowSums(r)).sum
        torque / mass
      })

      println("list of coarse ridges com:\n    " + fmtDblList(comList).mkString("    "))

      val widthListComCoarse = comList.drop(1).zip(comList.dropRight(1)).map(ba => ba._1 - ba._2)
      println("list of coarse ridges com widths:\n    " + fmtDblList(widthListComCoarse).mkString("    "))

      println("list of coarse ridges com widths sorted:\n    " + fmtDblList(widthListComCoarse.sorted).mkString("    "))
      println("coarse ridges widths range: " + (widthListComCoarse.max - widthListComCoarse.min))
    }

    if (true) {
      val search = 10
      val rowSums = dicomImage.rowSums
      val comList = coarseList_pix.map(cp => {
        val y = cp.toInt
        val range = (y - search until y + search)
        val indicies = range.map(y => y.toDouble).toArray
        val data = range.map(y => rowSums(y).toDouble).toArray
        val cubicSpline = new CubicSpline(indicies, data)

        val max = (-10000 to 10000).map(i => ((i / 5000.0) + cp)).maxBy(ii => cubicSpline.evaluate(ii))

        max
      })

      println("list of cubicSpline ridges com:\n    " + fmtDblList(comList).mkString("    "))

      val widthListComCoarse = comList.drop(1).zip(comList.dropRight(1)).map(ba => ba._1 - ba._2)
      println("list of cubicSpline ridges com widths:\n    " + fmtDblList(widthListComCoarse).mkString("    "))

      println("list of cubicSpline ridges com widths sorted:\n    " + fmtDblList(widthListComCoarse.sorted).mkString("    "))
      println("cubicSpline ridges widths range: " + (widthListComCoarse.max - widthListComCoarse.min))
    }

    val correctRadius = 5
    val correctedImage = dicomImage.correctBadPixels(dicomImage.identifyBadPixels(100, 2.0, 50, correctRadius, 100), correctRadius)

    def makeCoarseImage = {

      val bufImageCoarse = correctedImage.toDeepColorBufferedImage
      val graphics = ImageUtil.getGraphics(bufImageCoarse)
      graphics.setColor(Color.black)
      coarseList_pix.map(ls => graphics.drawLine(0, ls.round.toInt, dicomImage.width - 1, ls.round.toInt))
      val pngFile = new File(outDir, "TestLeafPositionAnalysis_leafSides_coarse.png")
      println("Writing image file " + pngFile.getAbsolutePath)
      ImageUtil.writePngFile(bufImageCoarse, pngFile)
    }

    def makePreciseImage = {
      val bufImagePrecise = correctedImage.toDeepColorBufferedImage
      val graphics = ImageUtil.getGraphics(bufImagePrecise)
      graphics.setColor(Color.white)
      preciseLeafSideList_pix.map(ls => graphics.drawLine(0, ls.round.toInt, dicomImage.width - 1, ls.round.toInt))
      val rgb = Color.WHITE.getRGB
      //ridge1.map(xy => bufImagePrecise.setRGB(xy._1, xy._2, rgb))
      val pngFile = new File(outDir, "TestLeafPositionAnalysis_leafSides_precise.png")
      println("Writing image file " + pngFile.getAbsolutePath)
      ImageUtil.writePngFile(bufImagePrecise, pngFile)
    }

    makeCoarseImage
    makePreciseImage

    val widthListCoarse = coarseList_pix.drop(1).zip(coarseList_pix.dropRight(1)).map(ba => ba._1 - ba._2)
    println("list of coarse leaf widths:\n    " + widthListCoarse.mkString("  "))
    println("list of coarse leaf widths sorted:\n    " + widthListCoarse.sorted.mkString("  "))

    println("Number of precise ridges found: " + preciseLeafSideList_pix.size)
    println("list of leaf ridges:\n    " + preciseLeafSideList_pix.mkString("  "))
    val widthListPrecise = preciseLeafSideList_pix.drop(1).zip(preciseLeafSideList_pix.dropRight(1)).map(ba => ba._1 - ba._2)
    println("list of precise leaf widths:\n    " + widthListPrecise.mkString("  "))
    println("list of precise leaf widths sorted:\n    " + widthListPrecise.sorted.mkString("  "))
    println("precise leaf widths range: " + (widthListPrecise.max - widthListPrecise.min))

    true should be(true)
  }
}
