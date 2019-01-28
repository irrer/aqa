
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
import org.aqa.webrun.phase2.leafPosition.LeafPositionCoarseLeafSides
import org.aqa.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestLeafPositionAnalysis_leafSides2 extends FlatSpec with Matchers {

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

    val planFile = new File(dir, """TestLeafPositionAnalysisPlan.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get
    val beamName = "PF Stat 0"
    val horizontal = true
    val profile = if (horizontal) dicomImage.rowSums else dicomImage.columnSums
    val translator = new IsoImagePlaneTranslator(imageAttrList)
    val leafEndList_pix = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, planAl).map(s => translator.iso2PixCoordY(s))
    val preciseLeafSideList_pix = LeafPositionAnalysis.leafSides_pix(horizontal, beamName, imageAttrList, dicomImage, planAl, translator, leafEndList_pix)

    val coarseList_pix = LeafPositionCoarseLeafSides.coarseLeafSides(horizontal, profile, imageAttrList, 5, 10, dicomImage)
    println("Number of coarse ridges found: " + coarseList_pix.size)
    println("list of coarse ridges:\n    " + coarseList_pix.map(l => l.formatted("%8.4f")).mkString("  "))

    val scoredSides = coarseList_pix.indices.drop(1).dropRight(1).map(i => {
      val below = profile(((coarseList_pix(i - 1) + coarseList_pix(i)) / 2).round.toInt)
      val above = profile(((coarseList_pix(i + 1) + coarseList_pix(i)) / 2).round.toInt)
      val peak = profile(coarseList_pix(i).round.toInt)
      val score = ((peak * 2) - (below + above)) / 2
      println("    " + coarseList_pix(i) + " : " + Util.fmtDbl(score))
      (coarseList_pix(i), score)
    })

    scoredSides.sortBy(_._2)

    println("Number of precise ridges found: " + preciseLeafSideList_pix.size)
    println("list of precise leaf ridges:\n    " + preciseLeafSideList_pix.map(l => l.formatted("%8.4f")).mkString("  "))

    val diffText = preciseLeafSideList_pix.indices.tail.map(i => preciseLeafSideList_pix(i) - preciseLeafSideList_pix(i - 1)).mkString("\n    ")
    println("diffs\n    " + diffText)

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

    true should be(true)
  }
}
