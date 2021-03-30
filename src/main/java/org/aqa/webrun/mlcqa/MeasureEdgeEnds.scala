package org.aqa.webrun.mlcqa

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.webrun.phase2.leafPosition.LeafPositionCoarseLeafSides
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil

import java.awt.Color
import java.io.File

class MeasureEdgeEnds(runReq: MlcQaRunReq) {

  // Locate leaf sides

  def measure(beamName: String): Unit = {
    val al = runReq.rtimageMap(beamName)

    val horizontal: Boolean = {
      val collimatorAngle = DicomUtil.findAllSingle(al, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head
      val angleRounded = Util.angleRoundedTo90(collimatorAngle)
      (angleRounded % 180) == 0
    }

    println("beamName: " + beamName)
    //println("GantryAngle: " + DicomUtil.findAllSingle(al, TagByName.GantryAngle).head.getDoubleValues.head)
    println("BeamLimitingDeviceAngle: " + DicomUtil.findAllSingle(al, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head)
    println("horizontal: " + horizontal)

    val dicomImage = new DicomImage(al)

    // val dc = dicomImage.getSubimage(new Rectangle(0, 200, 1024, 350))

    val profile = if (horizontal) dicomImage.rowSums else dicomImage.columnSums
    val translator = new IsoImagePlaneTranslator(al)

    println("0,0 pix: " + translator.pix2Iso(0, 0))
    println("511,655 pix: " + translator.pix2Iso(511.5, 654.5))
    println("-100,-100 mm to pix: " + translator.iso2Pix(-100, -100))
    println("0,0 mm to pix: " + translator.iso2Pix(0, 0))
    println("100,100 mm to pix: " + translator.iso2Pix(100, 100))
    println("-120,-120 mm to pix: " + translator.iso2Pix(-120, -120))
    println("-200,-200 mm to pix: " + translator.iso2Pix(-200, -200))

    def transSidesIso2Pix(list: Seq[Double]): Seq[Double] = {
      list.map(s => if (horizontal) translator.iso2PixCoordY(s) else translator.iso2PixCoordX(s))
    }

    def transSidesPix2Iso(list: Seq[Double]): Seq[Double] = {
      list.map(s => if (horizontal) translator.pix2IsoCoordY(s) else translator.pix2IsoCoordX(s))
    }

    val beamSeq = Phase2Util.getBeamSequenceOfPlan(beamName, runReq.rtplan)
    // val leafSideList_pix =
    // LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, runReq.rtplan, translator).
    // map(s => if (horizontal) translator.iso2PixCoordY(s) else translator.iso2PixCoordX(s))
    val sideListPlan_mm = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, runReq.rtplan, translator).sorted
    val sideListPlan_pix = transSidesIso2Pix(sideListPlan_mm)

    val preciseLeafSideList_pix = LeafPositionAnalysis.leafSides_pix(horizontal, beamName, al, dicomImage, runReq.rtplan, translator, sideListPlan_mm)
    val preciseLeafSideList_mm = transSidesPix2Iso(preciseLeafSideList_pix)

    val coarseList_pix = LeafPositionCoarseLeafSides.coarseLeafSides(horizontal, profile, al, 5, 10, dicomImage)

    def f(d: Double) = d.formatted("%8.3f")
    def fl(dl: Seq[Double]) = dl.map(f).mkString("  ")
    println("sideListPlan_mm         : " + fl(sideListPlan_mm))
    println("sideListPlan_pix        : " + fl(sideListPlan_pix))

    println("preciseLeafSideList_mm  : " + fl(preciseLeafSideList_mm))
    println("preciseLeafSideList_pix : " + fl(preciseLeafSideList_pix))

    println("coarseList_pix          : " + fl(coarseList_pix))
    // println("profile                 : \n" + profile.map(_.toLong).mkString("\n") + "\n")

    if (true) {
      println
      println("sideListPlan_mm         : " + fl(sideListPlan_mm.drop(1)))
      println("preciseLeafSideList_mm  : " + fl(preciseLeafSideList_mm))
      val incr = (1 until preciseLeafSideList_mm.size).map(i => preciseLeafSideList_mm(i) - preciseLeafSideList_mm(i - 1))
      println("precise incr mm         : " + fl(incr))

      val diff: Unit = {
        val size = Math.min(sideListPlan_mm.drop(1).size, preciseLeafSideList_mm.size)
        val both = sideListPlan_mm.slice(1, size + 1) zip preciseLeafSideList_mm.take(size)
        val diff = both.map(b => b._1 - b._2)
        println("diff                    : " + fl(diff))
      }
    }

    if (true) {
      println
      println("sideListPlan_pix        : " + fl(sideListPlan_pix.drop(1)))
      println("preciseLeafSideList_pix : " + fl(preciseLeafSideList_pix))
      println("preciseLeafSideList_pix : " + fl(preciseLeafSideList_pix))
      val diff: Unit = {
        val size = Math.min(sideListPlan_pix.drop(1).size, preciseLeafSideList_pix.size)
        val both = sideListPlan_pix.slice(1, size + 1) zip preciseLeafSideList_pix.take(size)
        val diff = both.map(b => b._1 - b._2)
        println("diff                    : " + fl(diff))
      }
    }

    val img = dicomImage.toDeepColorBufferedImage(0.01)

    if (false && horizontal) {
      ???
    } else {
      val g = ImageUtil.getGraphics(img)
      g.setColor(Color.black)

      val mid = 365

      def drawVertLineTop(xd: Double): Unit = {
        g.setColor(Color.black)
        val x = xd.round.toInt
        g.drawLine(x, 0, x, mid)
      }

      def drawVertLineMid(xd: Double): Unit = {
        g.setColor(Color.white)
        val x = xd.round.toInt
        g.drawLine(x, mid, x, dicomImage.height - 1)
      }

      sideListPlan_pix.foreach(x => drawVertLineTop(x))
      preciseLeafSideList_pix.foreach(x => drawVertLineMid(x))
      val outFile = new File("""D:\tmp\""" + System.currentTimeMillis() + """.png""")
      Util.writePng(img, outFile)
      println("Wrote file: " + outFile.getAbsolutePath)
    }

    if (false) {
      val center = dicomImage.width / 2
      val list = (0 until dicomImage.height).map(y => dicomImage.get(center, y).round.formatted("%8d"))
      println("center pixel values (vertical):\n" + list.mkString("\n"))
    }
  }
}

object MeasureEdgeEnds {

  private def readDicomFiles(testDir: File): Seq[AttributeList] = {
    Util.listDirFiles(testDir).filter(DicomFileUtilities.isDicomOrAcrNemaFile).flatMap(f => DicomFile(f).attributeList)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val beamName = "ABankC90"

    val testDir = new File("""src\test\resources\TestMlcQaMeasureEdgeEnds""")

    val dicomList = readDicomFiles(testDir)

    val rtimageList = dicomList.filter(Util.isRtimage)
    val rtplan = dicomList.find(Util.isRtplan).get

    if (true) {
      val ljp = DicomUtil.findAllSingle(rtplan, TagByName.LeafJawPositions)
      def fix(jp: Attribute): Unit = {
        if ((jp.getDoubleValues.length == 2) && (jp.getDoubleValues.head == -100)) {
          println("jp: " + jp.getDoubleValues.mkString("  "))
          jp.removeValues()
          jp.addValue(-199.9)
          jp.addValue(199.9)
        }
      }
      ljp.foreach(fix)
    }

    val rtimageMap = rtimageList.map(img => (Phase2Util.getBeamNameOfRtimage(rtplan, img), img)).filter(_._1.isDefined).map(bi => (bi._1.get, bi._2)).toMap

    val runReq = MlcQaRunReq(None, rtplan, rtimageMap)

    val mee = new MeasureEdgeEnds(runReq)

    mee.measure(rtimageMap.keys.head)

    println("Done.  Elapsed ms: " + (System.currentTimeMillis() - start))
  }
}
