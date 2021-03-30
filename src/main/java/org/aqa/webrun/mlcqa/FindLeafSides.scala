package org.aqa.webrun.mlcqa

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ImageUtil.LocateMax
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util

import java.awt.Rectangle
import java.io.File

class FindLeafSides(image: AttributeList, rtplan: AttributeList) {

  private val ConfigLeafPad_mm = 0.5

  private val isHorizontal: Boolean = Phase2Util.isHorizontal(image)
  private val translator = new IsoImagePlaneTranslator(image)

  private def d2i(d: Double): Int = d.round.toInt

  /**
    * Make an awt Rectangle from double precision values.  This is mostly a convenience function.
    * @param x X
    * @param y Y
    * @param width Width
    * @param height Height
    * @return a Rectangle
    */
  private def rectD(x: Double, y: Double, width: Double, height: Double): Rectangle = {
    new Rectangle(d2i(x), d2i(y), d2i(width), d2i(height))
  }

  private val dicomImage = new DicomImage(image)

  // The sorted list of leaf sides in the RTPLAN in pixels.
  private val leafSidesFromPlanAsPix: Seq[Double] = {
    DicomUtil.findAllSingle(rtplan, TagByName.LeafPositionBoundaries).head.getDoubleValues.sorted
  }

  private val beamNumber = DicomUtil.findAllSingle(image, TagByName.ReferencedBeamNumber).head.getIntegerValues.head
  private val beamSequence = Phase2Util.getBeamSequence(rtplan, Util.beamNumber(image))

  case class EndPair(min: Double, max: Double) {}

  /**
    * Get the positions of ends of the leaves in mm in isoplane.
    * @return Two positions indicating the end pair.
    */
  private val endPairIso: EndPair = {
    val cps = DicomUtil.seqToAttr(beamSequence, TagByName.ControlPointSequence).head
    val beamLimitList = DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)
    val end = beamLimitList.maxBy(bl => bl.get(TagByName.LeafJawPositions).getDoubleValues.length)
    val endList = end.get(TagByName.LeafJawPositions).getDoubleValues
    EndPair(endList.min, endList.max)
  }

  private val endPairPix: EndPair = {
    if (isHorizontal)
      ???
    else {
      val both = Seq(translator.iso2PixCoordY(-endPairIso.min), translator.iso2PixCoordY(-endPairIso.max))
      EndPair(both.min, both.max)
    }
  }

  // Bounding box for finding leaf ends
  private val topLeftLeafBox: Rectangle = {
    val leafThickness = (leafSidesFromPlanAsPix.head - leafSidesFromPlanAsPix(1)).abs
    if (isHorizontal) {
      ???
    } else {
      val leafThicknessPix = translator.iso2PixDistX(leafThickness)
      val penumbraThicknessPix = translator.iso2PixDistY(Config.PenumbraThickness_mm)

      val height = penumbraThicknessPix / 4
      val width = leafThicknessPix * 2
      val x = leafThicknessPix / 2
      val y = endPairPix.min - height / 2
      rectD(x, y, width, height)
    }
  }

  private def leafSideListOf(box: Rectangle): Seq[Double] = {
    val subImage = dicomImage.getSubimage(box)
    if (isHorizontal) {
      val profile = subImage.rowSums
      profile.foreach(println)
      ???
    } else {
      val profile = subImage.columnSums
      val max = profile.max
      // the minimums are well defined, but the locateMax function looks for maximums, so flip the profile vertically.
      val profileFlipped = profile.map(max - _)
      profileFlipped.foreach(println)

      val half = profile.size / 2

      val leafA = LocateMax.locateMax(profileFlipped.take(half))
      val leafB = LocateMax.locateMax(profileFlipped.drop(half)) + half

      val x = leafA + ConfigLeafPad_mm
      val width = (leafB - leafA) - (ConfigLeafPad_mm * 2)
      val penumbraHeight_pix = translator.iso2PixDistY(Config.PenumbraThickness_mm)
      val y = endPairPix.min - penumbraHeight_pix / 2
      val height = penumbraHeight_pix
      val endBoundingRectangle = rectD(x, y, width, height)

      val endProfile = dicomImage.getSubimage(endBoundingRectangle).rowSums

      println("endProfile\n" + endProfile.mkString("\n"))

      val end = LocateEdge.locateEdge(endProfile, (endProfile.min + endProfile.max) / 2)

      val endPosition_pix = y + end
      println("end pix: " + endPosition_pix)
      println("hey")

    }

    Seq() // TODO
  }

  if (true) {
    val bList = DicomUtil.findAllSingle(image, TagByName.BeamLimitingDeviceAngle).flatMap(_.getDoubleValues)
    println("bList: " + bList.mkString("\n"))
    println("isHorizontal: " + isHorizontal)
    println
  }

  leafSideListOf(topLeftLeafBox)
}

object FindLeafSides {
  def main(args: Array[String]): Unit = {
    val image = new DicomFile(new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_003_2020-03-23T19-12-25.000.dcm""")).attributeList.get
    val rtplan = new DicomFile(new File("""D:\tmp\aqa\GapSkew\dicom\GapSkewRtPlans\RP.1.2.246.352.71.5.824327626427.245627.20140602132138.dcm""")).attributeList.get

    new FindLeafSides(image, rtplan)
  }
}
