package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.geom.Point2D

object WLNonCardCoarseImage {

  def makeImage(al: AttributeList, preprocessedImage: DicomImage): BufferedImage = {
    val gantryAngle = DicomUtil.findAllSingle(al, TagByName.GantryAngle).head.getDoubleValues.head
    val colAngle = DicomUtil.findAllSingle(al, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head
    val tableAngle = DicomUtil.findAllSingle(al, TagByName.PatientSupportAngle).head.getDoubleValues.head

    val bufImg = {
      val sortedPixels = preprocessedImage.pixelData.flatten.sorted
      val minPixelValue = sortedPixels(10)
      val maxPixelValue = sortedPixels.dropRight(10).last
      preprocessedImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minPixelValue, maxPixelValue)
    }

    val gc = ImageUtil.getGraphics(bufImg)

    gc.setColor(Color.white)

    if (true) {
      val gantryText = "Gantry Angle: " + Util.fmtDbl(gantryAngle)
      val colText = "Collimator Angle: " + Util.fmtDbl(colAngle)
      val tableText = "Table Angle: " + Util.fmtDbl(tableAngle)

      val textHeight = ImageText.getFontHeight(gc)

      ImageText.drawTextCenteredAt(gc, preprocessedImage.width / 2, textHeight * 1.5, gantryText)
      ImageText.drawTextCenteredAt(gc, preprocessedImage.width / 2, textHeight * 2.5, colText)
      ImageText.drawTextCenteredAt(gc, preprocessedImage.width / 2, textHeight * 3.5, tableText)
    }

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      // Trace.trace(Util.d2i(x1) + " : " + Util.d2i(y1) + " : " + Util.d2i(x2) + " : " + Util.d2i(y2))
      gc.drawLine(Util.d2i(x1), Util.d2i(y1), Util.d2i(x2), Util.d2i(y2))
    }

    /**
      * Label the collimator edge
      * @param name Edge name.
      * @param point1 One end.
      * @param point2 The other end.
      */
    def labelEdge(name: String, point1: Point2D.Double, point2: Point2D.Double): Unit = {
      val centerX = (point1.getX + point2.getX) / 2
      val centerY = (point1.getY + point2.getY) / 2
      ImageText.drawTextCenteredAt(gc, centerX, centerY, name)
    }

    val rot = WLRotator(al)

    val X1Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYTop)))
    val X2Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYTop)))

    val X1Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYBottom)))
    val X2Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYBottom)))

    drawLine(X1Y2.getX, X1Y2.getY, X2Y2.getX, X2Y2.getY)
    drawLine(X1Y2.getX, X1Y2.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y1.getX, X2Y1.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y2.getX, X2Y2.getY, X2Y1.getX, X2Y1.getY)

    labelEdge("X1", X1Y2, X1Y1)
    labelEdge("X2", X2Y2, X2Y1)
    labelEdge("Y1", X1Y1, X2Y1)
    labelEdge("Y2", X1Y2, X2Y2)

    bufImg
  }

}
