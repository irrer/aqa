package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Rectangle
import javax.vecmath.Point2d

object EdgeSetImage {

  private def labelEdge(edge: Edge, gc: Graphics2D, si: ScaledImage): Unit = {

    gc.setColor(Color.yellow)
    val textPoint = {
      val distance = {
        val d = edge.loLine.centerPoint.distance(edge.edgeLine.centerPoint) + ImageText.getFontHeight(gc)

        val pos = edge.line.pointOn(d)
        val neg = edge.line.pointOn(-d)

        val direction =
          if (edge.edgeLine.centerPoint.distance(pos) < edge.edgeLine.centerPoint.distance(neg))
            1
          else
            -1
        d * direction
      }
      edge.line.pointOn(distance)

    }
    Trace.trace(s"%%%%% Experiment ${edge.name} : $textPoint")
    si.drawTextCenteredAt(gc, textPoint.x.toInt, textPoint.y.toInt, edge.name)

  }

  def makeImage(edgeSet: EdgeSet, preprocessedImage: DicomImage, scale: Int, al: AttributeList): BufferedImage = {
    val dicomImage: DicomImage = new DicomImage(al)

    /** A buffered image using the ball pixels as the brightest pixels. This makes the ball stand out more.  */
    val bufImg = BlankImage.make(preprocessedImage, edgeSet)

    val scaledImage = ImageUtil.magnify(bufImg, 1)

    def listCoordinates(edge: Edge): Seq[Point2d] = {
      Seq(
        edge.loLoAoi, //
        edge.loHiAoi, //
        edge.hiLoAoi, //
        edge.hiHiAoi
      )
    }

    val coordinateList = edgeSet.edgeList.flatMap(listCoordinates)

    val border_pix = 3
    val minX = Math.max(0, (coordinateList.map(_.getX).min - border_pix).round.toInt)
    val maxX = Math.min(dicomImage.width - 1, (coordinateList.map(_.getX).max + border_pix).round.toInt)
    val minY = Math.max(0, (coordinateList.map(_.getY).min - border_pix).round.toInt)
    val maxY = Math.min(dicomImage.height - 1, (coordinateList.map(_.getY).max + border_pix).round.toInt)

    val width = maxX - minX
    val height = maxY - minY

    val boundingRectangle = new Rectangle(minX, minY, width, height)

    val si = ScaledImage(scale, minX, minY)

    val aoi: BufferedImage = ImageUtil.magnify(ImageUtil.subImage(scaledImage, boundingRectangle), scale)

    // val buf = si.magnify(origImage)
    def drawAoi(edgeSet: EdgeSet, aoi: BufferedImage): Unit = {

      val gc = ImageUtil.getGraphics(aoi)
      // scale the font to match the scale of the image.
      val typePointSize: Int = {
        val font = gc.getFont
        val s = font.getSize
        val s2: Int = (s * scale * 0.75).round.toInt
        s2
      }
      ImageText.setFont(gc, ImageText.DefaultFont, typePointSize)

      gc.setColor(Color.white)

      def drawEdge(edge: Edge, color: Color): Unit = {
        labelEdge(edge, gc, si)

        gc.setColor(Color.white)
        si.drawLine(gc, edge.loLoAoi, edge.loHiAoi)
        si.drawLine(gc, edge.hiLoAoi, edge.hiHiAoi)
        si.drawLine(gc, edge.loLoAoi, edge.hiLoAoi)
        si.drawLine(gc, edge.loHiAoi, edge.hiHiAoi)
        gc.setColor(color)
        si.drawLine(gc, edge.edgeLo, edge.edgeHi)
        si.drawLine(gc, edge.edgeLine.centerPoint, edge.edgeLine.centerPoint)
      }

      drawEdge(edgeSet.X1, Color.green)
      drawEdge(edgeSet.X2, Color.green)
      drawEdge(edgeSet.Y1, Color.orange)
      drawEdge(edgeSet.Y2, Color.orange)
    }

    val gc = ImageUtil.getGraphics(aoi)
    gc.setColor(Color.green)
    si.drawLine(gc, edgeSet.xMeanLine.pointOn(10), edgeSet.xMeanLine.pointOn(-10))
    gc.setColor(Color.orange)
    si.drawLine(gc, edgeSet.yMeanLine.pointOn(10), edgeSet.yMeanLine.pointOn(-10))

    drawAoi(edgeSet, aoi)

    aoi
  }

}
