package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.DicomImage

import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

object WLNonCardBallAOIBounds {

  /**
    * Calculate the rectangle to enclose the region of the image that contains all the areas of interest
    * that were used for edge measurement.
    *
    * @param margin_pix Number of extra pixels to serve as a margin separating the AOIs from the image edge.
    * @return Bounding rectangle in integer pixels.
    */
  def calcAoiBounds(edgeSet: WLNonCardEdgeSet, margin_pix: Int): Rectangle = {
    def listCoordinates(edge: WLNonCardEdge): Seq[Point2d] = {
      Seq(
        edge.loLoAoi, //
        edge.loHiAoi, //
        edge.hiLoAoi, //
        edge.hiHiAoi
      )
    }

    val coordinateList = edgeSet.edgeList.flatMap(listCoordinates)

    val minX = (coordinateList.map(_.getX).min - margin_pix).round.toInt
    val maxX = (coordinateList.map(_.getX).max + margin_pix).round.toInt
    val minY = (coordinateList.map(_.getY).min - margin_pix).round.toInt
    val maxY = (coordinateList.map(_.getY).max + margin_pix).round.toInt

    val width = maxX - minX
    val height = maxY - minY

    val boundingRectangle = new Rectangle(minX, minY, width, height)

    boundingRectangle
  }

  def pointIsInBallAoi(point: Point2d, edgeSet: WLNonCardEdgeSet): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  def makeBallBounds(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage): Rectangle = {

    val pointsInside = for (x <- 0 until preprocessedImage.width; y <- 0 until preprocessedImage.height; if (pointIsInBallAoi(new Point2d(x, y), edgeSet))) yield new Point2i(x, y)

    val minX = pointsInside.map(_.x).min
    val maxX = pointsInside.map(_.x).max
    val minY = pointsInside.map(_.y).min
    val maxY = pointsInside.map(_.y).max

    val rectangle = new Rectangle(minX, minY, maxX - minX, maxY - minY)

    rectangle
  }

  def makeBallAOI(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage): DicomImage = {

    val rectangle = makeBallBounds(edgeSet, preprocessedImage)

    val subImg = preprocessedImage.getSubimage(rectangle)

    val minPixelValue = subImg.pixelData.flatten.sorted.slice(5, 15).sum / 10

    def doRow(y: Int): Seq[Float] =
      subImg
        .pixelData(y)
        .indices
        .map(x => {
          val p = new Point2d(x + rectangle.x, y + rectangle.y)
          if (pointIsInBallAoi(p, edgeSet))
            subImg.get(x, y)
          else
            minPixelValue
        })

    val pixArray = (0 until subImg.height).map(doRow)

    val newSubImg = new DicomImage(pixArray)
    newSubImg
  }

}
