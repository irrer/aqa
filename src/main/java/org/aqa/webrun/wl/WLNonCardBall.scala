package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

case class WLNonCardBall(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage) extends Logging {

  private def pointIsInBallAoi(point: Point2i): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  /**
    * Construct the Ball AOI by determining which points are within the edge lines that define the minimum edge values.
    *
    * @return Ball AOI.
    */
  private def makeBallBounds(): Rectangle = {

    val pointList =
      for ( //
        x <- 0 until preprocessedImage.width;
        y <- 0 until preprocessedImage.height;
        if pointIsInBallAoi(new Point2i(x, y))
      ) yield new Point2i(x, y)

    val x = pointList.map(_.getX).min
    val y = pointList.map(_.getY).min
    val width = pointList.map(_.getX).max - x
    val height = pointList.map(_.getY).max - y

    new Rectangle(x, y, width, height)
  }

  /**
    * Make an image that only contains the ball, with pixels outside the lo-line areas 'blacked' out.
    * @param bounds Bounds of ball AOI.
    * @return
    */
  private def makeBallAOI(bounds: Rectangle): DicomImage = {

    val minPixelValue = {
      val badPixelCount = 5 // discard this many pixels as potentially bad
      val samplePixelCount = 10 // use this many pixels to get an average of 'dark' pixels.
      val list = preprocessedImage.pixelData.flatten.sorted

      // find the average of the dark pixels.
      list.slice(badPixelCount, badPixelCount + samplePixelCount).sum / samplePixelCount
    }

    def doPoint(point: Point2i): Float = {
      if (pointIsInBallAoi(point))
        preprocessedImage.get(point.getX, point.getY)
      else
        minPixelValue
    }

    def doRow(y: Int): IndexedSeq[Float] =
      (0 until preprocessedImage.width).map(x => doPoint(new Point2i(x, y)))

    val array = (0 until preprocessedImage.height).map(doRow)
    new DicomImage(array).getSubimage(bounds)
  }

  /**
    * Use center of mass to find the center of the ball.
    * @return Center, in absolute pixel coordinates.
    */
  private def findBallCenter(): Either[String, Point2d] = {
    val center = new Point2d(xProfile + bounds.x, yProfile + bounds.y)
    Right(center)
  }

  private val bounds = makeBallBounds()
  private val ballAOI = makeBallAOI(bounds)

  private val xProfile = ImageUtil.centerOfMass(ballAOI.columnSums)
  private val yProfile = ImageUtil.centerOfMass(ballAOI.rowSums)

  Trace.trace(s"""xProfile:\n${ballAOI.columnSums.mkString("\n")}""")
  Trace.trace(s"""yProfile:\n${ballAOI.rowSums.mkString("\n")}""")

  private def validateProfile(profile: Seq[Float]): Option[String] = {

    val min = profile.min
    val max = profile.max

    def crosses(i: Int): Boolean = {
      ((profile(i) < min) && (profile(i) >= max)) ||
      ((profile(i) > min) && (profile(i) <= max))
    }

    val numberOfCrossPoints = profile.indices.dropRight(1).count(crosses)

    val ok = numberOfCrossPoints == 2
    if (ok)
      None
    else
      Some(s"Could not confidently locate ball.")
  }

  private def validateBall(): Option[String] = {
    None // if (validateProfile()) TODO
  }

  val ballCenter: Point2d = findBallCenter().right.get

}
