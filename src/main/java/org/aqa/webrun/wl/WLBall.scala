package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.MachineWL
import org.aqa.Util
import org.opensourcephysics.numerics.CubicSpline

import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.io.File
import javax.vecmath.Point2i

case class WLBall(aoiBounds: Rectangle, wholeImage: DicomImage, rtimage: AttributeList, machineWL: MachineWL, subDir: File) extends Logging {

  private val ballMinDiam_mm = 2.5
  private val ballMaxDiam_mm = 10.0

  private val aoiImage = wholeImage.getSubimage(aoiBounds)

  private val trans = new IsoImagePlaneTranslator(rtimage)

  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  /** Scaling for drawing images. */
  private val SCALE: Int = ((Config.WLScale / 0.526) * ((ResolutionX + ResolutionY) / 2.0)).round.toInt

  // The step size (in pixels) for crawling down the curve to find the
  // point where the ball height exceeds a threshold
  private val X_INCREMENT: Double = 0.001

  private val ballRadiusX_pix = trans.iso2PixDistX(machineWL.ballDiameter_mm / 2)
  private val ballRadiusY_pix = trans.iso2PixDistY(machineWL.ballDiameter_mm / 2)

  /** Expected radius of ball in (units of) number of pixels. */
  private val ballRadius_pix = Math.max(ballRadiusX_pix, ballRadiusY_pix)

  /**
    * Coarsely locate the center of the ball by finding the brightest small rectangle of pixels.
    * This should be precise to within about 1 pixel of actual center.
    *
    * @return The image-relative center coordinates in pixels.
    */
  private def coarseBallLocate(): Point = {
    val width_pix = trans.iso2PixDistX(ballMinDiam_mm).round.toInt
    val height_pix = trans.iso2PixDistY(ballMinDiam_mm).round.toInt
    val max = aoiImage.getMaxRect(width_pix, height_pix)
    // adjust from upper left corner to center, and make it image-relative.
    new Point(max.x + (width_pix / 2) + aoiBounds.x, max.y + (height_pix / 2) + aoiBounds.y)
  }

  private def toCubicSpline(values: Seq[Float]): CubicSpline = {
    new CubicSpline(values.indices.map(_.toDouble).toArray, values.map(_.toDouble).toArray)
  }

  private def centerOfMass(profile: Seq[Float]): SearchRange = {

    val spline = toCubicSpline(profile)

    if (true) {
      println("----- begin ------")
      val m = 25
      (1 to (profile.size * m)).foreach(i => {

        val x1 = (i.toDouble - 1) / m
        val x2 = i.toDouble / m
        val x3 = (i.toDouble + 1) / m

        val y1 = spline.evaluate(x1)
        val y2 = spline.evaluate(x2)
        val y3 = spline.evaluate(x3)

        val slope1 = (y2 - y1) / (x2 - x1)
        val slope2 = (y3 - y2) / (x3 - x2)

        val acc = (slope2 - slope1) / ((x3 - x1) / 4)
        println(slope1)
      })
      println("----- end ------")
    }

    val pointList = (0 until (profile.size / X_INCREMENT).round.toInt).map(i => spline.evaluate(i * X_INCREMENT))
    val min = pointList.min
    val max = pointList.max

    val minAcceptable = min + ((max - min) * (1.0 - (Config.WLBallHeightPercentForeground / 100.0)))

    val lo = pointList.indexWhere(p => p >= minAcceptable)
    val hi = (pointList.size - 1) - pointList.reverse.indexWhere(p => p >= minAcceptable)

    val weighted = (lo to hi).map(i => pointList(i) * i).sum * X_INCREMENT
    val sum = pointList.slice(lo, lo + hi - lo).sum
    val centerOfMass = weighted / sum

    new SearchRange(lo * X_INCREMENT, centerOfMass, hi * X_INCREMENT)
  }

  /**
    * Determine if the spline has a single maximum by walking the spline and counting the
    * number of times it crosses the average value.  It should cross exactly twice.
    */
  private def singleMax(values: IndexedSeq[Float]): Boolean = {
    val spline = toCubicSpline(values)
    val avg = (values.max + values.min) / 2
    val increment = 1000

    // determine if this value is over or under the average, and increment the count accordingly
    // crossCount: Number of times that the average was crossed.
    def cross(x: Int, crossCount: Int, overAvg: Boolean): (Int, Boolean) = {
      val value = spline.evaluate(x.toDouble / increment)
      0 match {
        case _ if overAvg && (value < avg)     => (crossCount + 1, false)
        case _ if (!overAvg) && (value >= avg) => (crossCount + 1, true)
        case _                                 => (crossCount, overAvg)
      }
    }

    val state = (0 to (values.length * increment)).toList.foldLeft((0, false))((s, x) => cross(x, s._1, s._2))
    val crsCount = state._1
    if (crsCount == 2) {
      logger.info(" Ball spline verified to cross average height exactly twice")
      true
    } else {
      logger.warn(" Wrong number of times that the ball spline crossed the average value.  Should be 2 but was " + crsCount)
      false
    }
  }

  /*
   * @param x: Approximate horizontal center of ball
   * @param y: Approximate vertical center of ball
   */

  private def fineBallLocate(aoiBall: DicomImage): Either[WLImageStatus.Value, Point2D.Double] = {

    val annotate = new WLAnnotate(SCALE, ballRadius_pix.round.toInt)

    val rSum = WLProcessImage.unitize(aoiBall.rowSums)
    val cSum = WLProcessImage.unitize(aoiBall.columnSums)

    val fineX = centerOfMass(cSum)
    val fineY = centerOfMass(rSum)

    if (true) {
      val b = aoiBall.toBufferedImage(Color.green)
      val f = new File(subDir, "aoiBall_" + System.currentTimeMillis() + ".png")
      Thread.sleep(50)
      Util.writePng(b, f)
      Trace.trace(s"wrote file $f")
      Trace.trace()
    }

    val image = annotate.saveFineLocatedImage(aoiBall.pixelData, fineX, fineY)

    if (singleMax(cSum) && singleMax(rSum)) {
      logger.info("Ball fine location relative to area of interest in pixels: " + fineX.center + ", " + fineY.center)
      Right(new Point2D.Double(fineX.center, fineY.center))
    } else
      Left(WLImageStatus.BallAreaNoisy)
  }

  /**
    * Make a search boundary centered around the given point.
    * @param point_pix Center point in global pixel coordinates.
    * @return Bounding rectangle.
    */
  private def makeBallSearchBounds(point_pix: Point): Rectangle = {
    val width_pix = trans.iso2PixDistX(ballMaxDiam_mm)
    val height_pix = trans.iso2PixDistY(ballMaxDiam_mm)

    val x = (point_pix.x - (width_pix / 2)).round.toInt
    val y = (point_pix.y - (height_pix / 2)).round.toInt
    val w = width_pix.round.toInt
    val h = height_pix.round.toInt

    new Rectangle(x, y, w, h)
  }

  /**
    * Determine if the ball is present by looking at the horizontal and vertical profiles.
    *
    * Each should have a single well define maximum point.
    *
    * @return True if there is a ball.
    */
  private def ballIsPresent(): Boolean = {
    singleMax(aoiImage.columnSums) && singleMax(aoiImage.rowSums)
  }

  /**
    * Given a profile, find the approximate distance between the min and max of the
    * derivative function of its cubic spline.
    * @param profile For this profile.
    * @return Distance in pixels.
    */
  private def distanceBetweenMinAndMax(profile: Seq[Float]): (Double, Double) = {

    val precision = 100

    val step = 1.0 / precision

    val spline = toCubicSpline(profile)

    val cubicSplineProfile = (0 until precision * profile.size).map(i => spline.evaluate(i * step))

    Trace.trace("jjjjjjjjjjjjj\n" + cubicSplineProfile.mkString("\n") + "\n")
    Trace.trace()

    def eval(i: Int): Double = {
      val y1 = cubicSplineProfile(i)
      val y2 = cubicSplineProfile(i + 1)
      val d = (y2 - y1) / step
      d
    }

    val derivative = cubicSplineProfile.indices.dropRight(1).map(eval)

    val derivativeMin = derivative.min
    val derivativeMax = derivative.max

    val minX = derivative.indexWhere(_ == derivativeMin) * step
    val maxX = derivative.indexWhere(_ == derivativeMax) * step

    if (minX < maxX)
      (minX, maxX)
    else
      (maxX, minX)
  }

  /**
    * Measure the width and height of the ball.
    *
    * Do this by converting the horizontal and vertical profiles to a cubic spline, and then
    * use the first derivative of that to find the max and min points.  The distance between
    * those points is related to the diameter of the ball.  Finally, multiply the distance
    * by a constant to get the diameter.
    *
    * @param ballSearchBounds Bounding search area for ball.
    * @return width and height as x and y in pixels.
    */
  private def measureBallSize_pix(ballSearchBounds: Rectangle): Rectangle2D.Double = {
    val area = wholeImage.getSubimage(ballSearchBounds)

    val leftRight = distanceBetweenMinAndMax(area.columnSums)
    val topBottom = distanceBetweenMinAndMax(area.rowSums)

    val ratio = 1.5

    val x = leftRight._1
    val y = topBottom._1
    val w = leftRight._2 - leftRight._1
    val h = topBottom._2 - topBottom._1
    val bounds = new Rectangle2D.Double(x, y, w, h)

    if (true) {
      val b = area.toBufferedImage(Color.blue)

      val gc = ImageUtil.getGraphics(b)
      gc.setColor(Color.red)

      gc.drawRect(x.round.toInt, y.round.toInt, w.round.toInt, h.round.toInt)

      val f = new File(subDir, "measureBallSize_pix" + System.currentTimeMillis() + ".png")
      Util.writePng(b, f)
      Trace.trace(s"wrote file $f")
      Trace.trace()
      Thread.sleep(50)
    }

    bounds
  }

  private def locate(): Either[WLImageStatus.Value, Point2D.Double] = {

    val coarse_pix = coarseBallLocate()

    val ballSearchBounds = makeBallSearchBounds(coarse_pix)

    val ballSize_pix = measureBallSize_pix(ballSearchBounds)

    Trace.trace(s"ballSize_pix: $ballSize_pix")

    val ballSizeX_iso = trans.pix2IsoDistX(ballSize_pix.width)
    val ballSizeY_iso = trans.pix2IsoDistY(ballSize_pix.height)

    Trace.trace(s" ballSizeX_iso: $ballSizeX_iso     ballSizeY_iso: $ballSizeY_iso  ")

    if (true) {
      Trace.trace(ballSearchBounds)
      val s = wholeImage.getSubimage(ballSearchBounds)
      val b = s.toBufferedImage(Color.red)
      val f = new File(subDir, "ballSearchBounds_" + System.currentTimeMillis() + ".png")
      Util.writePng(b, f)
      Trace.trace(s"wrote file $f")
      Trace.trace()
      Thread.sleep(50)
    }

    if (true) {
      val b = aoiImage.toBufferedImage(Color.orange)
      val f = new File(subDir, "all_aoi_" + System.currentTimeMillis() + ".png")
      Util.writePng(b, f)
      Trace.trace(s"wrote file $f")
      Trace.trace()
      Thread.sleep(50)
    }

    if (true || ballIsPresent()) { // TODO rm true

      val backgroundMean = (aoiImage.minPixelValue + aoiImage.maxPixelValue) / 2

      // get the list of pixel coordinates that are part of the ball
      val ballPixList = {
        val all =
          for (x <- 0 until aoiImage.width; y <- 0 until aoiImage.height)
            yield new Point2i(x, y)

        def isBig(p: Point2i): Boolean = {
          val v = aoiImage.get(p.x, p.y)

          v > backgroundMean
        }

        val pixelsOfBall = all.filter(isBig)

        pixelsOfBall
      }

      val boundary = 2

      val minX = ballPixList.map(_.x).min - boundary
      val maxX = ballPixList.map(_.x).max + boundary
      val minY = ballPixList.map(_.y).min - boundary
      val maxY = ballPixList.map(_.y).max + boundary

      val x = Math.max(0, minX)
      val y = Math.max(0, minY)
      val width = {
        val w = maxX - minX
        val j = x + w
        Math.min(aoiImage.width - x, w)
      }
      val height = {
        val h = maxY - minY
        val j = y + h
        Math.min(aoiImage.height - y, h)
      }

      val ballRect = new Rectangle(x, y, width, height)

      val ballAoi = aoiImage.getSubimage(ballRect)

      if (true) {
        val b = ballAoi.toBufferedImage(Color.green)
        val f = new File(subDir, "justTheBall_" + System.currentTimeMillis() + ".png")
        Thread.sleep(50)
        Util.writePng(b, f)
        Trace.trace(s" $f")
        Trace.trace()
      }

      fineBallLocate(ballAoi)

    } else
      Left(WLImageStatus.BallMissing)

  }

  /** The coordinates of the center of the ball in mm iso coordinates, or, an error code. */
  val center_pix: Either[WLImageStatus.Value, Point2D.Double] = locate()

  /*
  val ballFine: BufferedImage = {
    val annotate = new WLAnnotate(SCALE, ballRadius_pix.round.toInt)
    // annotate.saveFineLocatedImage(aoiFine.pixelData, point.get.getX.round.toInt, point.get.getY)
  }
   */

  /**
    * Make an image showing the level of background noise immediately around the ball.
    */
  private def showBallBackgroundNoise(name: String): BufferedImage = {
    // val aoiWidth = areaOfInterest.head.length
    // val aoiHeight = areaOfInterest.length
    // val aoi = subSection(areaOfInterest, 0, aoiWidth, 0, aoiHeight)

    val min = aoiImage.minPixelValue
    val max = aoiImage.maxPixelValue
    val limit = ((max - min) * 0.08) + min

    def doRow(row: IndexedSeq[Float]) = row.map(v => if (v > limit) min else v)

    val newAoi = aoiImage.pixelData.map(doRow)

    val bufImg = WLProcessImage.toPngScaled(newAoi, SCALE)
    bufImg
  }

  val ballBackgroundNoise: BufferedImage = showBallBackgroundNoise("ball_background")

}
