/*
 * Copyright 2026 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.LocateMax

import java.awt.Color
import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.security.InvalidParameterException
import javax.vecmath.Point2i
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class DicomImageDouble(val pixelData: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]]) {
  type ImSeq[T] = scala.collection.immutable.IndexedSeq[T]
  def this(pixelData: scala.collection.Seq[scala.collection.Seq[Double]]) = this(pixelData.asInstanceOf[scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]]])
  def this(pixelData: scala.collection.IndexedSeq[scala.collection.IndexedSeq[Double]]) =
    this(pixelData.asInstanceOf[scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]]])
  def this(attributeList: AttributeList) = this(DicomImageDouble.getPixelData(attributeList))

  val Rows: Int = pixelData.size
  val Columns: Int = {
    if (pixelData.isEmpty) 0
    else pixelData.head.size
  }

  val width: Int = Columns
  val height: Int = Rows

  //noinspection ScalaWeakerAccess
  def getSubArray(rectangle: Rectangle): scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = {
    val x = (if (rectangle.getX < 0) 0 else rectangle.getX).toInt
    val y = (if (rectangle.getY < 0) 0 else rectangle.getY).toInt
    val w = (if (rectangle.getX + rectangle.getWidth > width) width - rectangle.getX else rectangle.getWidth).toInt
    val h = (if (rectangle.getY + rectangle.getHeight > height) height - rectangle.getY else rectangle.getHeight).toInt

    val pixDat = (y until h + y).map(row => pixelData(row).slice(x, x + w))
    pixDat
  }

  /**
    * Make a new <code>DicomImageDouble</code> based on a sub-rectangle of this one.  If part of the
    * specified rectangle is out of bounds a cropped version will be used.
    */
  //noinspection SpellCheckingInspection
  //noinspection ScalaWeakerAccess
  def getSubimage(rectangle: Rectangle): DicomImageDouble = {
    new DicomImageDouble(getSubArray(rectangle))
  }

  //noinspection ScalaWeakerAccess
  def get(x: Int, y: Int): Double = {
    try pixelData(y)(x)
    catch {
      case _: Throwable =>
        0.toDouble
    }
  }

  /** minimum pixel value. */
  lazy val minPixelValue: Double = pixelData.map(row => row.min).min

  /** maximum pixel value. */
  lazy val maxPixelValue: Double = pixelData.map(row => row.max).max

  //noinspection ScalaUnusedSymbol
  def validPoint(x: Int, y: Int): Boolean = {
    val ok = (x >= 0) && (x < width) && (y >= 0) && (y < height)
    ok
  }

  /**
    * Get the sums of the columns of <code>pixelData</code>
    */
  //noinspection ScalaWeakerAccess
  def columnSums: IndexedSeq[Double] = {
    def colSum(x: Int) = { for (y <- pixelData.indices) yield { get(x, y) } }.sum
    (0 until width).map(x => colSum(x))
  }

  /**
    * Get the sums of the rows of <code>pixelData</code>
    */
  //noinspection ScalaWeakerAccess
  def rowSums: IndexedSeq[Double] = {
    pixelData.map(row => row.sum)
  }

  /**
    * Get the sum of all pixels.
    */
  //noinspection ScalaWeakerAccess
  def sum: Double = rowSums.sum

  /**
    * Get a list of the smallest pixel values.
    *
    * @param count: Number of pixel values to get.
    */
  //noinspection ScalaUnusedSymbol
  def minPixelValues(count: Int): IndexedSeq[Double] = {
    pixelData.foldLeft(IndexedSeq[Double]())((list, row) => (list ++ row).sorted.take(count))
  }

  /**
    * Get a list of the largest pixel values.
    *
    * @param count: Number of pixel values to get.
    */
  //noinspection ScalaUnusedSymbol
  def maxPixelValues(count: Int): IndexedSeq[Double] = {
    pixelData.foldLeft(IndexedSeq[Double]())((list, row) => (row ++ list).sorted.takeRight(count))
  }

  /**
    * Use a quick but coarse algorithm to scan the entire image and make a list of the worst pixels.
    *
    * Note: As quick as this is, it takes about 7.5 seconds to process 1190x1190 image.
    *
    * @param maxBadPix: restrict count of bad pixels to this many
    *
    * @param radius Distance to the farthest pixel for comparison.  For example, a
    * value of 3 would mean that a (3*2+1)*(3*2+1) = 7*7 pixel square would be used.
    */
  private def findWorstPixelsQuickly(maxBadPix: Int, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImageDouble.PixelRating] = {

    val diam = radius * 2 + 1
    val area = diam * diam

    class WorstPixels {
      val buf: ArrayBuffer[DicomImageDouble.PixelRating] = scala.collection.mutable.ArrayBuffer(DicomImageDouble.PixelRating(-1, 0, 0))

      var smallestRating: Double = buf.head.rating
      var smallestIndex = 0

      def shouldPut(value: Double): Boolean = (value > smallestRating) || (buf.size < maxBadPix)

      def put(rating: Double, x: Int, y: Int): Any = {
        if (buf.size < maxBadPix) {
          buf += DicomImageDouble.PixelRating(rating, x, y)
        } else {
          buf(smallestIndex) = DicomImageDouble.PixelRating(rating, x, y)
          smallestRating = buf.head.rating
          smallestIndex = 0
          buf.indices.map(i =>
            if (buf(i).rating < smallestRating) {
              smallestRating = buf(i).rating
              smallestIndex = i
            }
          )
        }
      }
    }

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    case class Column(x: Int, rowList: IndexedSeq[Double]) {
      def this(x: Int) =
        this(
          x, {
            val s = startAt(x, width)
            (0 until height).map(y => pixelData(y).slice(s, s + diam).sum)
          }
        )

      /**
        * Get the next column by subtracting the first X value of each row and adding the next.
        */
      def next: Column = {
        if (x >= (width - 1)) {
          this // going off the end
        } else {
          if (startAt(x, width) == startAt(x + 1, width)) {
            Column(x + 1, rowList)
          } else {
            val row = (0 until height).map(y => {
              rowList(y) - pixelData(y)(x - radius) + pixelData(y)(x - radius + diam)
            })
            val newCol = Column(x + 1, row)
            newCol
          }
        }
      }
    }

    case class Row(y: Int, column: Column, total: Double) {
      def this(y: Int, column: Column) = this(y, column, column.rowList.slice(startAt(y, height), startAt(y, height) + diam).sum)
      val avg: Double = total / area

      /**
        * Rate the 'badness' of this pixel.  The higher the number, the worse it is.
        */
      def rate(v: Double): Double = {
        val diffAvg = (avg - v).abs // absolute difference from average
        if (diffAvg <= BadPixelMinimumDeviation_CU) 0
        else diffAvg / avg
      }

      def next: Row = {
        if (startAt(y, height) == startAt(y + 1, height))
          Row(y + 1, column, total)
        else {
          // subtract the first and add the next
          Row(y + 1, column, total - column.rowList(y - radius) + column.rowList(y + radius + 1))
        }
      }
    }

    val worstPixels = new WorstPixels

    def evalCol(col: Column): Unit = {
      val row = new Row(0, col)

      def evalRow(row: Row, y: Int): Row = {
        val rating = row.rate(get(col.x, y))
        if (worstPixels.shouldPut(rating))
          worstPixels.put(rating, col.x, y)
        row.next
      }

      (0 until height).foldLeft(row)((r, y) => evalRow(r, y))
    }

    (0 until width).foldLeft(new Column(0))((col, _) => {
      evalCol(col)
      col.next
    })

    val buf = worstPixels.buf.toList.asInstanceOf[scala.collection.immutable.Seq[DicomImageDouble.PixelRating]]

    DicomImageDouble.sortPixelRating(buf).take(maxBadPix)
  }

  private def scoreWithStandardDeviation(badList: Seq[DicomImageDouble.PixelRating], radius: Int): Seq[DicomImageDouble.PixelRating] = {

    val diam = radius * 2 + 1

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    val badSet = badList.map(b => (b.x, b.y)).toSet

    def isGood(x: Int, y: Int) = {
      !badSet.contains((x, y))
    }

    /**
      * Given a bad pixel, determine how many (multiples of) standard deviation it is from its neighbors.
      */
    def stdDev(bp: DicomImageDouble.PixelRating): DicomImageDouble.PixelRating = {
      val xs = startAt(bp.x, width)
      val ys = startAt(bp.y, height)
      val pixelValue = get(bp.x, bp.y)

      val goodList = for (x <- xs until (xs + diam); y <- ys until (ys + diam); if isGood(x, y)) yield get(x, y)

      val mean = goodList.sum / goodList.size

      def stdOf(v: Double) = {
        val vm = v - mean
        val s = vm * vm
        s
      }

      val variance = goodList.map(v => stdOf(v)).sum / Math.max(goodList.size, 1)
      val stdDev = Math.sqrt(variance)
      val rating = ((pixelValue - mean) / stdDev).abs

      DicomImageDouble.PixelRating(rating, bp.x, bp.y)
    }

    badList.map(bp => stdDev(bp))
  }

  /**
    * Public function wrapper to support testing.
    */
  //noinspection ScalaWeakerAccess
  def testFindWorstPixelsQuickly(maxBadPix: Int, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImageDouble.PixelRating] =
    findWorstPixelsQuickly(maxBadPix, radius, BadPixelMinimumDeviation_CU)

  //noinspection ScalaUnusedSymbol
  def identifyBadPixels(maxBadPixels: Int, stdDev: Double, BadPixelMaximumPercentChange: Double, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImageDouble.PixelRating] = {

    val coarseBadList = findWorstPixelsQuickly(maxBadPixels * 4, radius, BadPixelMinimumDeviation_CU)

    val limit = stdDev + 1
    val stdDeviationBadListUnsorted = scoreWithStandardDeviation(coarseBadList, radius).filter(bp => bp.rating >= limit)
    val sortedStdDeviationBadList = stdDeviationBadListUnsorted.sortBy(_.rating).takeRight(maxBadPixels).reverse

    //val outlierList = identifyLargeOutlierPixels(maxBadPixels, BadPixelMaximumPercentChange, sortedStdDeviationBadList)

    // only add bad pixels that were not previously discovered
    // val outlierNew = outlierList.filter(o => stdDeviationBadListUnsorted.find(w => ((w.x == o.x) && (w.y == o.y))).isEmpty)

    //(sortedStdDeviationBadList ++ outlierList).sortBy(_.rating).reverse
    sortedStdDeviationBadList
  }

  def correctBadPixels(badList: Seq[DicomImageDouble.PixelRating], radius: Int): DicomImageDouble = {
    val diam = radius * 2 + 1

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    val badSet = badList.map(b => (b.x, b.y)).toSet

    def isGood(x: Int, y: Int) = { !badSet.contains((x, y)) }

    val mutablePixelData = pixelData.map(row => row.toArray).toArray

    def correct(bp: DicomImageDouble.PixelRating): Unit = {
      val xs = startAt(bp.x, width)
      val ys = startAt(bp.y, height)
      val goodList = for (x <- xs until (xs + diam); y <- ys until (ys + diam); if isGood(x, y)) yield get(x, y)
      val mean = goodList.sum / goodList.size
      mutablePixelData(bp.y)(bp.x) = mean
    }

    badList.foreach(bp => correct(bp))
    new DicomImageDouble(mutablePixelData.map(row => row.toIndexedSeq).toIndexedSeq)
  }

  /**
    * Given a flood field, divide every pixel in this image by every pixel in the flood field.
    */
  //noinspection ScalaUnusedSymbol
  def biasCorrect(floodImage: DicomImageDouble): DicomImageDouble = {
    val unbiased = (0 until height).map(row => (0 until width).map(col => pixelData(row)(col) / floodImage.pixelData(row)(col)))
    new DicomImageDouble(unbiased)
  }

  /**
    * Construct a <code>BufferedImage</code>, mapping values low to high from black (0) to <code>color</code>.
    *
    * @param rgbColorMap: Colors to use from 0 to 255
    *
    * @param min: minimum pixel value
    *
    * @param max: maximum pixel value
    */
  //noinspection ScalaWeakerAccess
  def toBufferedImage(rgbColorMap: IndexedSeq[Int], min: Double, max: Double): BufferedImage = {

    val ratio = 256 / (max - min)

    /**
      * Given a pixel level, return the RGB color.
      */
    def levelToColor(level: Double): Int = {
      val i = ((level - min) * ratio).round.toInt

      i match {
        case _ if i < 0   => rgbColorMap(0)
        case _ if i > 255 => rgbColorMap(255)
        case _            => rgbColorMap(i)
      }
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width; y <- 0 until height) bufImage.setRGB(x, y, levelToColor(get(x, y)))

    bufImage
  }

  def toBufferedImage(rgbColorMap: IndexedSeq[Int]): BufferedImage = toBufferedImage(rgbColorMap, minPixelValue, maxPixelValue)

  def toBufferedImage(color: Color): BufferedImage = toBufferedImage(ImageUtil.rgbColorMap(color), minPixelValue, maxPixelValue)

  /**
    * Create a buffered image of this DICOM image using multiple colors to give a deeper color depth of 1276 than the standard 256.
    */
  //noinspection ScalaWeakerAccess
  def toDeepColorBufferedImage(minimumV: Double, maximumV: Double): BufferedImage = {

    val range = (maximumV - minimumV).abs

    /**
      * Given a pixel value, return an RGB color
      */
    def p2Color(pixel: Double): Int = {
      val scaledPixel = {
        { (pixel - minPixelValue) / range } match {
          case p if p < 0 => 0
          case p if p > 1 => 1
          case p          => p
        }
      }

      val a = (1 - scaledPixel) / 0.2
      val X = Math.floor(a).toInt
      val Y = Math.floor(255 * (a - X)).toInt

      val rgb = X match {
        case 0 => (255, Y, 0)
        case 1 => (255 - Y, 255, 0)
        case 2 => (0, 255, Y)
        case 3 => (0, 255 - Y, 255)
        case 4 => (Y, 0, 255)
        case 5 => (255, 0, 255)
        case _ => (255, 0, 255) // pixel outside range
      }
      (rgb._1 << 8) + (rgb._2 << 16) + rgb._3
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (row <- 0 until height; col <- 0 until width) bufImage.setRGB(col, row, p2Color(get(col, row)))

    bufImage
  }

//noinspection ScalaWeakerAccess
  def dropOutlierPixels(percentToDrop: Double): (Double, Double) = {
    val numDrop = ((Rows * Columns) * ((percentToDrop / 2) / 100.0)).round.toInt

    @tailrec
    def trim(total: Int, hist: Seq[DicomImageDouble.HistPoint]): Seq[DicomImageDouble.HistPoint] = {
      if ((total < numDrop) && hist.nonEmpty)
        trim(total + hist.head.count, hist.tail)
      else hist
    }

    if (histogram.size >= 1276) {
      val dropLo = trim(0, histogram)
      val minPix = if (dropLo.isEmpty) minPixelValue else dropLo.head.value
      val dropHi = trim(0, dropLo.reverse)
      val maxPix = if (dropHi.isEmpty) maxPixelValue else dropHi.head.value
      (minPix, maxPix)
    } else
      (minPixelValue, maxPixelValue)
  }

  /**
    * Make a deep-color buffered image of the image.
    *
    * @param percentToDrop The percent of pixels that will be dropped from the high and low end of the
    * range of values. This is to avoid letting a few bad pixels with extremely high or low values to expand the
    * pixel range that most of the image contrast is lost.  To ignore this effect and use the full pixel
    * range, use a value of 0.
    *
    * If a value of 0.5 is given, then 0.25 percent of the lowest valued pixels and 0.25 percent of the
    * highest valued pixels will be dropped, and instead of being given their true color will be given the
    * color for the lowest or highest value respectively.
    */
  def toDeepColorBufferedImage(percentToDrop: Double): BufferedImage = {
    val minMax = dropOutlierPixels(percentToDrop)
    toDeepColorBufferedImage(minMax._1, minMax._2)
  }

  /**
    * Generate a count of each pixel value.
    *
    * @return List tuples [(pixel value, number of pixels with that value)] sorted by pixel value.
    */
  lazy val histogram: Seq[DicomImageDouble.HistPoint] = {
    val list = pixelData.flatten.groupBy(identity).toList.map(g => DicomImageDouble.HistPoint(g._1, g._2.size)).sortWith((a, b) => a.value < b.value)
    list
  }

  //noinspection ScalaUnusedSymbol
  def binnedHistogram(size: Int): Seq[DicomImageDouble.HistPoint] = {
    val inc = (maxPixelValue - minPixelValue) / size
    def vToBin(v: Double): Int = ((v - minPixelValue) / inc).floor.toInt

    def binToV(bs: (Int, Int)): DicomImageDouble.HistPoint = DicomImageDouble.HistPoint((bs._1 * inc) + minPixelValue, bs._2)
    val list = pixelData.flatten.groupBy(v => vToBin(v)).toList.map(g => (g._1, g._2.size)).sortWith((a, b) => a._1 < b._1).map(bs => binToV(bs))
    list
  }

  /**
    * Create a copy rotated 90 degrees counterclockwise.  This is done in a loss-less way.
    */
  //noinspection ScalaWeakerAccess
  def rotate90: DicomImageDouble = {
    new DicomImageDouble(((width - 1) to 0 by -1).map(y => (0 until height).map(x => get(y, x))))
  }

  /**
    * Convert this image to an image that can be drawn as square pixels.  The resulting square pixels will
    * be of the size of the smaller dimension.  For example, if the pixels were 0.2 by 0.5, then the
    * resulting pixels would be 0.2 by 0.2 .
    *
    * If there is an extra fractional column (at the bottom) or row (at the right) then it will be removed.
    *
    */
  def renderPixelsToSquare(pixelWidth: Double, pixelHeight: Double): DicomImageDouble = {

    val aspectRatio = pixelWidth / pixelHeight

    /**
      * Make the image taller to accurately reflect the true height of the pixels.
      */
    def makeTaller: DicomImageDouble = {
      def makeRow(y: Int) = {
        val topSourcePix = (aspectRatio * y).floor.toInt
        val bottomSourcePix = (aspectRatio * (y + 1)).floor.toInt

        val separator = bottomSourcePix / aspectRatio

        def pixVal(x: Int): Double = {
          if ((topSourcePix == bottomSourcePix) || (separator == separator.floor))
            get(x, topSourcePix)
          else {
            val composite = ((separator - separator.floor) * get(x, topSourcePix)) + ((separator.ceil - separator) * get(x, bottomSourcePix))
            composite
          }
        }

        (0 until width).map(x => pixVal(x))
      }

      val numRows = (height / aspectRatio).floor.toInt
      val pixArray = (0 until numRows).map(y => makeRow(y))
      val di = new DicomImageDouble(pixArray)
      di
    }

    /**
      * Make the image wider to accurately reflect the true width of the pixels.
      */
    def makeWider: DicomImageDouble = {
      rotate90.renderPixelsToSquare(pixelHeight, pixelWidth).rotate90.rotate90.rotate90
    }

    aspectRatio match {
      case 1.0                   => this // essentially a no-op.
      case _ if aspectRatio <= 0 => throw new InvalidParameterException("Aspect ratio must be greater than zero.  Ratio given: " + aspectRatio)
      case _ if aspectRatio < 1  => makeTaller
      case _                     => makeWider
    }
  }

  /**
    * Get the coordinates of the maximum point in the image.  The standard deviation of the point must be at least
    * larger than the given standard deviation.  The reason for this is to reject instances where the image has no
    * clear maximum.
    */
  def getMaxPoint(minStdDev: Double): Option[Point2D.Double] = {

    /**
      * Find the max point and verify that it is sufficiently large (as
      * opposed to just background noise).
      */
    def locateAndValidate(profile: Seq[Double]): Option[Double] = {
      val maxX = LocateMax.locateMax(profile.map(_.toFloat))
      val spline = LocateMax.toCubicSpline(profile.map(_.toFloat))
      val maxY = spline.evaluate(maxX)

      val mean = profile.sum / profile.size
      val sd = ImageUtil.stdDev(profile.map(_.toFloat))
      val stdDevOfMax = (maxY - mean).abs / sd
      if (stdDevOfMax < minStdDev)
        None
      else
        Some(maxX)
    }

    val position = Seq(locateAndValidate(columnSums), locateAndValidate(rowSums))

    val flat = position.flatten
    if (flat.size == 2)
      Some(new Point2D.Double(flat.head, flat(1)))
    else
      None
  }

  /**
    * Get the coordinates of the upper left corner of the group of pixels of the given size that have the sum of the largest values.
    */
  def getMaxRect(xSize: Int, ySize: Int): Point2i = {
    val rectList = for (x <- 0 to (width - xSize); y <- 0 to (height - ySize)) yield { (x, y, getSubimage(new Rectangle(x, y, xSize, ySize)).sum) }
    val max = rectList.maxBy(_._3)
    new Point2i(max._1, max._2)
  }

  /**
    * Get the average pixel value within the bounds of the given rectangle.  If a
    * bound cuts through a pixel, then consider the partial weight of that pixel.
    *
    * Approach is to cut the area into 9 rectangles, center, 4 edges, and 4 corners.
    * Find the sum of each and use the grand sum to calculate the average.
    *
    * @param rect: Bounds of rectangle in pixel coordinates.
    */
  def averageOfRectangle(rect: Rectangle2D.Double): Double = {
    import java.awt.Rectangle

    // TODO this case could be handled, but there has not been a practical case that requires it.
    if (
      (rect.getX.floor == (rect.getX + rect.getWidth).floor) ||
      (rect.getX.floor == (rect.getX + rect.getWidth).floor)
    )
      throw new RuntimeException("Rectangle must specify area that spans more than one pixel both vertically and horizontally.  Rectangle given: " + rect)

    val r = rect.getX + rect.getWidth // right-hand edge
    val b = rect.getY + rect.getHeight // bottom edge

    // bounds of center pixels (which comprise the majority of them)
    val x = rect.getX.ceil.toInt
    val y = rect.getY.ceil.toInt
    val w = r.floor.toInt - x
    val h = b.floor.toInt - y

    val topFraction = rect.getY.ceil - rect.getY
    val botFraction = b - b.floor
    val lftFraction = rect.getX.ceil - rect.getX
    val rgtFraction = r - r.floor

    // Sum of large area in the middle
    val centerSum = getSubimage(new Rectangle(x, y, w, h)).sum

    val topSum = getSubimage(new Rectangle(x, rect.getY.floor.toInt, w, 1)).sum * topFraction
    val botSum = getSubimage(new Rectangle(x, b.floor.toInt, w, 1)).sum * botFraction
    val lftSum = getSubimage(new Rectangle(rect.getX.floor.toInt, y, 1, h)).sum * lftFraction
    val rgtSum = getSubimage(new Rectangle(r.floor.toInt, y, 1, h)).sum * rgtFraction

    val topLft = get(rect.getX.floor.toInt, rect.getY.floor.toInt) * topFraction * lftFraction
    val topRgt = get(r.floor.toInt, rect.getY.floor.toInt) * topFraction * rgtFraction
    val botLft = get(rect.getX.floor.toInt, b.floor.toInt) * botFraction * lftFraction
    val botRgt = get(r.floor.toInt, b.floor.toInt) * botFraction * rgtFraction

    val sum = centerSum + topSum + botSum + lftSum + rgtSum + topLft + topRgt + botLft + botRgt

    val avg = sum / (rect.getWidth * rect.getHeight)
    avg
  }

  /**
    * Return the pixels as rows and columns of numbers.  This is mostly a convenience function for debugging.
    */
  def pixelsToText: String = {

    // true if all pixels can be exactly represented as integers
    val allInt = pixelData.flatten.map(p => p.toInt == p).reduce(_ && _)

    val lenY = height.toString.length
    def fmtY(y: Int): String = s"%${lenY}d".format(y)

    if (allInt) {

      /** Maximum width needed to display any number. */
      val numLen = Seq[Double](minPixelValue, maxPixelValue, width, height).map(n => n.toInt.toString.length).max
      def fmtTxt(t: String) = t.format("%" + (numLen + 1) + "s")
      def fmt(f: Double) = ("%" + (numLen + 1) + "d").format(f.toInt)

      val header = fmtTxt("") + "     " + (0 until width).map(x => fmt(x)).mkString + "\n"
      def makeRow(y: Int) = fmtY(y) + " : " + pixelData(y).map(p => fmt(p)).mkString + "\n"

      val text = "\n" + header + (0 until height).map(y => makeRow(y)).mkString

      text
    } else {
      def fmtF(f: Double): String = {
        val text = "%32.16f".format("%6.3e".format(f).toDouble).replaceAll("0*$", "").trim
        if (text.endsWith(".")) text + "0" else text
      }
      val textList = pixelData.flatten.map(fmtF)
      val left = textList.map(t => t.replaceAll("\\..*", "").length).max
      val right = textList.map(t => t.replaceAll(".*\\.", "").length).max
      val len = left + right + 1 + 2 // width of number + decimal + spaces in between

      val fmt = s"%$left.${right}f"

      def makeRow(y: Int): String = fmtY(y) + " : " + pixelData(y).map(p => fmt.format(p)).mkString("  ") + "\n"

      val header = "   " + (0 until width).map(x => s"%${len}d".format(x)).mkString + "\n"

      val text = "\n" + header + (0 until height).map(y => makeRow(y)).mkString
      text
    }

  }

  /**
    * Create a new pixel array with the pixels processed with the given transform.
    * @param trans Converts a single pixel.
    * @return a new pixel array.
    */
  //noinspection ScalaWeakerAccess
  def transform(trans: Double => Double): scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = {
    pixelData.map(row => row.map(pixel => trans(pixel)))
  }

  /**
    * Scale all pixels: new = (old * RescaleSlope) + RescaleIntercept
    * @param RescaleSlope Multiply each pixel by this.
    * @param RescaleIntercept Then add this.
    * @return
    */
  //noinspection ScalaWeakerAccess
  def scalePixels(RescaleSlope: Double, RescaleIntercept: Double): DicomImageDouble = {
    def trans(value: Double): Double = (value * RescaleSlope) + RescaleIntercept
    new DicomImageDouble(transform(trans))
  }

  /**
    * Scale pixels according to the RescaleSlope and RescaleIntercept values in the given al.
    * @param al Attribute list for DICOM.
    * @return A new DICOM image with pixels scaled.
    */
  //noinspection ScalaUnusedSymbol
  def scalePixels(al: AttributeList): DicomImageDouble = {
    def floatOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head
    scalePixels(floatOf(TagByName.RescaleSlope), floatOf(TagByName.RescaleIntercept))
  }

  /**
    * Given a one-parameter function, perform that function on each pixel.
    * @param func Transforms a single pixel.
    * @return A new DicomImageDouble.
    */
  //noinspection ScalaUnusedSymbol
  def fun1(func: Double => Double): DicomImageDouble = {
    def row(y: Int): scala.collection.immutable.IndexedSeq[Double] =
      (0 until width).map(x => func(get(x, y)))
    val pix: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = (0 until height).map(row)

    new DicomImageDouble(pix)
  }

  /**
    * Given a two-parameter function, perform that function on each pixel of the pair of images.
    *
    * new pixel = func(oldPixel, otherPixel)
    *
    * @param func Transforms a single pixel.
    * @return A new DicomImageDouble.
    */
  //noinspection ScalaUnusedSymbol
  def fun2(func: (Double, Double) => Double, other: DicomImageDouble): DicomImageDouble = {
    def row(y: Int): scala.collection.immutable.IndexedSeq[Double] =
      (0 until width).map(x => func(get(x, y), other.get(x, y)))
    val pix: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = (0 until height).map(row)

    new DicomImageDouble(pix)
  }

  /**
    * Create a new DicomImageDouble with all pixels set to the range of 0 through 1.
    *
    * @param percentBadPixels Drop this many pixels from both the top and bottom range to avoid problems with bad pixels.
    * @return New image with pixels valued to 0 through 1.
    */
  //noinspection ScalaUnusedSymbol
  def normalize(percentBadPixels: Double): DicomImageDouble = {
    val numDrop = ((Rows * Columns) * (percentBadPixels / 100.0)).round.toInt

    val sorted = pixelData.flatten.sorted.drop(numDrop).dropRight(numDrop)

    val min = sorted.head
    val max = sorted.last
    val range = max - min

    def row(y: Int): scala.collection.immutable.IndexedSeq[Double] =
      (0 until width).map(x => (get(x, y) - min) / range)

    val pix: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = (0 until height).map(row)

    new DicomImageDouble(pix)
  }

}

object DicomImageDouble {

  case class PixelRating(rating: Double, x: Int, y: Int) { // extends Point(point.x, point.y) {
    override def toString: String = "rating: " + rating + " : " + x + ", " + y
    //noinspection ScalaUnusedSymbol
    val isInvalid: Boolean = rating.toString.toLowerCase.contains("n")
  }

  //noinspection ScalaUnusedSymbol
  def sortPixelRating1(a: PixelRating, b: PixelRating): Boolean = a.rating > b.rating

  //noinspection ScalaWeakerAccess
  def sortPixelRating(list: Seq[PixelRating]): Seq[PixelRating] = {
    val sorted = list.sortBy(_.rating).reverse
    sorted
  }

  case class HistPoint(value: Double, count: Int) {}

  /**
    * Add multiple histograms into a single histogram.
    * @param seq List of histograms.
    * @return Sum of all inputs.
    */
  def histogramSum(seq: Iterable[Iterable[HistPoint]]): Seq[HistPoint] = {
    val all = seq.flatten.groupBy(_.value).map(g => HistPoint(g._1, g._2.map(_.count).sum)).filter(_.count > 0).toSeq.sortBy(_.value)
    all
  }

  //  case class BadPixelSet(list: Seq[PixelRating], minBound: Double, maxBound: Double);

  //noinspection ScalaWeakerAccess
  def getPixelData(attributeList: AttributeList): scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[Double]] = {
    val pixDat = attributeList.getPixelData.getShortValues
    val Rows = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val Columns = attributeList.get(TagFromName.Columns).getIntegerValues()(0)

    val pixelDataDouble = (0 until Rows).map(c => pixDat.slice(c * Columns, c * Columns + Columns).toIndexedSeq.map(s => (s & 0xffff).toDouble))
    pixelDataDouble
  }
}
