/*
 * Copyright 2025 Regents of the University of Michigan
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

package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.LocateEdge
import org.aqa.AQALine
import org.aqa.BiCubicImage

import javax.vecmath.Point2d
import scala.annotation.tailrec

/**
  *
  * @param name Name of edge
  * @param line Line that straddles the center of the profile.
  * @param offsetStart Starting offset of profile from the line's center point.
  * @param offsetFinish  Finishing offset of profile from the line's center point.
  * @param biCubicImage Bicubic version of image.
  * @param al Attribute list
  * @param width Width of sampling band in pixels.
  * @param resolution Resolution in pixels. 1 means same size as pixels, .5 means splitting pixels into 4 parts.
  */
case class Edge( //
    name: String,
    line: AQALine,
    offsetStart: Double,
    offsetFinish: Double,
    biCubicImage: BiCubicImage,
    al: AttributeList,
    width: Double,
    resolution: Double
) {

  private val positive = offsetFinish > offsetStart

  private val increment = if (positive) resolution else -resolution

  private val widthRounded: Int = (width / resolution).round.toInt

  private val RescaleSlope = al.get(TagByName.RescaleSlope).getDoubleValues.head
  private val RescaleIntercept = al.get(TagByName.RescaleIntercept).getDoubleValues.head

  /**
    * Convert a raw pixel value to CU.
    * @param pixelValue Value of pixel.
    * @return Value in CU.
    */
  private def toCu(pixelValue: Double): Double = {
    (pixelValue * RescaleSlope) + RescaleIntercept
  }

  /**
    * Make profile of the sum of values along the line.
    * @param offset:Distance from line's center point.
    * @param prof: Profile so far.
    * @return Profile of the sampled band.
    */
  @tailrec
  private def appendToProfile(offset: Double, prof: Seq[Double]): Seq[Double] = {
    if //
    (
      (positive && (offset <= offsetFinish)) || //
      ((!positive) && (offset >= offsetFinish))
    ) {
      val perpendicularLine = new AQALine(line.pointOn(offset), line.perpendicularAngle)

      val pointList = (0 until widthRounded).map(i => perpendicularLine.pointOn((i * resolution) - (width / 2)))
      val sum =
        try {
          val mean = pointList.map(biCubicImage.get).sum / pointList.size
          Some(mean)
        } catch {
          case _: org.apache.commons.math3.exception.OutOfRangeException =>
            None // out of bounds - just use what we've got so far
        }
      if (sum.isDefined)
        appendToProfile(offset + increment, prof :+ sum.get)
      else
        prof
    } else
      prof
  }

  /** the profile from the center (starting point) to the defined end of the AOI */
  private val centerToHiProfile: Seq[Double] = appendToProfile(offsetStart, Seq())

  /** Min profile value. */
  private val centerToHiMin: Double = centerToHiProfile.min

  /** Max profile value. */
  private val centerToHiMax: Double = centerToHiProfile.max

  /** Difference between max and min.  This is useful for gauging the validity of the edge.  This number should be
    * close to the overall range of the image.  If not, then this is probably not a Winston Lutz image.
    */
  val pixelValueRange: Double = centerToHiMax - centerToHiMin

  // Use this to define one edge of the AOI
  private val indexOfMin = centerToHiProfile.indexOf(centerToHiMin)

  /** List of pixel values from one end of the edge, through the transition, to the other end. */
  val edgeProfile: Seq[Double] = centerToHiProfile.drop(indexOfMin)

  private val edgeProfileMedian = (edgeProfile.min + edgeProfile.max) / 2

  /** Distance from the point to the edge. */
  val position: Double = (LocateEdge.locateEdge(edgeProfile.map(_.toFloat).toIndexedSeq, edgeProfileMedian) + indexOfMin) * resolution

  /** Point where the WL edge  */
  val edgeCenter: Point2d = line.pointOn(if (offsetFinish > 0) position else -position)

  /** Line at the nearest (from line's center) edge of the AOI. */
  val loLine: AQALine = {
    val distance = indexOfMin * resolution
    val offset = if (positive) distance else -distance
    new AQALine(line.pointOn(offset), line.perpendicularAngle)
  }

  val loLoAoi: Point2d = loLine.pointOn(-width / 2)

  val loHiAoi: Point2d = loLine.pointOn(width / 2)

  /** Line at the farthest (from line's center) edge of the AOI. */
  val hiLine: AQALine = new AQALine(line.pointOn(offsetFinish), line.perpendicularAngle)

  val hiLoAoi: Point2d = hiLine.pointOn(-width / 2)

  val hiHiAoi: Point2d = hiLine.pointOn(width / 2)

  /** Line at the edge that was found. */
  val edgeLine = new AQALine(edgeCenter, line.perpendicularAngle)

  val edgeLo: Point2d = edgeLine.pointOn(-width / 2)

  val edgeHi: Point2d = edgeLine.pointOn(width / 2)

  private def makeGradient(): Seq[Double] = {

    val gradientResolution = 0.5

    val sign = if (positive) 1 else -1

    def make1Column(point: Point2d): Option[Double] = {
      try {
        val col = AQALine(point, line.angle_deg)

        val count = (loLine.centerPoint.distance(hiLine.centerPoint) / gradientResolution).round.toInt

        val valueList = (0 until count).map(i => {
          val p = col.pointOn(i * gradientResolution * sign)
          biCubicImage.get(p)
          // jBiCub.get(p)
        })

        Some(valueList.sum / valueList.size)
      } catch {
        case _: Throwable =>
          None
      }
    }

    val colCount = (width / gradientResolution).round.toInt

    val offset = -width / 2

    val gr = (0 until colCount).flatMap(col => make1Column(loLine.pointOn(offset + (col * gradientResolution))))

    gr
  }

  /** Gradient showing how straight the edge is.  This can be thought of as a profile at a right angle to the edge's profile. */
  val gradient: Seq[Double] = makeGradient()

}
