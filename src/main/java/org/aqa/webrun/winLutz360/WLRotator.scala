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
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil

import java.awt.geom.Point2D
import javax.vecmath.Point2d

/**
  * Utilities for rotating points to match collimator rotation.
  *
  * @param rtimage DICOM image containing geometric information.
  */
case class WLRotator(rtimage: AttributeList) {
  val trans: IsoImagePlaneTranslator = new IsoImagePlaneTranslator(rtimage)

  private val XRayImageReceptorTranslation = rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues
  private val offsetX = XRayImageReceptorTranslation.head
  private val offsetY = XRayImageReceptorTranslation(1)

  private val angle: Double = DicomUtil.findAllTag(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head

  private val radians: Double = Math.toRadians(angle)
  private val cos = Math.cos(radians)
  private val sin = Math.sin(radians)

  private val jaws = DicomUtil.findAllTag(rtimage, TagByName.LeafJawPositions)

  val jawsXLeft: Double = jaws.head.getDoubleValues.head
  val jawsXRight: Double = jaws.head.getDoubleValues()(1)

  private val jawsYTopStd = jaws(1).getDoubleValues.head
  private val jawsYBottomStd = jaws(1).getDoubleValues()(1)

  val jawsYTop: Double = -jawsYTopStd
  val jawsYBottom: Double = -jawsYBottomStd

  /**
    * Rotate and offset the given iso point according to the rtimage.
    * @param point Rotate this.
    * @return New point in iso coordinates.
    */
  def rot(point: Point2D.Double): Point2D.Double = {

    val y = -point.getY

    val xRot = (point.getX * cos) - (y * sin)
    val yRot = (point.getX * sin) + (y * cos)

    val xFinal = xRot - (offsetX / trans.beamExpansionRatio)
    val yFinal = -(yRot - (offsetY / trans.beamExpansionRatio))

    new Point2D.Double(xFinal, yFinal)
  }
}

object WLRotator {

  /**
   * Rotate the given point around the given center by the given angle.
   *
   * @param point The point to rotate
   * @param center The center to rotate around
   * @param degrees Angle in degrees
   * @return The rotated point.
   */
  def rotatePoint(point: Point2d, center: Point2d, degrees: Double): Point2d = {
    val radians = Math.toRadians(degrees) // Convert angle to radians
    // offest by center
    val dx = point.getX - center.getX
    val dy = point.getY - center.getY

    // Perform rotation
    val rotatedX = dx * Math.cos(radians) - dy * Math.sin(radians)
    val rotatedY = dx * Math.sin(radians) + dy * Math.cos(radians)

    // Translate back
    val finalX = rotatedX + center.getX
    val finalY = rotatedY + center.getY

    new Point2d(finalX, finalY)
  }

}
