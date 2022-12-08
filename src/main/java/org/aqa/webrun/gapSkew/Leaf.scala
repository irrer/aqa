/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.gapSkew

/**
  * Describe one leaf position.
  *
  * @param yPosition_mm Vertical position of leaf in mm in isoplane.
  * @param xLeftPosition_mm Horizontal left side of measurement in mm in isoplane.
  * @param width_mm Width of measurement (distance across leaf) in mm in isoplane.
  */
case class Leaf(yPosition_mm: Double, xLeftPosition_mm: Double, width_mm: Double) {

  val xCenter_mm: Double = xLeftPosition_mm + (width_mm / 2)

  override def toString: String =
    yPosition_mm + "   xCenter_mm , width (mm): " + xCenter_mm + ", " + width_mm
}
