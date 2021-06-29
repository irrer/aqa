/*
 * Copyright 2021 Regents of the University of Michigan
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

import org.aqa.Util

import javax.vecmath.Point2d

/**
  * Describe one leaf position and the profile surrounding it.
  * @param position_mm Position of leaf in mm in isoplane.
  * @param xPosition_mm Left side of measurement in mm in isoplane.
  * @param width_mm Width of measurement (distance across leaf) in mm in isoplane.
  * @param profile X,Y graph to plot showing the profile.
  */
case class Leaf(position_mm: Double, xPosition_mm: Double, width_mm: Double, profile: Seq[ProfilePoint]) {
  override def toString =
    position_mm +
      "   x, width (mm): " + xPosition_mm + ", " + width_mm +
      "    profile: " + profile.map(p => Util.fmtDbl(p.y_mm) + ", " + Util.fmtDbl(p.cu)).mkString("    ")
}
