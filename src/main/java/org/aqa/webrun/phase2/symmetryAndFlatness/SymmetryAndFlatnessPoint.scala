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

package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Logging

import java.awt.geom.Point2D

/**
  * Define a configured point to be used for Symmetry and flatness.
  */
case class SymmetryAndFlatnessPoint(name: String, x_mm: Double, y_mm: Double) extends Logging {

  override def toString = name.formatted("%-12s") + "  x, y: " + x_mm.formatted("%7.1f") + ", " + y_mm.formatted("%7.1f")

  val asPoint = new Point2D.Double(x_mm, y_mm)
}
