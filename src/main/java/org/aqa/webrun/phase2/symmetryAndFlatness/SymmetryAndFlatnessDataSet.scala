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

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness

/**
 * Represent data needed to generate report for symmetry, flatness, and constancy
 *
 * @param symmetryAndFlatness Readings for a single beam.
 * @param output Output referenced by <code>symmetryAndFLatness</code>
 * @param baseline Baseline for this beam.  May be the same as <code>symmetryAndFLatness</code>
 * @param al Attribute list for this DICOM RTIMAGE.
 */
case class SymmetryAndFlatnessDataSet(symmetryAndFlatness: SymmetryAndFlatness, output: Output, baseline: SymmetryAndFlatness, al: AttributeList, rtplan: AttributeList) {
  val time: Long = Util.getOutputDateTime(Seq(al)).get
}
