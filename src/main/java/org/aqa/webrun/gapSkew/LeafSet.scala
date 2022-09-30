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

import com.pixelmed.dicom.AttributeList
import org.aqa.db.GapSkew

import java.awt.image.BufferedImage

/**
  * Describe a set of leaf results with enough information to make a web page.
  *
  * @param image Image showing analysis results.
  * @param attributeList RTIMAGE DICOM
  * @param rtplan RTPLAN DICOM
  * @param measurementSpan_mm Length of measurement taken to establish leaf position.
  *
  * @param gapSkew Measurements.
  */
case class LeafSet(
    image: BufferedImage,
    attributeList: AttributeList,
    rtplan: AttributeList,
    measurementSpan_mm: Double,
    gapSkew: Either[String, GapSkew] // Left: processing error, Right: Results of GapSkew
) {}
