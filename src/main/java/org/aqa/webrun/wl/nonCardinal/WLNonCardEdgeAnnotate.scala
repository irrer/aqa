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

package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging
import org.aqa.webrun.wl.WLMessage

/**
  * Create annotated images showing the edges.
  * @param edgeSet Set of edges found.
 * @param ball Ball measurements.
  * @param preprocessedImage DICOM image inverted as necessary.
  * @param al DICOM meta data.
  * @param wlMessage Log info and errors here.
  */
case class WLNonCardEdgeAnnotate( //
    edgeSet: WLNonCardEdgeSet,
    ball: WLNonCardBall,
    preprocessedImage: DicomImage,
    al: AttributeList,
    wlMessage: Option[WLMessage] = None
) extends Logging {}

object WLNonCardEdgeAnnotate extends Logging {
//
}
