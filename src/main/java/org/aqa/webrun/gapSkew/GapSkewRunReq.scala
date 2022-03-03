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
import org.aqa.Logging
import org.aqa.run.RunReqClass

/**
  * Container for validated data needed to run procedure.
  *
  * @param rtplan RTPLAN referenced.
  * @param rtimageMap Map of RTIMAGEs by beam name.
  */
case class GapSkewRunReq(rtplan: AttributeList, rtimageMap: Map[String, AttributeList]) extends RunReqClass with Logging {}
