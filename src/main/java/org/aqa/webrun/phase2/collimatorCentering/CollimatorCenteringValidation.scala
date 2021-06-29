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

package org.aqa.webrun.phase2.collimatorCentering

import org.aqa.Config
import org.aqa.webrun.phase2.Phase2Validation
import org.aqa.webrun.phase2.RunReq

/**
 * Validate the inputs for CollimatorCentering.  Require the 90 and 270 files to exist.
 */
object CollimatorCenteringValidation extends Phase2Validation {

  /**
   * Validate the given data.
   */

  override def validate(runReq: RunReq): Option[String] = {
    val file090 = runReq.rtimageMap.get(Config.CollimatorCentering090BeamName)
    val file270 = runReq.rtimageMap.get(Config.CollimatorCentering090BeamName)

    val result = (file090, file270) match {
      case (None, None) => Some("Neither the 90 image associated with beam " + Config.CollimatorCentering090BeamName + " or the " + "could not be found.")
      case (None, _) => Some("The 90 image associated with beam " + Config.CollimatorCentering090BeamName + " could not be found.")
      case (_, None) => Some("The 270 image associated with beam " + Config.CollimatorCentering270BeamName + " could not be found.")
      case (Some(f90), Some(f270)) => None // success
    }

    result
  }

}
