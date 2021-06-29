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

package org.aqa.webrun.phase2

import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import org.aqa.db.Machine
import com.pixelmed.dicom.SOPClass
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.Config
import org.aqa.db.Output
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.db.Input
import org.aqa.db.Procedure
import org.aqa.db.Institution
import org.aqa.db.User
import java.util.Date
import org.aqa.web.WebUtil._
import edu.umro.ImageUtil.DicomImage

abstract class Phase2Validation {
  def validate(runReq: RunReq): Option[String];
}