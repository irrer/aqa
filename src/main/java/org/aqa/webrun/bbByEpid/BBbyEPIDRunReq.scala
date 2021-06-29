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

package org.aqa.webrun.bbByEpid

import org.aqa.db.Machine
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass
import edu.umro.DicomDict.TagByName

/**
 * Data needed to run an EPID BB analysis.
 */
case class BBbyEPIDRunReq(epidList: Seq[AttributeList]) extends RunReqClass {

  val sopOfRTPlan: Option[String] = {
    try {
      val sop = DicomUtil.seqToAttr(epidList.head, TagByName.ReferencedRTPlanSequence).
        head.
        get(TagFromName.ReferencedSOPInstanceUID).
        getSingleStringValueOrNull
      if (sop == null)
        None
      else
        Some(new String(sop))
    } catch {
      case t: Throwable => None
    }
  }

}
