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