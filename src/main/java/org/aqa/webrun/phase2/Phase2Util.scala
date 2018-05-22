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
import java.awt.geom.Point2D

object Phase2Util extends Logging {

  /**
   * Get the plan that this image references.  If it does not reference exactly one it will throw an exception.
   */
  def referencedPlanUID(rtimage: DicomFile): String = {
    val planSeqList = Util.seq2Attr(rtimage.attributeList.get, TagFromName.ReferencedRTPlanSequence)
    val planUidList = planSeqList.map(al => al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull).filter(uid => uid != null).distinct
    if (planUidList.size != 1) throw new RuntimeException("RTIMAGE file should reference exactly one plan, but actually references " + planUidList.size)
    planUidList.head
  }

  /**
   * Determine if the given image references the given plan.
   */
  def imageReferencesPlan(plan: DicomFile, image: DicomFile): Boolean = {
    try {
      val planUID = plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
      def doesRef(al: AttributeList) = al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull.equals(planUID)
      Util.seq2Attr(image.attributeList.get, TagFromName.ReferencedRTPlanSequence).map(al => doesRef(al)).nonEmpty
    } catch {
      case t: Throwable => false
    }
  }

  /**
   * Get all RTPLAN files, including any that were just now downloaded and any that are in the
   * shared directory.  If a plan is found more than once then return only one
   * occurrence of it.  If the plan occurs both in the shared directory and was
   * downloaded, then prefer the one in the shared directory.
   */
  def getPlanList(dicomList: Seq[DicomFile]): Seq[DicomFile] = {
    val configuredPlans: Seq[DicomFile] = {
      try {
        DicomFile.readDicomInDir(Config.sharedDir).filter(df => df.isModality(SOPClass.RTPlanStorage))
      } catch {
        case t: Throwable => {
          logger.warn("Unexpected problem while getting pre-configured RTPLANs: " + t)
          Seq[DicomFile]()
        }
      }
    }

    val downloadedPlans: Seq[DicomFile] = try {
      val configSopList = configuredPlans.map(c => Util.sopOfAl(c.attributeList.get))
      dicomList.filter(df => df.isModality(SOPClass.RTPlanStorage)).filter(d => !configSopList.contains(Util.sopOfAl(d.attributeList.get)))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected problem while getting RTPLAN: " + t)
        Seq[DicomFile]()
      }
    }

    DicomFile.distinctSOPInstanceUID(configuredPlans ++ downloadedPlans)
  }

  /**
   * If the serial number for the machine is not already set, then set it by using the DeviceSerialNumber in the RTIMAGE.
   */
  def setMachineSerialNumber(machine: Machine, rtimage: AttributeList) = {
    if (machine.serialNumber.isEmpty) {
      try {
        val DeviceSerialNumber = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrNull
        if (DeviceSerialNumber != null) Machine.setSerialNumber(machine.machinePK.get, DeviceSerialNumber)
      } catch {
        case t: Throwable => logger.warn("Unable to update machine serial number " + machine + " : " + t)
      }
    }
  }

  /**
   * If a plan was used that was not already saved, then save it to the shared directory so that it will be available for future runs.
   */
  def saveRtplan(plan: DicomFile) = { // TODO plan is null!!??
    try {
      if (plan.file.getParentFile != Config.sharedDir) {
        val data = Util.readBinaryFile(plan.file)
        val planFile = new File(Config.sharedDir, plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull + Util.dicomFileNameSuffix)
        Util.writeBinaryFile(planFile, data.right.get)
        logger.info("Wrote new plan file " + planFile)
      }
    } catch {
      case t: Throwable => logger.error("Unable to save rtplan " + plan.file.getAbsolutePath + " : " + t)
    }
  }

  /**
   * Given an RTPLAN and an RTIMAGE, get the name of the beam that the RTIMAGE is referencing in the plan.
   */
  def getBeamNameOfRtimage(plan: DicomFile, rtimage: DicomFile): Option[String] = {
    try {
      val ReferencedBeamNumber = rtimage.attributeList.get.get(TagFromName.ReferencedBeamNumber).getIntegerValues().head
      val beam = Util.seq2Attr(plan.attributeList.get, TagFromName.BeamSequence).find(bs => bs.get(TagFromName.BeamNumber).getIntegerValues().head == ReferencedBeamNumber).get
      val BeamName = beam.get(TagFromName.BeamName).getSingleStringValueOrNull
      if (BeamName == null) None else Some(BeamName)
    } catch {
      case t: Throwable => None
    }
  }

  /**
   * Given an RTPLAN, a list of RTIMAGEs, and a BeamName, return the RTIMAGE associated with BeamName.
   */
  def findRtimageByBeamName(plan: DicomFile, rtimageList: IndexedSeq[DicomFile], BeamName: String): Option[DicomFile] = {
    val beam = rtimageList.map(rti => (rti, getBeamNameOfRtimage(plan, rti))).filter(rn => rn._2.isDefined && rn._2.get.equals(BeamName))
    if (beam.nonEmpty) Some(beam.head._1) else None
  }

  /**
   * Wrap Phase2 HTML with nice headers.
   */
  def wrapSubProcedure(output: Output, content: Elem, title: String, status: ProcedureStatus.Value): String = {

    def wrap(col: Int, name: String, value: String): Elem = {
      <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ value }</div>
    }

    val machine = if (output.machinePK.isDefined) Machine.get(output.machinePK.get) else None
    val institution = if (machine.isDefined) Institution.get(machine.get.institutionPK) else None
    val input = Input.get(output.inputPK)
    val procedure = Procedure.get(output.procedurePK)
    val user = if (output.userPK.isDefined) User.get(output.userPK.get) else None
    val institutionName = if (institution.isDefined) institution.get.name else "unknown"

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _ => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _ => "unknown"
      }
    }

    val machineId = if (machine.isDefined) machine.get.id else "unknown"
    val userId = if (user.isDefined) user.get.id else "unknown"

    val elapsed: String = {
      val fin = output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = {
      procedure match {
        case Some(proc) =>
          proc.name + " : " + proc.version
        case _ => ""
      }
    }

    val passFailImage = {
      if (status == ProcedureStatus.pass) {
        <div title="Passed!"><img src={ Config.passImageUrl } width="128"/></div>
      } else {
        <div title="Failed"><img src={ Config.failImageUrl } width="128"/></div>
      }
    }

    val div = {
      <div class="row col-md-10 col-md-offset-1">
        <div class="row">
          <div class="col-md-1">{ passFailImage }</div>
          <div class="col-md-3" title={ title }><h2>{ title }</h2></div>
          <div class="col-md-1" title="Machine"><h2>{ machineId }</h2></div>
        </div>
        <div class="row" style="margin:20px;">
          { wrap(1, "Institution", institutionName) }
          { wrap(2, "Data Acquisition", dateToString(output.dataDate)) }
          { wrap(2, "Analysis", analysisDate) }
          { wrap(1, "User", userId) }
          { wrap(1, "Elapsed", elapsed) }
          { wrap(3, "Procedure", procedureDesc) }
        </div>
        <div class="row" style="margin:20px;">
          { content }
        </div>
      </div>
    }

    // write the report to the output directory
    val text = wrapBody(div, title, None, true, None)
    text
  }

  /**
   * Create a corrected version of the given image using configured parameters.
   */
  def correctBadPixels(originalImage: DicomImage): DicomImage = {
    val numPixels = originalImage.width * originalImage.height
    val sampleSize = ((Config.BadPixelSamplePerMillion / 1000000.0) * numPixels).round.toInt
    val maxBadPixels = ((Config.MaxBadPixelPerMillion / 1000000.0) * numPixels).round.toInt
    val badPixelList = originalImage.identifyBadPixels(sampleSize, maxBadPixels, Config.BadPixelStdDevMultiple)
    originalImage.correctBadPixels(badPixelList)
  }

  def getImagePlanePixelSpacing(attributeList: AttributeList): Point2D.Double = {
    val ImagePlanePixelSpacing = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    new Point2D.Double(ImagePlanePixelSpacing(0), ImagePlanePixelSpacing(1))
  }
}