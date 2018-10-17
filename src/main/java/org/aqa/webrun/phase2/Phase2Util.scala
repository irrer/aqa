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
import java.awt.Point
import org.aqa.db.MachineType
import org.aqa.web.WebServer

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
      def doesRef(al: AttributeList) = {
        val imgRef = al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
        val j = imgRef.equals(planUID)
        imgRef.equals(planUID)
      }
      val j1 = Util.seq2Attr(image.attributeList.get, TagFromName.ReferencedRTPlanSequence).filter(al => doesRef(al)).nonEmpty
      j1
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
  def saveRtplan(plan: DicomFile) = {
    if (plan == null) {
      logger.error("Was given null RTPLAN DicomFile")
    } else {
      try {
        if (plan.file.getParentFile != Config.sharedDir) {
          val data = Util.readBinaryFile(plan.file)
          if (data.isLeft) {
            logger.error("Could not read RTPLAN file " + plan.file + " : " + fmtEx(data.left.get))
          } else {
            val planFile = new File(Config.sharedDir, plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull + Util.dicomFileNameSuffix)
            Util.writeBinaryFile(planFile, data.right.get)
            logger.info("Wrote new plan file " + planFile)
          }
        }
      } catch {
        case t: Throwable => logger.error("Unable to save rtplan " + plan.file.getAbsolutePath + " : " + fmtEx(t))
      }
    }
  }

  /**
   * Given an RTPLAN and an RTIMAGE, get the name of the beam that the RTIMAGE is referencing in the plan.
   */
  def getBeamNameOfRtimage(plan: DicomFile, rtimage: DicomFile): Option[String] = {
    try {
      val ReferencedBeamNumber = rtimage.attributeList.get.get(TagFromName.ReferencedBeamNumber).getIntegerValues.head
      val beam = Util.seq2Attr(plan.attributeList.get, TagFromName.BeamSequence).find(bs => bs.get(TagFromName.BeamNumber).getIntegerValues().head == ReferencedBeamNumber).get
      val BeamName = beam.get(TagFromName.BeamName).getSingleStringValueOrNull
      val bn = if (BeamName == null) None else Some(BeamName)
      println("getBeamNameOfRtimage  file: " + rtimage.file.getName +
        "    ReferencedBeamNumber: " + ReferencedBeamNumber +
        "    bn: " + bn +
        "    beam: " + beam.toString.replace('\0', ' ') + "\n")
      bn
    } catch {
      case t: Throwable => None
    }
  }

  /**
   * Given an RTPLAN and a beam name, get the beam sequence.
   */
  def getBeamSequenceOfPlan(beamName: String, plan: AttributeList): AttributeList = {
    val bs = Util.seq2Attr(plan, TagFromName.BeamSequence).filter(b => b.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(beamName)).head
    bs
  }

  /**
   * Given an RTPLAN, a list of RTIMAGEs, and a BeamName, return the RTIMAGE associated with BeamName.
   */
  def findRtimageByBeamName(plan: DicomFile, rtimageList: IndexedSeq[DicomFile], BeamName: String): Option[DicomFile] = {
    val beam = rtimageList.map(rti => (rti, getBeamNameOfRtimage(plan, rti))).filter(rn => rn._2.isDefined && rn._2.get.equals(BeamName))
    if (beam.nonEmpty) Some(beam.head._1) else None
  }

  /**
   * Given a status, determine if it is 'good'.
   */
  def statusOk(status: ProcedureStatus.Value): Boolean = {
    Seq(ProcedureStatus.pass, ProcedureStatus.done).find(s => s.toString.equals(status.toString)).isDefined
  }

  /**
   * Wrap Phase2 HTML with nice headers.
   */
  def wrapSubProcedure(extendedData: ExtendedData, content: Elem, title: String, status: ProcedureStatus.Value, runScript: Option[String], runReq: RunReq): String = {

    def wrap(col: Int, name: String, value: String): Elem = {
      <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ value }</div>
    }

    def mainReport: Elem = {
      val href = WebServer.urlOfResultsFile(extendedData.output.dir) + "/" + Output.displayFilePrefix + ".html"
      <div class="col-md-1 col-md-offset-1" title='Return to main (overview) report'><a href={ href }>Main Report</a></div>
    }

    val analysisDate: String = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _ => extendedData.output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _ => "unknown"
      }
    }

    val machineId = extendedData.machine.id
    val userId = extendedData.user.id

    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val passFailImage = {
      if (statusOk(status)) {
        <div title="Passed!"><img src={ Config.passImageUrl } width="128"/></div>
      } else {
        <div title="Failed"><img src={ Config.failImageUrl } width="128"/></div>
      }
    }

    val machType = extendedData.machineType.manufacturer + " " + extendedData.machineType.model + " " + extendedData.machineType.version

    val pixelSpacing = {
      runReq.imageSize.getX.round.toInt.toString + " * " + runReq.imageSize.getY.round.toInt.toString + " pixels " +
        runReq.ImagePlanePixelSpacing.getX.formatted("%5.3f") + " * " + runReq.ImagePlanePixelSpacing.getY.formatted("%5.3f") + " mm"
    }

    val div = {
      <div class="row">
        <div class="row">
          <div class="col-md-1 col-md-offset-1">{ passFailImage }</div>
          <div class="col-md-3" title={ title }><h2>{ title }</h2></div>
          <div class="col-md-1" title="Machine"> <h2>{ machineId }</h2></div>
          <div class="col-md-2">
            <span title="Machine Type">{ machType }</span>
            <br/>
            <span title="Multileaf Collimator">{ extendedData.multileafCollimator.model }</span>
          </div>
          <div class="col-md-2" title="Machine Type">
            <span title="EPID (electronic portal imaging device)">{ extendedData.epid.model }</span>
            <br/>
            <span title="X*Y pixel size in mm">{ pixelSpacing }</span>
          </div>
        </div>
        <div class="row">
          { mainReport }
          { wrap(1, "Main Report", extendedData.institution.name) }
          { wrap(2, "Institution", extendedData.institution.name) }
          { wrap(2, "Data Acquisition", dateToString(extendedData.output.dataDate)) }
          { wrap(2, "Analysis", analysisDate) }
          { wrap(1, "User", userId) }
          { wrap(1, "Elapsed", elapsed) }
          { wrap(3, "Procedure", procedureDesc) }
        </div>
        <div class="row">
          { content }
        </div>
      </div>
    }

    // write the report to the output directory
    val text = wrapBody(div, title, None, true, runScript)
    text
  }

  /**
   * Get a list of bad pixels in the given image according to the configuration for Phase 2.
   */
  def identifyBadPixels(originalImage: DicomImage): IndexedSeq[Point] = {
    val numPixels = originalImage.width * originalImage.height
    val sampleSize = ((Config.BadPixelSamplePerMillion / 1000000.0) * numPixels).round.toInt
    val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / 1000000.0) * numPixels).round.toInt
    val badPixelList = originalImage.identifyBadPixels(sampleSize, maxBadPixels, Config.BadPixelStdDev)
    badPixelList
  }

  //  /**
  //   * Create a corrected version of the given image using configured parameters.
  //   */
  //  def correctBadPixels(originalImage: DicomImage, badPixelList: IndexedSeq[Point]): DicomImage = {
  //    val numPixels = originalImage.width * originalImage.height
  //    val sampleSize = ((Config.BadPixelSamplePerMillion / 1000000.0) * numPixels).round.toInt
  //    val maxBadPixels = ((Config.MaxBadPixelPerMillion / 1000000.0) * numPixels).round.toInt
  //    val badPixelList = originalImage.identifyBadPixels(sampleSize, maxBadPixels, Config.BadPixelStdDevMultiple)
  //    originalImage.correctBadPixels(badPixelList)
  //  }

  def getImagePlanePixelSpacing(attributeList: AttributeList): Point2D.Double = {
    val ImagePlanePixelSpacing = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    new Point2D.Double(ImagePlanePixelSpacing(0), ImagePlanePixelSpacing(1))
  }

  /**
   * HTML snippet to describe a crash.
   */
  def procedureCrash(name: String) = {
    <div>
      { name + " crashed" }<br/>
      <img src={ Config.failImageUrl } height="32"/>
    </div>
  }

  def jawDescription(al: AttributeList): String = {
    try {
      val jaws = MeasureTBLREdges.imageCollimatorPositions(al)
      val width = ((jaws.X1.abs + jaws.X2.abs) / 10).round.toInt
      val height = ((jaws.Y1.abs + jaws.Y2.abs) / 10).round.toInt
      width + " x " + height + " cm"
    } catch {
      case t: Throwable => ""
    }
  }

  def angleDescription(al: AttributeList): String = {
    try {
      val g = al.get(TagFromName.GantryAngle).getDoubleValues.head
      val c = al.get(TagFromName.BeamLimitingDeviceAngle).getDoubleValues.head

      "G" + Util.angleRoundedTo90(g) + " C" + Util.angleRoundedTo90(c)
    } catch {
      case t: Throwable => ""
    }
  }

  def dicomViewBaseName(beamName: String, al: AttributeList): String = {
    (beamName + "_" + jawDescription(al)).replaceAll("[^a-zA-Z0-9]", "_").replaceAll("__*", "_").replaceAll("_$", "").replaceAll("^_", "")
  }

  def dicomViewHtmlFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val htmlFile = dicomViewBaseName(runReq.beamNameOfAl(al), al) + ".html"
    val viewDir = new File(extendedData.output.dir, "view")
    new File(viewDir, htmlFile)
  }

  def dicomViewImageHtmlFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val htmlFile = dicomViewBaseName(runReq.beamNameOfAl(al), al) + "_image.html"
    val viewDir = new File(extendedData.output.dir, "view")
    new File(viewDir, htmlFile)
  }

  def dicomViewImageFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val pngFile = dicomViewBaseName(runReq.beamNameOfAl(al), al) + ".png"
    val viewDir = new File(extendedData.output.dir, "view")
    new File(viewDir, pngFile)
  }

  def dicomViewHref(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): String = {
    WebServer.urlOfResultsFile(dicomViewHtmlFile(al, extendedData, runReq))
  }

  def dicomViewImageHref(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): String = {
    WebServer.urlOfResultsFile(dicomViewImageFile(al, extendedData, runReq))
  }

  def dicomViewImageHtmlHref(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): String = {
    WebServer.urlOfResultsFile(dicomViewImageHtmlFile(al, extendedData, runReq))
  }

}
