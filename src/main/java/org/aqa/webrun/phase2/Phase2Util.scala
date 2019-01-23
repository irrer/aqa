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
import java.text.SimpleDateFormat
import org.aqa.db.CenterDose
import org.aqa.db.PMI
import org.aqa.db.Baseline
import org.aqa.web.PMIList
import org.aqa.web.MachineList
import org.aqa.web.MachineUpdate
import edu.umro.ScalaUtil.DicomUtil

/**
 * Utilities for Phase 2.
 */
object Phase2Util extends Logging {

  /**
   * Get the plan that this image references.  If it does not reference exactly one it will throw an exception.
   */
  def referencedPlanUID(rtimage: DicomFile): String = {
    val planSeqList = DicomUtil.seqToAttr(rtimage.attributeList.get, TagFromName.ReferencedRTPlanSequence)
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
      val j1 = DicomUtil.seqToAttr(image.attributeList.get, TagFromName.ReferencedRTPlanSequence).filter(al => doesRef(al)).nonEmpty
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
      val beam = DicomUtil.seqToAttr(plan.attributeList.get, TagFromName.BeamSequence).find(bs => bs.get(TagFromName.BeamNumber).getIntegerValues().head == ReferencedBeamNumber).get
      val BeamName = beam.get(TagFromName.BeamName).getSingleStringValueOrNull
      val bn = if (BeamName == null) None else Some(BeamName.trim)
      bn
    } catch {
      case t: Throwable => None
    }
  }

  /**
   * Given an RTPLAN and a beam name, get the beam sequence.
   */
  def getBeamSequenceOfPlan(beamName: String, plan: AttributeList): AttributeList = {
    val bs = DicomUtil.seqToAttr(plan, TagFromName.BeamSequence).filter(b => b.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(beamName.trim)).head
    bs
  }

  /**
   * Given an RTPLAN, a list of RTIMAGEs, and a BeamName, return the RTIMAGE associated with BeamName.
   */
  def findRtimageByBeamName(plan: DicomFile, rtimageList: IndexedSeq[DicomFile], BeamName: String): Option[DicomFile] = {
    val beam = rtimageList.map(rti => (rti, getBeamNameOfRtimage(plan, rti))).filter(rn => rn._2.isDefined && rn._2.get.equals(BeamName.trim))
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

    def mainReport: Elem = {
      val href = WebServer.urlOfResultsFile(extendedData.output.dir) + "/" + Output.displayFilePrefix + ".html"
      <div class="col-md-1 col-md-offset-1" title='Return to main (overview) report'><a href={ href }>Main Report</a></div>
    }

    val analysisDate: Date = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _ => extendedData.output.startDate
      }
      date
    }

    def dateToString(date: Option[Date]): String = {
      val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss")
      date match {
        case Some(date) => dateFormat.format(date)
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

    def wrap(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span AQAAlias="">{ value }</span>
        } else {
          val valueList = value.split("\n");
          { <span>{ valueList.head }{ valueList.tail.map(line => { <span><br/> { line } </span> }) }</span> }
        }

      { <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ html }</div> }

    }

    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")

    val dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }

    val div = {
      <div class="row">
        <div class="row">
          <div class="col-md-1 col-md-offset-1">{ passFailImage }</div>
          <div class="col-md-3" title={ title }><h2>{ title }</h2></div>
          <div class="col-md-1" title="Treatment machine. Click to view and modify machine configuration.">
            <h2>
              { MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, machineId) }
            </h2>
          </div>
          <div class="col-md-1">
            <span title="Machine Type">{ machType }</span>
            <br/>
            <span title="Multileaf Collimator">{ extendedData.multileafCollimator.model }</span>
          </div>
          <div class="col-md-2" title="Machine Type">
            <span title="EPID (electronic portal imaging device)">{ extendedData.epid.model }</span>
            <br/>
            <span title="X*Y pixel size in mm">{ pixelSpacing }</span>
          </div>
          <div class="col-md-1">
            <a href={ PMIList.path + "?machinePK=" + extendedData.machine.machinePK.get } title="View and modify maintenance events for this machine">Maintenance</a>
          </div>
        </div>
        <div class="row">
          { mainReport }
          { wrap(2, "Institution", extendedData.institution.name, true) }
          { wrap(1, "Data Acquisition", dataAcquisitionDate, false) }
          { wrap(1, "Analysis", twoLineDate.format(analysisDate), false) }
          { wrap(1, "User", userId, true) }
          { wrap(1, "Elapsed", elapsed, false) }
          { wrap(1, "Procedure", procedureDesc, false) }
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
  def identifyBadPixels(originalImage: DicomImage, radius: Int): Seq[DicomImage.PixelRating] = {
    val numPixels = originalImage.width * originalImage.height
    val sampleSize = ((Config.BadPixelSamplePerMillion / 1000000.0) * numPixels).round.toInt
    val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / 1000000.0) * numPixels).round.toInt
    val badPixels = originalImage.identifyBadPixels(maxBadPixels, Config.BadPixelStdDev, Config.BadPixelMaximumPercentChange, radius, Config.BadPixelMinimumDeviation_CU)
    badPixels
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
    (beamName.trim + "_" + jawDescription(al)).replaceAll("[^a-zA-Z0-9]", "_").replaceAll("__*", "_").replaceAll("_$", "").replaceAll("^_", "")
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

  def beamNameToId(beamName: String) = beamName.replaceAll("[# \"'@<>]", "_")

  /**
   * Create a list of points whose sum can be used to measure the center dose of an image.
   */
  def makeCenterDosePointList(attributeList: AttributeList): Seq[Point] = {
    val spacing = Phase2Util.getImagePlanePixelSpacing(attributeList)
    val width = attributeList.get(TagFromName.Columns).getIntegerValues().head
    val height = attributeList.get(TagFromName.Rows).getIntegerValues().head

    // get center of image, accounting for 1/2 pixel offset
    val xCenter = (width / 2.0) + 0.5 // in pixels
    val yCenter = (height / 2.0) + 0.5 // in pixels

    // center of image in mm
    val center = new Point2D.Double(xCenter * spacing.getX, yCenter * spacing.getY)

    val xRadius = (Config.CenterDoseRadius_mm / spacing.getX).toInt + 2 // in pixels
    val yRadius = (Config.CenterDoseRadius_mm / spacing.getY).toInt + 2 // in pixels

    val xRange = (xCenter - xRadius).floor.toInt to (xCenter + xRadius).ceil.toInt // pixel range
    val yRange = (yCenter - yRadius).floor.toInt to (yCenter + yRadius).ceil.toInt // pixel range

    // step through pixels and see which are close enough.  Both x and y are in pixels.
    def nearCenter(x: Int, y: Int): Boolean = center.distance(x * spacing.getX, y * spacing.getY) <= Config.CenterDoseRadius_mm

    val pointList = for (x <- xRange; y <- yRange; if nearCenter(x, y)) yield { new Point(x, y) }
    pointList
  }

  /**
   * Measure dose as specified by the list of points and return it in the proper units.
   */
  def measureDose(pointList: Seq[Point], dicomImage: DicomImage, attributeList: AttributeList): Double = {
    // average value raw pixel values
    val rawAverage = pointList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pointList.size

    val m = attributeList.get(TagFromName.RescaleSlope).getDoubleValues().head
    val b = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues().head
    val dose = (rawAverage * m) + b
    dose
  }

  case class PMIBaseline(pmi: Option[PMI], baseline: Baseline);

  /**
   * Look through the BeamSequence and find the ReferencedBeamSequence that matches the given beam.
   */
  def getBeamSequence(plan: AttributeList, beamNumber: Int): AttributeList = {
    // Determine if the given attribute list references the given beam number.
    def matchesBeam(beamNumber: Int, al: AttributeList): Boolean = (al.get(TagFromName.BeamNumber).getIntegerValues.head == beamNumber)

    DicomUtil.seqToAttr(plan, TagFromName.BeamSequence).find(bs => matchesBeam(beamNumber, bs)).get
  }

  /**
   * Return true if the collimator in the image is oriented horizontally, false for vertically.
   *
   * This is based on whether the rounded collimator angle is 0 or 180 for horizontal, 90 or 270 for vertical.
   *
   * Other collimator angles such as 45 degrees may give unexpected results.
   */
  def isHorizontal(image: AttributeList): Boolean = (Util.angleRoundedTo90(image.get(TagFromName.BeamLimitingDeviceAngle).getDoubleValues.head).toInt % 180) == 0

}
