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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.MachineUpdate
import org.aqa.web.MaintenanceRecordList
import org.aqa.web.OutputList
import org.aqa.web.WebServer
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData

import java.awt.Point
import java.awt.geom.Point2D
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
  * Utilities for Phase 2.
  */
object Phase2Util extends Logging {

  /**
    * Get the plan that this image references.  If it does not reference exactly one it will throw an exception.
    */
  def referencedPlanUID(rtimage: AttributeList): String = {
    val planSeqList = DicomUtil.seqToAttr(rtimage, TagByName.ReferencedRTPlanSequence)
    val planUidList = planSeqList.map(al => al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull).filter(uid => uid != null).distinct
    if (planUidList.size != 1) throw new RuntimeException("RTIMAGE file should reference exactly one plan, but actually references " + planUidList.size)
    planUidList.head
  }

  /**
    * Determine if the given image references the given plan.
    */
  def imageReferencesPlan(plan: AttributeList, image: AttributeList): Boolean = {
    try {
      val planUID = plan.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull

      def doesRef(al: AttributeList) = {
        val imgRef = al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
        imgRef.equals(planUID)
      }

      val doesRefPlan = DicomUtil.seqToAttr(image, TagByName.ReferencedRTPlanSequence).exists(al => doesRef(al))
      doesRefPlan
    } catch {
      case _: Throwable => false
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
        DicomFile.readDicomInDir(Config.sharedDir).filter(df => df.isRtplan)
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected problem while getting pre-configured RTPLAN(s): " + t)
          Seq[DicomFile]()
      }
    }

    val downloadedPlans: Seq[DicomFile] =
      try {
        val configSopList = configuredPlans.map(c => Util.sopOfAl(c.attributeList.get))
        dicomList.filter(df => df.isRtplan).filter(d => !configSopList.contains(Util.sopOfAl(d.attributeList.get)))
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected problem while getting RTPLAN: " + t)
          Seq[DicomFile]()
      }

    DicomFile.distinctSOPInstanceUID(configuredPlans ++ downloadedPlans)
  }

  /**
    * If the serial number for the machine is not already set, then set it by using the DeviceSerialNumber in the RTIMAGE.
    */
  def setMachineSerialNumber(machine: Machine, rtimage: AttributeList): Unit = {
    if (machine.serialNumber.isEmpty) {
      try {
        val DeviceSerialNumber = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrNull
        logger.info("Saving machine " + machine.id + "'s serial number " + DeviceSerialNumber)
        if (DeviceSerialNumber != null) Machine.setSerialNumber(machine.machinePK.get, DeviceSerialNumber)
      } catch {
        case t: Throwable => logger.warn("Unable to update machine serial number " + machine + " : " + t)
      }
    }
  }

  /**
    * If a plan was used that was not already saved, then save it to the shared directory so that it will be available for future runs.
    */
  def saveRtplan(plan: DicomFile): Unit = {
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
  def getBeamNameOfRtimage(plan: AttributeList, rtimage: AttributeList): Option[String] = {
    try {
      val ReferencedBeamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
      val beam = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).find(bs => bs.get(TagByName.BeamNumber).getIntegerValues.head == ReferencedBeamNumber).get
      val BeamName = Util.normalizedBeamName(beam)
      val bn = if (BeamName == "") None else Some(BeamName.trim)
      bn
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Given an RTPLAN and an RTIMAGE, get the name of the beam that the RTIMAGE is referencing in the plan.
    */
  def getBeamNameOfRtimageDf(plan: DicomFile, rtimage: DicomFile): Option[String] = getBeamNameOfRtimage(plan.attributeList.get, rtimage.attributeList.get)

  /**
    * Given an RTPLAN and a beam name, get the beam sequence.
    */
  def getBeamSequenceOfPlan(beamName: String, plan: AttributeList): AttributeList = {
    val bs = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).filter(b => Util.normalizedBeamName(b).equals(beamName.trim)).head
    bs
  }

  /**
    * Given an RTPLAN, a list of RTIMAGE(s), and a BeamName, return the RTIMAGE associated with BeamName.
    */
  def findRtimageByBeamName(plan: DicomFile, rtimageList: IndexedSeq[DicomFile], BeamName: String): Option[DicomFile] = {
    val beam = rtimageList.map(rti => (rti, getBeamNameOfRtimageDf(plan, rti))).filter(rn => rn._2.isDefined && rn._2.get.equals(BeamName.trim))
    if (beam.nonEmpty) Some(beam.head._1) else None
  }

  /**
    * Given a status, determine if it is 'good'.
    */
  def statusOk(status: ProcedureStatus.Value): Boolean = {
    Seq(ProcedureStatus.pass, ProcedureStatus.done).exists(s => s.toString.equals(status.toString))
  }

  /**
    * Wrap Phase2 HTML with nice headers.
    */
  def wrapSubProcedure(extendedData: ExtendedData, content: Elem, title: String, status: ProcedureStatus.Value, runScript: Option[String], runReq: RunReq): String = {

    def mainReport: Elem = {
      val href = WebServer.urlOfResultsFile(extendedData.output.dir) + "/" + Output.displayFilePrefix + ".html"
      <div class="col-md-1 col-md-offset-1" title='Return to main (overview) report'>
        <a href={href}>Main Report</a>
      </div>
    }

    val analysisDate: Date = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _       => extendedData.output.startDate
      }
      date
    }

    val machineId = extendedData.machine.id
    val userId = extendedData.user.id

    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val passFailImage = {
      if (statusOk(status)) {
        <div title="Passed!">
          <img src={Config.passImageUrl} width="128"/>
        </div>
      } else {
        <div title="Failed">
          <img src={Config.failImageUrl} width="128"/>
        </div>
      }
    }

    val machType = extendedData.machineType.manufacturer + " " + extendedData.machineType.model + " " + extendedData.machineType.version

    val pixelSpacing = {
      runReq.imageSize.getX.round.toInt.toString + " * " + runReq.imageSize.getY.round.toInt.toString +
        " pixels " + runReq.floodTranslator.pix2IsoDistX(1).formatted("%5.3f") + " * " + runReq.floodTranslator.pix2IsoDistY(1).formatted("%5.3f") +
        " mm"
    }

    def wrap(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span aqaalias="">
            {value}
          </span>
        } else {
          val valueList = value.split("\n");
          {
            <span>
              {valueList.head}{
              valueList.tail.map(line => {
                <span>
                <br/>{line}
              </span>
              })
            }
            </span>
          }
        }

      {
        <div class={"col-md-" + col}>
          <em>
            {name}
            :</em> <br/>{html}
        </div>
      }

    }

    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")

    val dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }

    val div = {
      <div class="row">
        <div class="row">
          <div class="col-md-1 col-md-offset-1">
            {passFailImage}
          </div>
          <div class="col-md-3" title={title}>
            <h2>
              {title}
            </h2>
          </div>
          <div class="col-md-1" title="Treatment machine. Click to view and modify machine configuration.">
            <h2>
              {MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, machineId)}
            </h2>
          </div>
          <div class="col-md-1">
            <span title="Machine Type">
              {machType}
            </span>
            <br/>
            <span title="Multileaf Collimator">
              {extendedData.multileafCollimator.model}
            </span>
          </div>
          <div class="col-md-2" title="Machine Type">
            <span title="EPID (electronic portal imaging device)">
              {extendedData.epid.model}
            </span>
            <br/>
            <span title="X*Y pixel size in mm">
              {pixelSpacing}
            </span>
          </div>
          <div class="col-md-1">
            <a href={MaintenanceRecordList.path + "?machinePK=" + extendedData.machine.machinePK.get} title="View and modify maintenance events for this machine">Maintenance</a>
          </div>
        </div>
        <div class="row">
          {mainReport}{wrap(2, "Institution", extendedData.institution.name, asAlias = true)}{wrap(1, "Data Acquisition", dataAcquisitionDate, asAlias = false)}{
        wrap(1, "Analysis", twoLineDate.format(analysisDate), asAlias = false)
      }{wrap(1, "User", userId, asAlias = true)}{wrap(1, "Elapsed", elapsed, asAlias = false)}{wrap(1, "Procedure", procedureDesc, asAlias = false)}<div class="col-md-1">
          {OutputList.redoUrl(extendedData.output.outputPK.get)}
        </div>
        </div>
        <div class="row">
          {content}
        </div>
      </div>
    }

    // write the report to the output directory
    val text = wrapBody(div, title, None, c3 = true, runScript)
    text
  }

  /**
    * Get a list of bad pixels in the given image according to the configuration for Phase 2.
    */
  def identifyBadPixels(originalImage: DicomImage, radius: Int): Seq[DicomImage.PixelRating] = {
    val numPixels = originalImage.width * originalImage.height
    val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / 1000000.0) * numPixels).round.toInt
    val badPixels = originalImage.identifyBadPixels(maxBadPixels, Config.BadPixelStdDev, Config.BadPixelMaximumPercentChange, radius, Config.BadPixelMinimumDeviation_CU)
    badPixels.filter(bp => bp.rating > 100)
  }

  def getImagePlanePixelSpacing(attributeList: AttributeList): Point2D.Double = {
    val ImagePlanePixelSpacing = attributeList.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
    new Point2D.Double(ImagePlanePixelSpacing(0), ImagePlanePixelSpacing(1))
  }

  /**
    * HTML snippet to describe a crash.
    */
  def procedureCrash(name: String): Elem = {
    <div>
      {name + " crashed"}<br/>
      <img src={Config.failImageUrl} height="32"/>
    </div>
  }

  def jawDescription(rtimage: AttributeList, rtplan: AttributeList): String = {
    try {
      val jaws = MeasureTBLREdges.imageCollimatorPositions(rtimage, rtplan)
      val width = ((jaws.X1.abs + jaws.X2.abs) / 10).round.toInt
      val height = ((jaws.Y1.abs + jaws.Y2.abs) / 10).round.toInt
      width + " x " + height + " cm"
    } catch {
      case _: Throwable => ""
    }
  }

  def angleDescription(al: AttributeList): String = {
    try {
      val g = al.get(TagByName.GantryAngle).getDoubleValues.head
      val c = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

      "G" + Util.angleRoundedTo90(g) + " C" + Util.angleRoundedTo90(c)
    } catch {
      case _: Throwable => ""
    }
  }

  def dicomViewBaseName(beamName: String, rtimage: AttributeList, rtplan: AttributeList): String = {
    (beamName.trim + "_" + jawDescription(rtimage, rtplan)).replaceAll("[^a-zA-Z0-9]", "_").replaceAll("__*", "_").replaceAll("_$", "").replaceAll("^_", "")
  }

  def dicomViewHtmlFile(al: AttributeList, beamName: String, outputDir: File, rtplanAl: AttributeList): File = {
    val htmlFile = dicomViewBaseName(beamName, al, rtplanAl) + ".html"
    val viewDir = new File(outputDir, "view")
    new File(viewDir, htmlFile)
  }

  def dicomViewHtmlFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val f1 = dicomViewHtmlFile(al, runReq.beamNameOfAl(al), extendedData.output.dir, runReq.rtplan)
    if (true) { // TODO rm
      val htmlFile = dicomViewBaseName(runReq.beamNameOfAl(al), al, runReq.rtplan) + ".html"
      val viewDir = new File(extendedData.output.dir, "view")
      val f2 = new File(viewDir, htmlFile)
      if (!f1.getAbsolutePath.equals(f2.getAbsolutePath))
        throw new RuntimeException("change to function is wrong! : \nnew: " + f1.getAbsolutePath + "\nold: " + f2.getAbsolutePath)
    }
    f1
  }

  def dicomViewImageHtmlFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val htmlFile = dicomViewBaseName(runReq.beamNameOfAl(al), al, runReq.rtplan) + "_image.html"
    val viewDir = new File(extendedData.output.dir, "view")
    new File(viewDir, htmlFile)
  }

  def dicomViewImageFile(al: AttributeList, extendedData: ExtendedData, runReq: RunReq): File = {
    val pngFile = dicomViewBaseName(runReq.beamNameOfAl(al), al, runReq.rtplan) + ".png"
    val viewDir = new File(extendedData.output.dir, "view")
    new File(viewDir, pngFile)
  }

  def dicomViewHref(al: AttributeList, beamName: String, outputDir: File, rtplanAl: AttributeList): String = {
    WebServer.urlOfResultsFile(dicomViewHtmlFile(al, beamName, outputDir, rtplanAl))
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

  /**
    * Create a list of points whose sum can be used to measure the center dose of an image.
    */
  def makeCenterDosePointList(attributeList: AttributeList, collimatorCenterOfRotation: Point2D.Double): Seq[Point] = {
    val translator = new IsoImagePlaneTranslator(attributeList)

    // inspect this many pixels outside the calculated radius to account for round off errors
    val pad = 2

    val mmX = collimatorCenterOfRotation.getX
    val mmY = collimatorCenterOfRotation.getY

    val radius_mm = Config.CenterDoseRadius_mm

    val loX = (translator.iso2PixCoordX(mmX - radius_mm) - pad).round.toInt
    val hiX = (translator.iso2PixCoordX(mmX + radius_mm) + pad).round.toInt
    val loY = (translator.iso2PixCoordY(mmY - radius_mm) - pad).round.toInt
    val hiY = (translator.iso2PixCoordY(mmY + radius_mm) + pad).round.toInt

    val xPixRange = loX until hiX
    val yPixRange = loY until hiY

    // step through pixels and see which are close enough.
    def nearCenter(xPix: Int, yPix: Int): Boolean = {
      collimatorCenterOfRotation.distance(translator.pix2Iso(xPix, yPix)) <= radius_mm
    }

    val pointList = for (xPix <- xPixRange; yPix <- yPixRange; if nearCenter(xPix, yPix)) yield {
      new Point(xPix, yPix)
    }
    pointList
  }

  /**
    * Convert a list of pixel values to dose.
    */
  def pixToDose(pixValueSeq: Seq[Double], attributeList: AttributeList): Seq[Double] = {
    val m = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val b = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    pixValueSeq.map(p => (p * m) + b)
  }

  def pixToDose(pixValue: Float, attributeList: AttributeList): Double = pixToDose(Seq(pixValue.toDouble), attributeList).head

  /**
    * Measure dose as specified by the list of points and return it in the proper units.
    */
  def measureDose(pointList: Seq[Point], dicomImage: DicomImage, attributeList: AttributeList): Double = {
    // average value raw pixel values
    val rawAverage = pointList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pointList.size
    val dose = Phase2Util.pixToDose(rawAverage, attributeList)
    dose
  }

  case class MaintenanceRecordBaseline(maintenanceRecord: Option[MaintenanceRecord], baseline: Baseline) {}

  /**
    * Look through the BeamSequence and find the ReferencedBeamSequence that matches the given beam.
    */
  def getBeamSequence(plan: AttributeList, beamNumber: Int): AttributeList = {
    // Determine if the given attribute list references the given beam number.
    def matchesBeam(beamNumber: Int, al: AttributeList): Boolean = al.get(TagByName.BeamNumber).getIntegerValues.head == beamNumber

    DicomUtil.seqToAttr(plan, TagByName.BeamSequence).find(bs => matchesBeam(beamNumber, bs)).get
  }

  /**
    * Return true if the collimator in the image is oriented horizontally, false for vertically.
    *
    * This is based on whether the rounded collimator angle is 0 or 180 for horizontal, 90 or 270 for vertical.
    *
    * Other collimator angles such as 45 degrees may give unexpected results.
    */
  def isHorizontal(image: AttributeList): Boolean = (Util.angleRoundedTo90(image.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head) % 180) == 0

  /**
    * First try getting the RTPLAN from the uploaded DICOM, then from the database.  If both fail, then return None.
    *
    * @param rtplanUid SOP Instance UID of RTPLAN
    * @param alList    List of DICOM files.
    * @return RTPLAN or None.
    */
  def fetchRtplan(rtplanUid: String, alList: Seq[AttributeList]): Option[AttributeList] = {

    def rtplanReferencedByUploadedRtimage(rtplanUid: String, alList: Seq[AttributeList]) = {
      val rtplanList = alList.filter(Util.isRtplan)
      rtplanList.find(al => Util.sopOfAl(al).equals(rtplanUid)) match {
        case Some(al) => Some(al)
        case _        => None
      }
    }

    def rtplanInDb(rtplanUid: String): Option[AttributeList] = {
      val rtplan = DicomSeries
        .getBySopInstanceUID(rtplanUid)
        .flatMap(_.attributeListList)
        .filter(al => Util.sopOfAl(al).equals(rtplanUid))
        .headOption
      rtplan
    }

    rtplanReferencedByUploadedRtimage(rtplanUid, alList) match {
      case Some(al) => Some(al)
      case _        => rtplanInDb(rtplanUid)
    }
  }

}
