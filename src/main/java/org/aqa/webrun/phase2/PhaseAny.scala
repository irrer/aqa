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
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.WedgePoint
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.aqa.webrun.focalSpot.FSAnalysis
import org.aqa.webrun.focalSpot.FSRunReq
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringAnalysis
import org.aqa.webrun.phase2.collimatorPosition.CollimatorPositionAnalysis
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.webrun.phase2.metadataCheck.MetadataCheckAnalysis
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis
import org.aqa.webrun.phase2.vmat.VMATAnalysis
import org.aqa.webrun.phase2.wedge.WedgeAnalysis
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem

/**
  * Run PhaseAny code.
  */
class PhaseAny(procedure: Procedure) extends WebRunProcedure with RunTrait[RunReq] {

  /** Defines precision - Format to use when showing numbers. */
  //private val outputFormat = "%7.5e"

  //private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)
  private val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", primary = true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("PhaseAny"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  /*
  private def emptyForm(valueMap: ValueMapT, response: Response): Unit = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }
   */

  private case class BasicData(rtplan: AttributeList, rtimageListByBeam: Seq[(Option[String], AttributeList)]) {

    private def dfToString(al: AttributeList) = {
      val sop = Util.sopOfAl(al)
      val modality = Util.modalityOfAl(al)
      sop + "    " + modality
    }

    private def beamOf(beamName: Option[String]) = if (beamName.isDefined) beamName.get else "NoBeamName"

    override def toString: String =
      "machine: " + dfToString(rtplan) + "\n" +
        "rtimageListByBeam:\n    " + rtimageListByBeam.map(r => beamOf(r._1) + "  " + dfToString(r._2)).mkString("\n    ")
  }

  /**
    * Check that there is a single plan, single machine, and some images.
    */
  private def basicValidation(rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Either[StyleMapT, BasicData] = {
    logger.info("Number of RTPLAN files uploaded: " + rtplanList.size)
    logger.info("Number of RTIMAGE files uploaded: " + rtimageList.size)
    val machineSerialNumberList = rtimageList.flatMap(rtimage => Util.getAttrValue(rtimage, TagFromName.DeviceSerialNumber))

    def rtimageDate(rtimage: AttributeList): Long = {
      Util.extractDateTimeAndPatientIdFromDicomAl(rtimage)._1.map(d => d.getTime).distinct.max
    }

    val dateTimeList = rtimageList.map(rtimage => rtimageDate(rtimage)).sorted
    val maxDuration = Math.round(Config.MaxProcedureDuration * 60 * 1000)

    // associate each image with a plan
    //val planGroups = rtplanList.map(plan => (plan, rtimageList.filter(img => Phase2Util.imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    val planUIDReferences = rtimageList.map(img => Phase2Util.referencedPlanUID(img)).distinct

    val dbSeriesList = planUIDReferences.flatMap(planUID => DicomSeries.getBySopInstanceUID(planUID))

    val dbList = dbSeriesList.map(ds => ds.attributeListList.head)

    // Look in the uploaded rtplan list and DicomSeries in the database for the plan(s) referenced.  If that fails, then try the shared directory.
    val referencedRtplanList: Seq[AttributeList] = {
      if (Config.ProductionMode || rtplanList.isEmpty) { // Either this is is ProductionMode, or is TestMode and the user did not upload a plan.
        val matchingUploaded = rtplanList.filter(plan => planUIDReferences.contains(Util.sopOfAl(plan)))
        val list = matchingUploaded ++ dbList
        if (list.nonEmpty)
          list
        else
          Seq() // nothing found
      } else {
        // this is TestMode, and the user uploaded a plan, so use the RTPLAN that the user uploaded
        rtplanList
      }
    }

    //noinspection SpellCheckingInspection
    // look in the RTPLANs referenced in the database, and if there is one that is for a
    // different procedure than what the user requested, flag an error.
    val dbRefWrongProcedure: Option[String] = {
      val wrong = dbSeriesList.find(s => s.procedurePK.isDefined && s.procedurePK.get != procedure.procedurePK.get)
      if (wrong.isDefined) {
        val wrongProc = Procedure.get(wrong.get.procedurePK.get).get
        Some("User requested to run " + procedure.fullName + " but the RTPLAN referenced is for " + wrongProc.fullName)
      } else
        None // no error
    }

    /**
      * Make a human readable list of machines
      */
    0 match {
      case _ if Config.ProductionMode && (planUIDReferences.size > 1) => formErr("The RTIMAGES reference more than one RTPLAN.")
      case _ if referencedRtplanList.isEmpty                          => formErr("Can not find the referenced RTPLAN.  Retry and upload the RTPLAN with the images. ")
      //noinspection SpellCheckingInspection
      case _ if rtimageList.isEmpty           => formErr("No RTIMAGEs given")
      case _ if dbRefWrongProcedure.isDefined => formErr(dbRefWrongProcedure.get)

      case _ if machineSerialNumberList.isEmpty =>
        //noinspection SpellCheckingInspection
        formErr(
          "None of the " + rtimageList.size +
            " RTIMAGEs have a device serial number (0018,1000) tag.\\n" +
            "This can happen on a new machine or one that has been recently serviced.\\n" +
            "The device serial number is required by this software to identify the instance of the machine."
        )

      case _ if machineSerialNumberList.size != rtimageList.size =>
        //noinspection SpellCheckingInspection
        formErr(
          "Only " + machineSerialNumberList.size + " of the " + rtimageList.size +
            " RTIMAGEs have a device serial number (0018,1000) tag.\\n" +
            "This can happen on a new machine or one that has been recently serviced.\\n" +
            "The device serial number is required by this software to identify the instance of the machine."
        )

      case _ if Config.ProductionMode && ((dateTimeList.last - dateTimeList.head) > maxDuration) =>
        formErr("Over " + Config.MaxProcedureDuration + " minutes from first to last image.  These RTIMAGE files were not from the same session")

      case _ =>
        val rtplan = referencedRtplanList.head
        val rtimageByBeam = rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(rtplan, rtimage), rtimage))
        Right(BasicData(rtplan, rtimageByBeam)) // TODO remove machine from class?
    }
  }

  /**
    * determine if there are undefined beams
    */
  private def beamNotDefinedProblem(basicData: BasicData): Option[String] = {
    // check for beams that are not in the plan
    val undefFileNameList = basicData.rtimageListByBeam.filter(b => b._1.isEmpty).map(b => b._2)
    if (undefFileNameList.nonEmpty) {
      val undefBeamNumbers = undefFileNameList.map(al => al.get(TagByName.ReferencedBeamNumber)).filter(a => a != null).flatMap(a => a.getIntegerValues)
      Some(
        "There were " + undefFileNameList.size + " file(s) that reference a beam that was not in the plan, or, had no beam number." + WebUtil.titleNewline +
          "  Beam number(s) in images but not defined in RTPLAN: " + undefBeamNumbers.mkString("  ") + WebUtil.titleNewline +
          "  Number of images that did not reference a beam number: " + (undefFileNameList.size - undefBeamNumbers.size)
      )
    } else None
  }

  /**
    * Make sure that the beams required for collimator centering have been uploaded.
    * @param basicData List of beams.
    * @return Error message if there is a problem, None if ok.
    */
  private def validateCollimatorCentering(basicData: BasicData): Option[String] = {
    val requiredBeamList = 0 match {
      case _ if procedure.isPhase2 => Config.collimatorCenteringPhase2List
      case _ if procedure.isPhase3 => Config.collimatorCenteringPhase3List
      case _                       => throw new RuntimeException("Unexpected error.  Invalid procedure for PhaseAny: " + procedure)
    }

    val allBeamNames = basicData.rtimageListByBeam.flatten(bd => bd._1)

    val sep = " | "
    val uploaded = requiredBeamList.map(_.trim).filter(allBeamNames.contains).sorted.mkString(sep)
    val expected = requiredBeamList.map(_.trim).sorted.mkString(sep)

    /**
      * Check to make sure that there is a collimator centering beam at collimator angle 90 and 270 for each of the gantry angles.
      * @param list List of all collimator centering beams.
      * @return None if everything is ok, or, a message indicating the problem.
      */
    def allAreOpposingPairs(list: Seq[AttributeList]): Option[String] = {
      def ga(img: AttributeList) = Util.angleRoundedTo90(Util.gantryAngle(img))
      def ca(img: AttributeList) = Util.angleRoundedTo90(Util.collimatorAngle(img))

      val gantryAngleGroupList = list.groupBy(ga)

      /**
        * Determine if there are two beams at this gantry angle and their collimator angles are 90 and 270.
        * @param imgList List of beams for one gantry angle.
        * @return None if ok, message if something is wrong.
        */
      def hasBoth(imgList: Seq[AttributeList]): Option[String] = {
        if ((imgList.size == 2) && imgList.exists(img => ca(img) == 90) && imgList.exists(img => ca(img) == 270))
          None
        else
          Some(s"Collimator centering beams for gantry angle ${ga(list.head)} have collimator angles of ${list.map(ca).sorted.mkString(", ")} but should be 90 and 270 degrees.")
      }

      val errorList = gantryAngleGroupList.values.flatMap(hasBoth)
      errorList.headOption
    }

    if (uploaded.equals(expected)) {
      val rtimageList = basicData.rtimageListByBeam.filter(rtimage => rtimage._1.isDefined && requiredBeamList.contains(rtimage._1.get)).map(_._2)
      allAreOpposingPairs(rtimageList)
    } else {
      val nl = WebUtil.titleNewline
      val text = {
        s"Did not find required collimator centering beams.$nl" +
          s"  Expected beams:$nl$expected$nl Uploaded beams:$uploaded$nl"
      }
      Some(text)
    }
  }

  /**
    * Get the acquisition date+time of the given RTIMAGE.
    * @param al This rtimage.
    * @return Date+time as ms.
    */
  private def dateOfRtimage(al: AttributeList): Long = {
    try {
      val date = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get
      date.getTime
    } catch {
      case _: Throwable => -1
    }
  }

  private def makeRtimageMap(basicData: BasicData): Map[String, AttributeList] = {
    // only beams with known names
    val list = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined)

    val groups = list.groupBy(rl => rl._1.get)

    def latestOf(rl: Seq[(Option[String], AttributeList)]) = {
      val latest = rl.maxBy(r => dateOfRtimage(r._2))
      (latest._1.get, latest._2)
    }

    val culled = groups.map(g => latestOf(g._2)).filterNot(rl => rl._1.equals(Config.FloodFieldBeamName))

    culled
  }

  /**
    * Check beam definitions, existence of flood field, and organize inputs into <code>RunReq</code> to facilitate further processing.
    */
  private def basicBeamValidation(basicData: BasicData): Either[StyleMapT, RunReq] = {
    val rtimageMap = makeRtimageMap(basicData)
    //val rtimageMap = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && (!rl._1.get.equals(Config.FloodFieldBeamName))).map(rl => (rl._1.get, rl._2)).toMap
    val flood = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && rl._1.get.equals(Config.FloodFieldBeamName)).sortBy(f => dateOfRtimage(f._2))
    val colCentering = validateCollimatorCentering(basicData)

    beamNotDefinedProblem(basicData) match {
      case Some(errorMessage)          => formErr(errorMessage)
      case _ if flood.isEmpty          => formErr("Flood field beam is missing")
      case _ if colCentering.isDefined => formErr(colCentering.get)
      case _                           => Right(RunReq(basicData.rtplan, rtimageMap, flood.last._2, Seq(), Seq())) // success
    }
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  private def validatePhaseAny(rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Either[StyleMapT, RunReq] = {
    basicValidation(rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]) match {
      case Left(fail) => Left(fail)
      case Right(basicData) =>
        logger.info("Received file list: " + basicData)
        val basicBeamValid = basicBeamValidation(basicData)
        if (basicBeamValid.isLeft) basicBeamValid
        else {
          val runReq = basicBeamValid.right.get
          val bpErr = BadPixelAnalysis.validate(runReq)
          if (bpErr.isDefined) Left(Error.make(form.uploadFileInput.get, bpErr.get)) else Right(runReq)
        }
    }
  }

  /**
    * If more than one RTIMAGE reference the same beam, then remove all but the one with the the latest date/time.
    * private def cullRedundantBeamReferences(rtimageList: Seq[DicomFile]): Seq[DicomFile] = {
    * val groupList = rtimageList.groupBy(df => df.attributeList.get.get(TagByName.ReferencedBeamNumber).getIntegerValues.head).values
    *
    * def latestDateTime(al: AttributeList): Long = {
    * Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.map(dt => dt.getTime).max
    * }
    *
    * def minGroup(g: Seq[DicomFile]): DicomFile = {
    * if (g.size == 1) g.head
    * else {
    * val sorted = g.map(df => (latestDateTime(df.attributeList.get), df)).sortBy(_._1)
    * val latestDicomFile = sorted.last._2
    * val text = sorted.dropRight(1).map(df => Util.sopOfAl(df._2.attributeList.get)).mkString("\n    ")
    * logger.info("Ignoring RTIMAGE files that redundantly reference the same beam.  Keeping the chronologically newest one:\n    " + text +
    * "\nKeeping " + Util.sopOfAl(latestDicomFile.attributeList.get))
    * latestDicomFile
    * }
    * }
    *
    * val culled = groupList.map(g => minGroup(g))
    * logger.info("Number of RTIMAGE files culled due to redundant beam references: " + (rtimageList.size - culled.size))
    * culled.toSeq
    * }
    */

  private def makeHtml(extendedData: ExtendedData, procedureStatus: ProcedureStatus.Value, elemList: Seq[Elem], runReq: RunReq): Unit = {

    val sizedGroups = edu.umro.ScalaUtil.Util.sizedGroups(elemList, 4)

    def showGroup(group: Seq[Elem]) = {
      <div class="row" style="margin-top:40px; margin-left:10px; margin-right:10px;">
        {
        group.map(e => <div class="col-md-3">
        {e}
      </div>)
      }
      </div>
    }

    def table = {
      <div class="col-md-10 col-md-offset-1">
        {sizedGroups.map(g => showGroup(g))}
      </div>
    }

    val procedureName = {
      0 match {
        case _ if extendedData.procedure.isPhase2 => "Phase 2"
        case _ if extendedData.procedure.isPhase3 => "Phase 3"
        case _                                    => extendedData.procedure.fullName
      }
    }
    val text = Phase2Util.wrapSubProcedure(extendedData, table, procedureName, procedureStatus, None, runReq.rtimageMap)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
  }

  /**
    * Run the sub-procedures.
    */
  private def runPhaseAny(extendedData: ExtendedData, runReq: RunReq): ProcedureStatus.Value = {
    logger.info("Starting PhaseAny analysis")

    val alList = runReq.rtimageMap.values.toSeq

    // Save DICOM series to the database if they are not already there.  It is possible that the RTIMAGE files are
    // from different series, so make sure to split them by series and store them separately.  This is important for
    // the automatic client that will ask this server if a given series has been processed or not.  This actually
    // happened once.
    val seriesList = alList.groupBy(al => Util.serInstOfAl(al)).values
    val procedurePK = procedure.procedurePK.get
    seriesList.foreach(series => DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, series, Some(procedurePK)))

    val summaryList: Either[Seq[Elem], Seq[Elem]] = MetadataCheckAnalysis.runProcedure(extendedData, runReq) match {
      case Left(fail) => Left(Seq(fail))
      case Right(metadataCheck) =>
        BadPixelAnalysis.runProcedure(extendedData, runReq) match {
          case Left(fail) => Left(Seq(metadataCheck.summary, fail))
          case Right(badPixel) =>
            CollimatorCenteringAnalysis.runProcedure(extendedData, runReq) match {
              case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, fail))
              case Right(collimatorCentering) =>
                val collimatorCenteringResource = new CollimatorCenteringResource(collimatorCentering.result, runReq)
                CenterDoseAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource) match {
                  case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, collimatorCentering.summary, fail))
                  case Right(centerDose) =>
                    val prevSummaryList = Seq(metadataCheck, badPixel, collimatorCentering, centerDose).map(r => r.summary)
                    val seq: Seq[() => Either[Elem, SubProcedureResult]] = {
                      val s = Seq(
                        () => CollimatorPositionAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource),
                        () => WedgeAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource),
                        () => SymmetryAndFlatnessAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource),
                        () => LeafPositionAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource),
                        () => VMATAnalysis.runProcedure(extendedData, runReq, collimatorCenteringResource)
                      )

                      if (extendedData.procedure.isPhase3) { // include Focal Spot for Phase3, but not Phase2
                        val fs = { () => FSAnalysis.runProcedure(extendedData, FSRunReq(runReq.rtplan, runReq.rtimageMap)) }
                        s :+ fs
                      } else s
                    }

                    val list = seq.par.map(f => f())
                    val summaryList = prevSummaryList ++ list.map(r => if (r.isLeft) r.left.get else r.right.get.summary)
                    val pass = !list.exists(r => r.isLeft) && list.forall(s => Phase2Util.statusOk(s.right.get.status))
                    if (pass) Right(summaryList) else Left(summaryList)
                }
            }
        }
    }
    logger.info("Finished all PhaseAny analysis")

    val status = summaryList match {
      case Left(_)  => ProcedureStatus.fail
      case Right(_) => ProcedureStatus.pass
    }

    val sumList = summaryList match {
      case Left(list)  => list
      case Right(list) => list
    }

    logger.info("Generating " + procedure.fullName + " HTML")
    makeHtml(extendedData, status, sumList, runReq)
    logger.info("Done generating " + procedure.fullName + " HTML")

    status
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = {
    //val result = validate(emptyValueMap, alList.filter(al => Util.isRtimage(al)))
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val rtplanList = alList.filter(al => Util.isRtplan(al))

    def getRtplan = {
      // If this is production mode, or, if it is test mode and there is no RTPLAN in the input list, then find the plan with the matching UID.
      if (Config.ProductionMode || rtplanList.isEmpty) {
        val rtplanUID = Phase2Util.referencedPlanUID(rtimageList.head)
        DicomSeries.getBySopInstanceUID(rtplanUID).headOption match {
          // get this from the database
          case Some(ds) => ds.attributeListList.head
          case _ =>
            logger.warn("Could not find RTPLAN in database.  Looking in file system.") // if it was not in the database, then check in the share directory.  This code should be deprecated soon.  TODO
            val file = new File(Config.sharedDir, rtplanUID + ".dcm")
            val df = new DicomFile(file)
            df.attributeList.get
        }
      } else {
        // TestMode
        rtplanList.head
      }
    }

    val rtplan = getRtplan
    val rtimageMap = rtimageList.map(al => (Phase2Util.getBeamNameOfRtimage(rtplan, al).get, al)).toMap
    val floodBeamName = rtimageMap.keys.find(_.toLowerCase.contains("flood")).get

    val symmetryAndFlatnessBaselineRedoBeamList = {
      if (oldOutput.isEmpty)
        Seq[String]()
      else
        SymmetryAndFlatness.getBaselineByOutput(oldOutput.get.outputPK.get).map(_.beamName) // get list of beams that were marked as baseline so the new results can be marked as baseline
    }

    val wedgeBaselineRedoBeamList: Seq[String] = {
      if (oldOutput.isEmpty)
        Seq()
      else {
        // List of wedge points that were baselines.
        val wereBaseline = WedgePoint.history(oldOutput.get.machinePK.get, oldOutput.get.procedurePK).filter(wh => wh.output.outputPK.get == oldOutput.get.outputPK.get && wh.wedgePoint.isBaseline)
        // Transform into beam name and background beam name, which is used to identify it.
        val list = wereBaseline.map(wh => wh.wedgePoint.wedgeBeamName + wh.wedgePoint.backgroundBeamName)
        list
      }
    }

    val runReq = RunReq(rtplan, rtimageMap, rtimageMap(floodBeamName), symmetryAndFlatnessBaselineRedoBeamList, wedgeBaselineRedoBeamList) // TODO add Wedge baseline list
    runReq
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    alList.filter(al => Util.isRtimage(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val min = alList.filter(Util.isRtimage).map(Util.extractDateTimeAndPatientIdFromDicomAl).flatMap(_._1.headOption).min
    Some(new Timestamp(min.getTime))
  }

  override def getProcedure: Procedure = procedure

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    val rtplanList = alList.filter(al => Util.isRtplan(al))
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val result = validatePhaseAny(rtplanList, rtimageList)
    result
  }

  override def run(extendedData: ExtendedData, runReq: RunReq, response: Response): ProcedureStatus.Value = {
    val status = runPhaseAny(extendedData, runReq)
    //.runProcedure(extendedData, runReq)
    status
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
