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

package org.aqa.simpleRtPlan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.db.Machine
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.text.SimpleDateFormat
import java.util.Date

/**
  * Generate a simple DICOM RTPLAN file customized for the user's environment.
  */
class SimpleRtPlanInterface extends Restlet with SubUrlAdmin with Logging {

  private val pageTitleSelect = "Simple RTPlan"

  private def planName = new WebInputText("Plan Name", true, 2, 0, "Name to distinguish this plan from others", false)

  private def machineName() = {
    def makeSelectList(response: Option[Response]): List[(String, String)] = {
      val user = getUser(response.get.getRequest).get
      val machineList = Machine.listMachinesFromInstitution(user.institutionPK).filter(_.tpsID_real.isDefined)
      val selectList = machineList.map(m => (m.getRealTpsId.get.trim, m.getRealId.trim)).toList
      selectList
    }
    new WebInputSelect(label = "Machine", showLabel = true, col = 2, offset = 0, selectList = makeSelectList, aqaAlias = false)
  }

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private lazy val templateFiles = {
    new TemplateFiles
  }

  private lazy val beamInterfaceList = BeamInterfaceList(templateFiles)

  /** Inserts vertical spacing above beam list. */
  private def header = {
    val imgElem = {
      <a rel="/static/images/CoordinateDiagram.png" class="screenshot" href="/static/images/CoordinateDiagram.png" title="">
        <img src="/static/images/CoordinateDiagram.png" height="65"/>
      </a>
    }
    new WebPlainText(label = "header", showLabel = false, col = 5, offset = 0, html = _ => <center style="margin-top:50px;"><b><h4>Beam Parameters {imgElem}</h4></b></center>)
  }

  private var idCount = 0
  private def uniqueId: String =
    idCount.synchronized {
      idCount = idCount + 1
      "id" + idCount
    }

  private def makePlainText(text: String, title: Option[String] = None, col: Int = 1): WebPlainText = {
    val elem =
      if (title.isDefined)
        <center title={title.get}><b>{text}</b></center>
      else
        <center><b>{text}</b></center>
    new WebPlainText(label = uniqueId, showLabel = false, col = col, offset = 0, html = _ => elem)
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
    * List of beam parameters.
    *
    * @return Parameters for all beams.
    */

  def beamColSeqX: Seq[BeamInterface] = {
    val rtplan = new DicomFile(templateFiles.ofModality("RTPLAN").head.file).attributeList.get
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamAlList.map(beamAl => BeamInterface(rtplan, beamAl))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def row1: WebRow = List(patientID, patientName)
  private def row2: WebRow = List(planName, machineName())
  private def row3: WebRow = List(header)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create RTPlan", ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(makePlainText(" "), createButton, makePlainText(" "), cancelButton)

  private def makeWebForm() = {
    val runScript = SimpleRtplanJS.runScript(beamInterfaceList.beamList.head)
    val rowList = List(row1, row2, row3) ++ beamInterfaceList.makeWebRows() ++ List(assignButtonList)
    new WebForm(action = pathOf, title = Some("Simple Emergency RTPLAN"), rowList = rowList, fileUpload = 0, runScript = Some(runScript))
  }

  private def formSelect(valueMap: ValueMapT, response: Response): Unit = {
    val form = makeWebForm()
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val uniqueText = new SimpleDateFormat("yyyy-MM-dd").format(new Date)

    val defaultPatient = "$Simple_" + uniqueText

    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val planNameMap = if (empty(planName.label)) Map((planName.label, "Simple" + uniqueText.replace('-', ' '))) else emptyValueMap

    val valMap = valueMap ++ beamInterfaceList.beamInitialValueMap ++ patientIdMap ++ patientNameMap ++ planNameMap
    form.setFormResponse(valMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def validateEntryFields(valueMap: ValueMapT): StyleMapT = {
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty
    val planNameErr = if (empty(planName.label)) Error.make(planName, "A plan name must be given.") else styleNone
    val machErr = if (empty(machineName().label)) Error.make(machineName(), "A  must be given.") else styleNone
    val patIdErr = if (empty(patientID.label)) Error.make(patientID, "A patient machine nameID must be given.") else styleNone
    val patNameErr = if (empty(patientName.label)) Error.make(patientName, "A patient name must be given.") else styleNone
    val patIdTooLongErr = if (valueMap(patientID.label).length > 64) Error.make(patientID, "Patient ID can not be over 64 characters..") else styleNone
    val planNameTooLongErr = if (valueMap(planName.label).length > 16) Error.make(planName, "Plan Name can not be over 16 characters..") else styleNone
    // removed this check because it might stop people from generating a plan.  Technically the DICOM spec says that it
    // must be 16 characters or shorter, but the reality is that a lot of systems handle longer strings.
    //val machTooLongErr = if (valueMap(machineName.label).size > 16) Error.make(patientID, "Machine Name can not be over 16 characters..") else styleNone

    planNameErr ++ machErr ++ patIdErr ++ patNameErr ++ patIdTooLongErr ++ planNameTooLongErr // ++ machTooLongErr
  }

  /**
    * Make sure that the configuration is set up.  If broken, the user can not fix it, but does know what
    * to request of the system administrator.
    *
    * @return Error on failure, empty list on success.
    */
  private def validateConfigAndFiles() = {
    val templateDir = Config.SimpleRtplanTemplateDir
    def rtplanFile = Util.listDirFiles(templateDir.get).find(f => f.getName.toLowerCase().contains("rtplan"))

    if (Config.SimpleRtplanTemplateDir.isDefined && rtplanFile.isDefined && rtplanFile.get.canRead)
      styleNone
    else
      Error.make(
        planName.label,
        "RTPLAN file used as template has not been set up in the configuration.  Have the system administrator configure SimpleRtplanTemplateDir."
      )
  }

  /**
    * Make sure configuration and entered fields are valid.
    *
    * @param valueMap Values entered by user.
    * @return List of errors.  List is empty if everything is ok.
    */
  private def validate(valueMap: ValueMapT): StyleMapT = {
    val errorList = validateConfigAndFiles ++ validateEntryFields(valueMap) ++ beamInterfaceList.validateBeamFields(valueMap)
    errorList
  }

  private val downloadList = scala.collection.mutable.Map[String, ModifiedPlan]()

  private def downloadZip(valueMap: ValueMapT, response: Response): Unit = {
    val zip = {
      val uid = valueMap("download")
      val modifiedPlan = downloadList(uid)
      val data = modifiedPlan.zippedContent
      data
    }

    val entity = new ByteArrayRepresentation(zip, MediaType.APPLICATION_GNU_ZIP)
    response.setEntity(entity)
  }

  private def showDownload(modifiedPlan: ModifiedPlan, valueMap: ValueMapT, response: Response): Unit = {
    Trace.trace(valueMap)
    val downloadUrl = {
      val name = FileUtil.replaceInvalidFileNameCharacters(valueMap(patientID.label), '_').replace(' ', '_') + ".zip"
      val url = pathOf + "/" + name + "?download=" + modifiedPlan.rtplanUID
      Trace.trace(url)
      url
    }

    val downloadLink = new WebPlainText("Download", false, 3, 0, _ => { <h3> <a href={downloadUrl} title="Click to download zipped DICOM RTPLAN and supporting files.">Download</a></h3> })

    val dicomViewHtml = {
      val elem = {
        <div>
          <pre title="DICOM meta-data">{WebUtil.nl + modifiedPlan.rtplanText}</pre>
        </div>
      }
      elem
    }

    val dicomView = new WebPlainText("Download", false, 10, 0, _ => dicomViewHtml)

    def summary = {
      // val elem = { <pre>{WebUtil.nl + valueMapToString(valueMap).replaceAll("\n", WebUtil.nl)}</pre> }
      val elem = <div style="margin-right=200px; margin-left=200px;">
        <table class="table table-bordered">
          <tr>
            <td> <b>Patient ID: </b>{valueMap(patientID.label)} </td>
            <td> <b>Machine: </b>{valueMap(machineName().label)} </td>
          </tr>
          <tr>
            <td> <b>Patient Name: </b>{valueMap(patientName.label)} </td>
            <td> <b>Plan Name: </b>{valueMap(planName.label)} </td>
          </tr>
        </table>
        {beamInterfaceList.makeReviewTable(valueMap)}
      </div>
      new WebPlainText("Summary", false, 10, 0, _ => elem)
    }

    val rowA: WebRow = List(downloadLink)
    val rowB: WebRow = List(new WebPlainText("SummaryHeader", false, 10, 0, _ => { <h4>RTPlan Summary</h4> }))
    val rowC: WebRow = List(summary)
    val rowD: WebRow = List(new WebPlainText("DetailHeader", false, 10, 0, _ => { <h4>RTPlan as Text</h4> }))
    val rowE: WebRow = List(dicomView)

    val form = new WebForm(pathOf, List(rowA, rowB, rowC, rowD, rowE))
    form.setFormResponse(valueMap, styleNone, "Download Simple RTPLAN", response, Status.SUCCESS_OK)
  }

  private case class BeamReference(beam: AttributeList, fractionReference: AttributeList) {}

  private def makeCsv(valueMap: ValueMapT, NominalBeamEnergy: Double): String = {
    val rows = Seq(
      Seq("Patient ID:" + Util.textToCsv(valueMap(patientID.label))),
      Seq("Patient Name:" + Util.textToCsv(valueMap(patientName.label))),
      Seq("Machine:" + Util.textToCsv(valueMap(machineName().label))),
      Seq("Plan Name:" + Util.textToCsv(valueMap(planName.label)))
    )

    val text = rows.map(r => r.mkString(",")).mkString("\n") + beamInterfaceList.makeCsvSummary(valueMap, NominalBeamEnergy)
    text
  }

  private def createRtplan(valueMap: ValueMapT): ModifiedPlan = {

    val beamSpecificationList = beamInterfaceList.beamList.filter(_.isTreat).map(b => b.toBeamSpecification(valueMap))

    val NominalBeamEnergy = {
      val treatBeams = beamInterfaceList.beamList.filter(_.isTreat)
      val energyCol = treatBeams.head.colList.find(c => c.name.equals(treatBeams.head.labelEnergy)).get

      // the energy that the user specified
      val energy = valueMap(energyCol.label).trim.toDouble
      energy
    }

    val selectedMachineName = valueMap(machineName().label)
    val machineMap = {
      val machineList = Machine.listMachinesFromInstitution(getUser(valueMap).get.institutionPK).filter(_.tpsID_real.isDefined)
      val machineMap = machineList.map(m => (m.getRealTpsId.get, m)).toMap
      machineMap
    }

    val machine = machineMap(selectedMachineName)

    val makeRtPlan = new MakeRtPlan(
      PatientID = valueMap(patientID.label),
      PatientName = valueMap(patientName.label),
      machine = machine,
      RTPlanLabel = valueMap(planName.label),
      beamSpecificationList
    )

    val csvText = makeCsv(valueMap, NominalBeamEnergy)

    val modifiedPlan = makeRtPlan.makeZipWithSupportingFiles(csvText)
    downloadList.synchronized { downloadList.put(modifiedPlan.rtplanUID, modifiedPlan) }
    modifiedPlan
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  /**
    * Show the user a message saying why the creation of a custom rtplan failed.
    */
  private def showFailedCustomize(valueMap: ValueMapT, styleMap: StyleMapT, response: Response): Unit = {
    makeWebForm().setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def valueMapToString(valueMap: ValueMapT): String = {
    val text =
      "Patient ID: " + valueMap(patientID.label) + "\n" +
        "Patient Name: " + valueMap(patientName.label) + "\n" +
        "Plan Name: " + valueMap(planName.label) + "\n" +
        "Machine: " + valueMap(machineName().label) + "\n"
    text
  }

  private def validateAndMakePlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap: StyleMapT = validate(valueMap)
    if (styleMap.isEmpty) {
      logger.info("Creating plan.  Parameters: \n" + valueMapToString(valueMap))
      val modifiedPlan = createRtplan(valueMap)
      showDownload(modifiedPlan, valueMap, response)
    } else {
      showFailedCustomize(valueMap, styleMap, response)
    }
  }

  /**
    * Quit working on this.
    *
    * @param response Response to user.
    */
  private def quit(response: Response): Unit = response.redirectSeeOther("/static/admin.html")

  private def notConfigured(response: Response): Unit = {
    val elem =
      <div>
        The Emergency Simple RTPLAN generator has not been configured on this system.<p></p>
        The <b>SimpleRtplanTemplateDir</b> parameter needs to be set up in the configuration file and DICOM template files installed.
        <p></p>
        <p></p>
        <h4><a href="/">Return to Home Page</a></h4>
      </div>
    WebUtil.simpleWebPage(elem, "Not Configured", response)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = beamInterfaceList.beamInitialValueMap ++ getValueMap(request)

    try {
      val user = CachedUser.get(request)

      0 match {
        case _ if (beamInterfaceList.beamList.isEmpty) => notConfigured(response)
        case _ if user.isEmpty                         => quit(response)
        case _ if buttonIs(valueMap, cancelButton)     => quit(response)
        case _ if valueMap.contains("download")        => downloadZip(valueMap, response)
        case _ if buttonIs(valueMap, createButton)     => validateAndMakePlan(valueMap, response)
        case _                                         => formSelect(valueMap, response) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
