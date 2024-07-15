package org.aqa.webrun.seriesMaker

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.Procedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.ButtonType
import org.aqa.web.WebUtil.FormButton
import org.aqa.web.WebUtil.WebForm
import org.aqa.web.WebUtil.WebRow
import org.aqa.web.WebUtil.dicomFilesInSession
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Util
import org.aqa.web.WebUtil.WebPlainText
import org.aqa.AnonymizeUtil
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.web.WebUtil.WebInputHidden
import org.aqa.web.WebUtil.WebInputSelect
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object SeriesMaker extends Logging {
  private val path = new String((new SeriesMaker).pathOf)
}

class SeriesMaker extends Restlet with SubUrlRoot with Logging {

  private val pageTitle = "Series Maker"

  private val assignBeamsTag = "assignBeams"

  private val beamAssignmentListTag = "beamAssignmentList"

  private val elapsedFormat = new SimpleDateFormat("mm:ss")

  private val planColor = "#404040"

  private def colorList =
    Seq(
      "#39698C",
      "#008585",
      "#8a508f",
      "#bc5090",
      "#c7522a",
      "#74a892",
      "#ff6361",
      "#ff8531",
      "#ffa600"
    )

  /**
    * Provide convention for HTML ids.
    *
    * @param group Which group the item belongs to.
    * @param beam  Item index within group.
    */
  private case class LocalHtmlId(group: Int, beam: Int) {
    val beamId = s"RTIMAGE_${group}_$beam"
    val planId = s"RTPLAN_$group"
    val dragId = s"${beamId}_drag"
  }

  private class FormButtonProcedure(name: String, val procedure: Option[Procedure]) extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def nextButton = makeButton("Next", ButtonType.BtnDefault)

  private def resetButton = makeButton("Reset", ButtonType.BtnDefault)

  private def downloadButton = makeButton("Download", ButtonType.BtnDefault)

  private def runButton = makeButton("Run", ButtonType.BtnDefault)

  private def procedureSelection(procedurePK: Option[Long]): WebInputSelect = {
    //noinspection ScalaUnusedSymbol
    def list(response: Option[Response]): List[(String, String)] = {
      val list: Seq[Procedure] = {
        if (procedurePK.isDefined)
          Seq(Procedure.get(procedurePK.get).get) // If the procedure is known, then only show that one.
        else {
          Procedure.list // procedure is not known.  Let the user pick it.
        }
      }

      list.sortBy(_.fullName).map(p => (if (p.procedurePK.isDefined) p.procedurePK.get.toString else "0", p.fullName)).toList
    }

    val sel = new WebInputSelect("Procedure", false, 2, 0, list, false)
    sel
  }

  private val js = {
    s"""
      |
      |function allowDrop(ev) {
      |  ev.preventDefault();
      |}
      |
      |function drag(ev) {
      |  ev.dataTransfer.setData("text", ev.target.id);
      |}
      |
      |function drop(ev) {
      |  ev.preventDefault();
      |  var data = ev.dataTransfer.getData("text");
      |  ev.target.appendChild(document.getElementById(data));
      |
      |  // set the beam assignment list so that when queried, it shows which images are
      |  // assigned to which beams.
      |  var list = document.querySelectorAll('[id^="RTIMAGE"]');
      |  var beamAssignmentList = document.getElementById("$beamAssignmentListTag");
      |  beamAssignmentList.setAttribute("value", "");
      |  var valueText = "";
      |  var i = 0;
      |  for (i = 0; i < list.length; i++) {
      |    if (list[i].id.includes("drag")) {
      |      var drag = list[i];
      |      var parent = drag.parentElement;
      |      if (parent.id.includes("RTPLAN")) {
      |        valueText = valueText + " " + parent.id + ":" + drag.id;
      |      }
      |    }
      |  }
      |  beamAssignmentList.setAttribute("value", valueText);
      |}
      |
      |""".stripMargin
  }

  /**
    * Given a list of sorted RTIMAGES, put them into groups.  Files should be in the same group if they have the same series UID.
    */
  private def groupImages(list: Seq[AttributeList]): Seq[Seq[AttributeList]] = {

    def timeOf(rtimage: AttributeList) = SeriesMakerReq.rtimageTimeDate(rtimage).getTime

    val groupList = {
      val gl = list.groupBy(_.get(TagByName.SeriesInstanceUID).getSingleStringValueOrEmptyString).values
      gl.map(_.sortBy(timeOf)).toSeq.sortBy(g => timeOf(g.head))
    }

    groupList
  }

  private def getMachineName(rtimage: AttributeList, valueMap: ValueMapT): String = {

    val noMachine = "Machine Not Available"

    if (
      (rtimage.get(TagByName.RadiationMachineName) != null) &&
      rtimage.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString.nonEmpty
    ) {
      val anonAttr = rtimage.get(TagByName.RadiationMachineName)
      val user = WebUtil.getUser(valueMap).get

      val j = AnonymizeUtil.deAnonymizeAttribute(user.institutionPK, anonAttr) // TODO rm
      Trace.trace(j)
      if (j.nonEmpty) {
        val j1 = j.get.getSingleStringValueOrNull()
        Trace.trace(j1)
        Trace.trace()
      }
      Trace.trace()

      AnonymizeUtil.deAnonymizeAttribute(user.institutionPK, anonAttr) match {
        case Some(attr) =>
          val m = attr.getSingleStringValueOrEmptyString.trim
          if (m.isEmpty)
            noMachine
          else
            m
        case _ =>
          noMachine
      }
    } else
      noMachine
  }

  private def formatDraggable(color: String, id: LocalHtmlId, content: Option[Elem]): Elem = {
    val borderRadius = "border-radius:10px"
    val width = "width:280px"
    val height = "height:38px"
    val border = s"border: 2px solid $color"

    val inner: Seq[Elem] = {
      if (content.isDefined)
        Seq(<span id={id.dragId} draggable="true" ondragstart="drag(event)">{content.get}</span>)
      else
        Seq()
    }

    val outerId = if (content.isDefined) id.beamId else id.planId

    <div id={outerId} style={s"align:auto;$borderRadius;$border;$width;$height;"} ondrop="drop(event)" ondragover="allowDrop(event)">{inner}</div>
  }

  private def formatRtimage(color: String, id: LocalHtmlId, elapsed_ms: Long, beamName: String): Elem = {
    val timeText = Util.formatDate(elapsedFormat, new Date(elapsed_ms))
    val content = {
      <span>
        <span class="badge badge-secondary" style={s"background: $color;margin:2px;"}>
          <table>
            <tr>
              <td>
                <span style="vertical-align:middle;font-size:2.0em;margin-right:12px;">{id.beam}</span>
              </td>
              <td>
                <span style="vertical-align:middle;font-size:1.0em;">{timeText}</span>
              </td>
              <td>
                <span style="vertical-align:middle;font-size:1.5em;margin-left:12px;margin-right:5px;">{beamName}</span>
              </td>
            </tr>
          </table>
        </span>
        <b> {""} </b>
      </span>
    }
    formatDraggable(color, id, Some(content))
  }

  private def rtimageToHtml(color: String, id: LocalHtmlId, first: Date, al: AttributeList, req: SeriesMakerReq): Elem = {

    val beamName: String = {
      val attr = al.get(TagByName.ReferencedBeamNumber)

      0 match {
        case _ if attr == null =>
          "Beam NA"
        case _ if Phase2Util.getBeamNameOfRtimage(req.rtplan, al).isDefined => // get beam name from plan
          Phase2Util.getBeamNameOfRtimage(req.rtplan, al).get
        case _ => // have beam number, but no plan
          attr.getIntegerValues.head.toString
      }
    }

    val elapsed_ms = SeriesMakerReq.rtimageTimeDate(al).getTime - first.getTime

    <tr>
      <td>
        {formatRtimage(color, id, elapsed_ms, beamName)}
      </td>
    </tr>
  }

  private def rtimageGroupToHtml(valueMap: ValueMapT, group: Seq[AttributeList], groupIndex: Int, color: String, req: SeriesMakerReq): Elem = {

    val firstFormat = new SimpleDateFormat("YYYY EEE MMM d HH:mm")

    val first = SeriesMakerReq.rtimageTimeDate(group.head)

    val machineName = getMachineName(group.head, valueMap)

    val headerText = Seq(machineName, Util.formatDate(firstFormat, first)).mkString(" :: ")

    val content = {
      <tr>
        <td>
          <table class="table table-bordered" foo="bar">
            <thead>
              <tr>
                <th style="text-align:center;">
                  {headerText}
                </th>
              </tr>
            </thead>{group.indices.map(i => rtimageToHtml(color, LocalHtmlId(groupIndex, i), first, group(i), req))}
          </table>
        </td>
      </tr>
    }
    content
  }

  /**
    * Create HTML that displays the list of RTIMAGES.
    * @param valueMap HTML parameters.
    * @return Displayable list of RTIMAGES.
    */
  private def rtimageListToHtml(valueMap: ValueMapT, req: SeriesMakerReq): WebUtil.WebPlainText = {

    val groupList = groupImages(req.rtimageList)

    // <th style="display:block; margin-left:auto;margin-right:auto;width:150px;">RTIMAGE List</th>
    val content = {
      <div>
        <table style="width: 100%;" class="table table-bordered">
          <thead>
            <tr>
              <th style="text-align:center;">RTIMAGE List</th>
            </tr>
          </thead>
          {groupList.indices.map(groupIndex => rtimageGroupToHtml(valueMap, groupList(groupIndex), groupIndex, colorList(groupIndex % colorList.size), req))}
        </table>
      </div>
    }
    Trace.trace("content: " + content)
    new WebPlainText(label = "Beams", showLabel = false, col = 3, offset = 1, _ => content)
  }

  /**
    * Show the RTPLAN HTML if the plan can not be ascertained.
    * @param req Requirements.
    * @return Plan display.
    */
  private def rtplanToHtml(req: SeriesMakerReq): WebUtil.WebPlainText = {

    def toHtml(rtimage: AttributeList): Elem = {

      val beamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
      val beamName = Phase2Util.getBeamNameOfRtimage(req.rtplan, rtimage).get

      val id = LocalHtmlId(beamNumber, -1)

      <tr>
        <td>
          <table>
            <tr>
              <td>
                {formatDraggable(planColor, id, None)}
              </td>
              <td>
                <span style="margin-left:20px; width:200px;"><b>{beamName}</b></span>
              </td>
            </tr>
          </table>
        </td>
      </tr>
    }

    val content = {
      <table style="width: 100%;" class="table table-bordered">
        <thead>
          <tr>
            <th style="text-align:center;">Plan Beams</th>
          </tr>
        </thead>
        {req.templateList.map(toHtml)}
      </table>
    }

    new WebPlainText(label = "Plan", showLabel = false, col = 4, offset = 1, _ => content)

  }

  private def makeUploadForm(valueMap: ValueMapT): WebForm = {

    val dicomList = dicomFilesInSession(valueMap).flatMap(_.attributeList)

    val buttonRow = List(nextButton, cancelButton)

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(buttonRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = 10, runScript = None)

    form
  }

  private def makeAssignForm(valueMap: ValueMapT, seriesMakerReq: SeriesMakerReq): WebForm = {

    val procedurePK: Option[Long] = {
      val rtplanSop = Util.sopOfAl(seriesMakerReq.rtplan)
      val pk = DicomSeries.getBySopInstanceUID(rtplanSop).flatMap(_.procedurePK).headOption
      pk
    }

    val alRow = List(rtimageListToHtml(valueMap, seriesMakerReq), rtplanToHtml(seriesMakerReq))

    val selectRow = List(procedureSelection(procedurePK))

    val buttonRow = List(resetButton, downloadButton, runButton, cancelButton)

    val beamAssignmentList = new WebInputHidden(beamAssignmentListTag)

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(alRow, List(beamAssignmentList), selectRow, buttonRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = -1, runScript = Some(js))

    form
  }

  /**
    * Make a list of the RTIMAGE files that the user uploaded.
    * @param alList All DICOM files.
    * @return RTIMAGE files.
    */
  private def rtimageList(alList: Seq[AttributeList]): Seq[AttributeList] = SeriesMakerReq.extractDistinctRtimageList(alList)

  private def makeAssignBeamsForm(valueMap: ValueMapT, seriesMakerReq: SeriesMakerReq, response: Response): Unit = {
    ???
  }

  /**
    * Respond when the user clicks the 'Next' button.  Validate files.  If good, go to assign beams page.  If bad, show error message.
    * @param valueMap Web parameters.
    * @param alList List of DICOM files.
    * @param response HTTP response.
    */
  private def processNext(valueMap: ValueMapT, alList: Seq[AttributeList], response: Response): Unit = {

    val seriesMakerReq = SeriesMakerReq.makeRequirements(alList)

    if (seriesMakerReq.isLeft) {
      val errMsg = seriesMakerReq.left.get
      val form = makeUploadForm(valueMap: ValueMapT)
      val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
      form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      val newValueMap = valueMap + (assignBeamsTag -> "true")
      val form = makeAssignForm(newValueMap, seriesMakerReq.right.get)
      form.setFormResponse(valueMap, WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)
      Trace.trace("TODO") // TODO
    }
  }

  private def getAssignedBeams(valueMap: ValueMapT): Seq[(Int, LocalHtmlId)] = {

    def makePair(text: String): (Int, LocalHtmlId) = {
      val list = text.split(":").filter(_.nonEmpty)
      def intOf(t: String): Seq[Int] = t.replaceAll("[^0-9]", " ").split(" ").filter(_.nonEmpty).map(_.toInt)
      val plan = intOf(list.head).head
      val group = intOf(list(1)).head
      val imageIndex = intOf(list(1))(1)

      (plan, LocalHtmlId(group, imageIndex))
    }

    val list: Seq[(Int, LocalHtmlId)] = valueMap.get(beamAssignmentListTag) match {
      case Some(text) =>
        Trace.trace(text)
        Trace.trace(text.split(" "))
        text.split(" ").filter(_.nonEmpty) map makePair
      case _ => Seq()
    }

    list
  }

  private def attrText(attr: Attribute): Option[String] = {
    if ((attr != null) && (attr.getSingleStringValueOrNull != null) && (attr.getSingleStringValueOrEmptyString.nonEmpty))
      Some(attr.getSingleStringValueOrEmptyString)
    else
      None
  }

  private def attrIn(alList: Seq[AttributeList], tag: AttributeTag): Option[String] = {
    alList.flatMap(al => DicomUtil.findAllSingle(al, tag)).flatMap(attrText).headOption
  }

  private def getDeviceSerialNumber(valueMap: ValueMapT, req: SeriesMakerReq, RadiationMachineName: String): String = {
    val dsn = attrIn(req.rtimageList, TagByName.DeviceSerialNumber) match {
      case Some(text) => text
      case _ =>
        val institutionPK = WebUtil.getUser(valueMap).get.institutionPK

        val RadiationMachineNameReal = {
          val attr = AttributeFactory.newAttribute(TagByName.RadiationMachineName)
          attr.addValue(RadiationMachineName)
          val text = AnonymizeUtil.deAnonymizeAttribute(institutionPK, attr).get.getSingleStringValueOrEmptyString
          text
        }

        val machine = Machine.getForInstitution(institutionPK).find(m => m.tpsID_real.isDefined && m.tpsID_real.get.equals(RadiationMachineNameReal))
        val realDsn = machine.get.getRealDeviceSerialNumber.get
        val al = new AttributeList
        val machineAttr = AttributeFactory.newAttribute(TagByName.DeviceSerialNumber)
        machineAttr.addValue(realDsn)
        al.put(machineAttr)
        AnonymizeUtil.anonymizeDicom(institutionPK, al)
        val dsn = al.get(TagByName.DeviceSerialNumber).getSingleStringValueOrEmptyString()
        dsn
    }
    dsn
  }

  private def assembleSeries(valueMap: ValueMapT, beamAssignmentList: Seq[(Int, LocalHtmlId)], req: SeriesMakerReq): SeriesCache.Entry = {

    val groupList = groupImages(req.rtimageList)

    def putBeam(beamNumber: Int, beamRef: LocalHtmlId): ConvertDicom.BeamConversion = {
      val rtimage = groupList(beamRef.group)(beamRef.beam)
      val template = req.templateList.find(_.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == beamNumber).get
      ConvertDicom.BeamConversion(rtimage, template)
    }

    val list = beamAssignmentList.map(ri => putBeam(ri._1, ri._2))

    def findFirst(tag: AttributeTag): Option[String] = {
      val text = Seq(attrIn(req.rtimageList, tag), attrIn(Seq(req.rtplan), tag)).flatten.headOption
      text
    }

    val PatientName = findFirst(TagByName.PatientName).get
    val PatientID = findFirst(TagByName.PatientID).get

    val RadiationMachineName = Seq(findFirst(TagByName.RadiationMachineName), findFirst(TagByName.TreatmentMachineName)).filter(_.nonEmpty).head.get

    val InstitutionName: String = Seq(attrIn(Seq(req.rtplan), TagByName.InstitutionName), attrIn(Seq(req.rtplan), TagByName.InstitutionName)).flatten.head

    val DeviceSerialNumber = getDeviceSerialNumber(valueMap, req, RadiationMachineName)

    val alList: Seq[AttributeList] =
      ConvertDicom.processSeries(
        list,
        RadiationMachineName = RadiationMachineName,
        DeviceSerialNumber = DeviceSerialNumber,
        PatientName = PatientName,
        PatientID = PatientID,
        InstitutionName = InstitutionName,
        req.rtplan
      )

    val entry = SeriesCache.put(session = valueMap(WebUtil.sessionLabel), beamAssignmentList = valueMap(beamAssignmentListTag), alList, req.rtplan)

    entry
  }

  private def processDownload(valueMap: ValueMapT, alList: Seq[AttributeList], response: Response): Unit = {
    val seriesMakerReq = SeriesMakerReq.makeRequirements(alList)

    val assignBeams = getAssignedBeams(valueMap)
    Trace.trace(assignBeams.mkString("\n"))

    if (assignBeams.isEmpty) {
      val errMsg = "No beams have been assigned to the plan."
      val form = makeAssignForm(valueMap, seriesMakerReq.right.get)
      val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
      form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      /*
      val newValueMap = valueMap + (assignBeamsTag -> "true")
      val form = makeAssignForm(newValueMap, seriesMakerReq.right.get)
      form.setFormResponse(valueMap, WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)
       */
      val entry = assembleSeries(valueMap, assignBeams, seriesMakerReq.right.get)

      val entity = new ByteArrayRepresentation(entry.content, MediaType.APPLICATION_ZIP)
      response.setEntity(entity)
      response.setStatus(Status.SUCCESS_OK)

      val fileName = "AQASeries" + Util.timeAsFileName(new Date(entry.time))
      WebUtil.setDownloadName(response, fileName)
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    def buttonIs(button: FormButton): Boolean = {
      val value = valueMap.get(button.label)
      value.isDefined && value.get.equals(button.label)
    }

    try {
      val user = CachedUser.get(request)
      val alList = dicomFilesInSession(valueMap).filter(_.attributeList.isDefined).map(_.attributeList.get)

      0 match {
        // case _ if user.isEmpty                                                                                   => updateMach()

        case _ if buttonIs(cancelButton) =>
          response.redirectSeeOther("/")

        case _ if buttonIs(nextButton) || buttonIs(resetButton) =>
          processNext(valueMap, alList, response)

        case _ if buttonIs(downloadButton) || buttonIs(resetButton) =>
          processDownload(valueMap, alList, response)

        case _ =>
          val form = makeUploadForm(valueMap: ValueMapT)
          form.setFormResponse(valueMap, errorMap = WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)

      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

    /*
     */
  }
}
