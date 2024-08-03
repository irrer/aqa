package org.aqa.webrun.seriesMaker

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomInputStream
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.RawByte
import org.aqa.Logging
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
import org.aqa.web.WebUtil.WebInputHidden
import org.aqa.web.WebUtil.WebInputSelect
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.DicomFile
import org.aqa.db.Machine
import org.aqa.Config
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.Session
import org.aqa.webrun.WebRun
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.ByteArrayInputStream
import java.io.File
import java.io.InputStream
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
      "#c7522a",
      "#8a508f",
      "#008585",
      "#bc5090",
      "#ff6361",
      "#74a892",
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

  // private class FormButtonProcedure(name: String, val procedure: Option[Procedure]) extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def nextButton = makeButton("Next", ButtonType.BtnDefault)

  private def resetButton = makeButton("Reset", ButtonType.BtnDefault)

  private def downloadButton = makeButton("Download", ButtonType.BtnDefault)

  private def runButton = makeButton("Run", ButtonType.BtnDefault)

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
  private def groupImages(list: Seq[DicomFile]): Seq[Seq[DicomFile]] = {

    def timeOf(rtimage: AttributeList) = SeriesMakerReq.getContentDateTime(rtimage).getTime

    val groupList = {
      val gl = list.groupBy(_.al.get(TagByName.SeriesInstanceUID).getSingleStringValueOrEmptyString).values
      gl.map(g => g.sortBy(df => timeOf(df.al))).toSeq.sortBy(g => timeOf(g.head.al))
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
    val width = "width:460px"
    val height = "height:47px"
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

  /**
    * Make a reference to the image for the given DICOM file.
    * @param sessionDirName Name of session directory
    * @param pngFileName    Name of png file.
    * @return HTML snippet.
    */
  private def imageRef(sessionDirName: String, pngFileName: String): Elem = {

    val imgSrc = s"/${Config.tmpDirName}/$sessionDirName/$pngFileName"

    <a rel={imgSrc} class="screenshot" href={imgSrc}>
      <img src={imgSrc} style="height:32px;"/>
    </a>
  }

  private def imageRef(dicomFile: DicomFile): Elem = {
    imageRef(dicomFile.file.getParentFile.getName, Util.sopOfAl(dicomFile.al) + ".png")
  }

  private def formatRtimage(color: String, id: LocalHtmlId, elapsed_ms: Long, beamName: String, dicomFile: DicomFile): Elem = {

    val fileName = dicomFile.file.getName

    val abbrevFileName: String = {
      val abbrevLen = 18
      val f1 = if (fileName.toLowerCase.endsWith(".dcm")) fileName.dropRight(4) else fileName
      val f2 = if (f1.length > abbrevLen) "..." + f1.takeRight(abbrevLen - 3) else f1
      f2
    }

    val timeText = Util.formatDate(elapsedFormat, new Date(elapsed_ms))

    val content = {
      <span>
        <span class="badge badge-secondary" style={s"background: $color;margin:2px;"}>
          <table>
            <tr style="vertical-align:middle;">
              <td>
                <span style="font-size:2.0em;">{id.beam + 1}</span>
              </td>
              <td style="width:1000px;text-align:left;">
                <span style="font-size:1.5em;margin-left:8px;">{beamName}</span>
              </td>
              <td>
                <span style="font-size:1.0em;margin-left:8px;">{timeText}</span>
              </td>
              <td>
                <span style="font-size:1.0em;margin-left:8px;">{imageRef(dicomFile)}</span>
              </td>
              <td>
                <span style="font-size:1.0em;margin-left:8px;" title={fileName}>{abbrevFileName}</span>
              </td>
            </tr>
          </table>
        </span>
        <b> {""} </b>
      </span>
    }
    formatDraggable(color, id, Some(content))
  }

  private def rtimageToHtml(color: String, id: LocalHtmlId, first: Date, rtimage: DicomFile, req: SeriesMakerReq): Elem = {

    val al = rtimage.al

    val beamName: String = {
      val attr = al.get(TagByName.ReferencedBeamNumber)

      0 match {
        case _ if attr == null =>
          val g = Util.angleRoundedTo90(Util.gantryAngle(al))
          val c = Util.angleRoundedTo90(Util.collimatorAngle(al))
          val mv = {
            val m = DicomUtil.findAllSingle(al, TagByName.KVP).head.getDoubleValues.head / 1000
            if (m.round == m) m.round.toString else Util.fmtDbl(m)
          }
          s"** G:$g C:$c Mv:$mv"
        case _ if Phase2Util.getBeamNameOfRtimage(req.rtplan, al).isDefined => // get beam name from plan
          Phase2Util.getBeamNameOfRtimage(req.rtplan, al).get
        case _ => // have beam number, but no plan
          attr.getIntegerValues.head.toString
      }
    }

    val elapsed_ms = SeriesMakerReq.getContentDateTime(al).getTime - first.getTime

    <tr>
      <td>
        {formatRtimage(color, id, elapsed_ms, beamName, rtimage)}
      </td>
    </tr>
  }

  private def rtimageGroupToHtml(valueMap: ValueMapT, group: Seq[DicomFile], groupIndex: Int, color: String, req: SeriesMakerReq): Elem = {

    val firstFormat = new SimpleDateFormat("YYYY EEE MMM d HH:mm")

    val first = SeriesMakerReq.getContentDateTime(group.head.al)

    val machineName = getMachineName(group.head.al, valueMap)

    val headerText = Seq(machineName, Util.formatDate(firstFormat, first)).mkString(" :: ")

    val content = {
      <tr>
        <td>
          <table class="table table-bordered" style={s"border: 3px solid $color;"}>
            <thead>
              <tr>
                <th style="text-align:center;">
                  {headerText}
                </th>
              </tr>
            </thead>
            {group.indices.map(i => rtimageToHtml(color, LocalHtmlId(groupIndex, i), first, group(i), req))}
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
    new WebPlainText(label = "Beams", showLabel = false, col = 4, offset = 1, _ => content)
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

      val sessionDirName = req.rtimageList.head.file.getParentFile.getName

      <tr>
        <td>
          <table>
            <tr>
              <td>
                {formatDraggable(planColor, id, None)}
              </td>
              <td>
                <span style="margin-left:20px;">{imageRef(sessionDirName, Util.sopOfAl(rtimage) + ".png")}</span>
              </td>
              <td>
                <span style="margin-left:20px; width:300px;"><b>{beamName}</b></span>
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

    new WebPlainText(label = "Plan", showLabel = false, col = 5, offset = 1, _ => content)

  }

  private def makeUploadForm(): WebForm = {

    val buttonRow = List(nextButton, cancelButton)

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(buttonRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = 10, runScript = None)

    form
  }

  private def patientIdText(): WebUtil.WebInputText = {
    new WebUtil.WebInputText("PatientID", 3, 0, "Patient ID (MRN).")
  }

  private def patientNameText(): WebUtil.WebInputText = {
    new WebUtil.WebInputText("PatientName", 3, 0, "Patient Name.")
  }

  private val zipFileNameLabel = "Download File"

  private def zipFileName(): WebUtil.WebInputText = {
    new WebUtil.WebInputText(zipFileNameLabel, 3, 0, "Name of downloaded zip file.")
  }

  private val machineLabel = "Machine"
  private val procedureLabel = "Procedure"

  private def makeAssignForm(valueMap: ValueMapT, req: SeriesMakerReq): WebForm = {

    val institutionPK = WebUtil.getUser(valueMap).get.institutionPK

    def machineSelection(): WebInputSelect = {
      val multileafCollimatorPK = req.machine.multileafCollimatorPK
      //noinspection ScalaUnusedSymbol
      def list(response: Option[Response]): List[(String, String)] = {
        Machine
          .listMachinesFromInstitution(institutionPK)
          .filter(m => (m.multileafCollimatorPK == multileafCollimatorPK) && (m.tpsID_real.isDefined && m.serialNumber.isDefined))
          .map(m => (m.machinePK.get.toString, m.getRealId))
          .toList
      }

      val sel = new WebInputSelect(machineLabel, showLabel = true, 3, 0, list, false)
      sel
    }

    def procedureSelection(procedurePK: Option[Long]): WebInputSelect = {
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

      val sel = new WebInputSelect(procedureLabel, showLabel = true, 2, 0, list, false)
      sel
    }

    val procedurePK: Option[Long] = {
      val rtplanSop = SeriesMakerReq.anonymizedRtplanSop(institutionPK, req.rtplan)
      val pk = DicomSeries.getBySopInstanceUID(rtplanSop).flatMap(_.procedurePK).headOption
      pk
    }

    val alRow = List(rtimageListToHtml(valueMap, req), rtplanToHtml(req))

    val selectRow = List(procedureSelection(procedurePK), machineSelection(), zipFileName())
    val textRow = List(patientNameText(), patientIdText())

    val buttonRow = List(resetButton, downloadButton, runButton, cancelButton)
    val spacerRow = List(new WebPlainText("spacer", false, 1, 1, _ => <p style="margin:100px;"> </p>))

    val beamAssignmentList = new WebInputHidden(beamAssignmentListTag)

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(alRow, List(beamAssignmentList), selectRow, textRow, buttonRow, spacerRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = -1, runScript = Some(js))

    form
  }

  /**
    * Make an image file for each image.
    * @param valueMap Get session dir from here.
    * @param req List of DICOM files.
    */
  private def makePngImageFiles(valueMap: ValueMapT, req: SeriesMakerReq): Unit = {
    val dir = WebUtil.sessionDir(valueMap).get

    def annotate(buf: BufferedImage, al: AttributeList): Unit = {

      val gc = ImageUtil.getGraphics(buf)
      gc.setColor(Color.black)
      val byteArray = DicomUtil.PixelDataToByteArray(al)
      val hash = edu.umro.ScalaUtil.Crypto.hash(byteArray)
      val hashText = RawByte.formatByteArray(hash)

      val text = s"Hash: $hashText"

      val size = ImageText.getTextDimensions(gc, text)

      val xCenter = al.get(TagByName.Columns).getIntegerValues.head / 2
      val yCenter = {
        val rows = al.get(TagByName.Rows).getIntegerValues.head
        val margin = 0
        rows - ((size.getHeight / 2).round + margin)
      }

      ImageText.drawTextCenteredAt(gc, xCenter, yCenter, text)
    }

    def makeImage(al: AttributeList, baseFileName: String): Unit = {
      val image = new DicomImage(al)
      val buf = image.toDeepColorBufferedImage(0.01)
      annotate(buf, al)
      Util.writePng(buf, new File(dir, baseFileName + ".png"))
    }

    val distinctBySop = (req.rtimageList.map(_.al) ++ req.templateList).groupBy(al => Util.sopOfAl(al)).values.map(_.head)
    distinctBySop.foreach(al => makeImage(al, Util.sopOfAl(al)))
  }

  /**
    * Respond when the user clicks the 'Next' button.  Validate files.  If good, go to assign beams page and
    * assign default values to data entry fields.  If bad, show error message.
    * @param valueMap Web parameters.
    * @param alList List of DICOM files.
    * @param response HTTP response.
    */
  private def processNext(valueMap: ValueMapT, alList: Seq[DicomFile], response: Response): Unit = {

    val institutionPK = WebUtil.getUser(valueMap).get.institutionPK

    val seriesMakerReq = SeriesMakerReq.makeRequirements(institutionPK, alList)

    if (seriesMakerReq.isLeft) {
      val errMsg = seriesMakerReq.left.get
      val form = makeUploadForm()
      val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
      form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      val req = seriesMakerReq.right.get
      val imageList: Seq[AttributeList] = req.rtimageList.map(_.al) ++ req.templateList

      def deAnon(attr: Attribute) = AnonymizeUtil.deAnonymizeAttribute(institutionPK, attr).map(_.getSingleStringValueOrEmptyString)

      val PatientID = {
        val list = imageList.flatMap(al => deAnon(al.get(TagByName.PatientID))) :+ req.rtplan.get(TagByName.PatientID).getSingleStringValueOrEmptyString
        val patId = list.find(_.nonEmpty)
        if (patId.isEmpty)
          "NA"
        else
          patId.get
      }

      val PatientName = {
        val list = imageList.flatMap(al => deAnon(al.get(TagByName.PatientName))) :+ req.rtplan.get(TagByName.PatientName).getSingleStringValueOrEmptyString
        val patName = list.find(_.nonEmpty)
        if (patName.isEmpty)
          "NA"
        else
          patName.get
      }

      makePngImageFiles(valueMap, req)

      val newValueMap: ValueMapT = valueMap +
        (assignBeamsTag -> "true") +
        (machineLabel -> req.machine.machinePK.get.toString) +
        (zipFileNameLabel -> { "AQASeries" + Util.timeAsFileName(new Date()) }) +
        (patientIdText().label -> PatientID) +
        (patientNameText().label -> PatientName)

      val form = makeAssignForm(newValueMap, req)
      form.setFormResponse(newValueMap, WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)
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
        text.split(" ").filter(_.nonEmpty) map makePair
      case _ => Seq()
    }

    list
  }

  private def attrText(attr: Attribute): Option[String] = {
    if ((attr != null) && (attr.getSingleStringValueOrNull != null) && attr.getSingleStringValueOrEmptyString.nonEmpty)
      Some(attr.getSingleStringValueOrEmptyString)
    else
      None
  }

  private def assembleSeries(valueMap: ValueMapT, beamAssignmentList: Seq[(Int, LocalHtmlId)], req: SeriesMakerReq): SeriesCache.Entry = {

    val institutionPK = WebUtil.getUser(valueMap).get.institutionPK

    val groupList = groupImages(req.rtimageList)

    def putBeam(beamNumber: Int, beamRef: LocalHtmlId): ConvertDicom.BeamConversion = {
      val rtimage: AttributeList = {
        val al = groupList(beamRef.group)(beamRef.beam).al
        val clear: AttributeList = if (DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(al)).nonEmpty) {
          val clearAl = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(al)).head
          clearAl
        } else {
          al
        }
        clear
      }
      val template = {
        val anon = req.templateList.find(_.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == beamNumber).get
        val clear = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(anon)).head
        clear
      }
      ConvertDicom.BeamConversion(rtimage, template)
    }

    val list = beamAssignmentList.map(ri => putBeam(ri._1, ri._2))

    val PatientName = valueMap(patientNameText().label)
    val PatientID = valueMap(patientIdText().label)

    val InstitutionName: String = {
      def attrIn(alList: Seq[AttributeList]): Option[String] = {
        alList.flatMap(al => DicomUtil.findAllSingle(al, TagByName.InstitutionName)).flatMap(attrText).find(_.trim.nonEmpty)
      }
      Seq(attrIn(Seq(req.rtplan)), attrIn(Seq(req.rtplan))).flatten.head
    }

    val machine = Machine.get(valueMap(machineLabel).toLong).get

    val DeviceSerialNumber = machine.getRealDeviceSerialNumber.get

    val RadiationMachineName = machine.getRealTpsId.get

    val rtplanUid = Util.sopOfAl(req.rtplan)

    val alList: Seq[AttributeList] =
      ConvertDicom.processSeries(
        list.toIndexedSeq,
        RadiationMachineName = RadiationMachineName,
        DeviceSerialNumber = DeviceSerialNumber,
        PatientName = PatientName,
        PatientID = PatientID,
        InstitutionName = InstitutionName,
        rtplanUid
      )

    val entry = SeriesCache.put(session = valueMap(WebUtil.sessionLabel), beamAssignmentList = valueMap(beamAssignmentListTag), alList, req.rtplan)

    entry
  }

  /**
    * Respond to the user's request for download by creating a zipped version of the modified DICOM files for download.
    * @param valueMap User specified parameters.
    * @param alList List of uploaded files.
    * @param response HTML response.
    */
  private def processDownload(valueMap: ValueMapT, alList: Seq[DicomFile], response: Response): Unit = {
    val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
    val seriesMakerReq = SeriesMakerReq.makeRequirements(institutionPK, alList)

    val assignBeams = getAssignedBeams(valueMap)

    if (assignBeams.isEmpty) {
      val errMsg = "No beams have been assigned to the plan."
      val form = makeAssignForm(valueMap, seriesMakerReq.right.get)
      val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
      form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      val entry = assembleSeries(valueMap, assignBeams, seriesMakerReq.right.get)

      val entity = new ByteArrayRepresentation(entry.content, MediaType.APPLICATION_ZIP)
      response.setEntity(entity)
      response.setStatus(Status.SUCCESS_OK)

      val fileName = {
        val n = FileUtil.replaceInvalidFileNameCharacters(valueMap(zipFileNameLabel).trim, '_')
        if (n.toLowerCase.endsWith(".zip")) n else n + ".zip"
      }
      WebUtil.setDownloadName(response, fileName)
      logger.info(s"Downloading SeriesMaker file $fileName ...")
    }
  }

  /**
    * Unzip to the content to DICOM files, anonymize each, then return a list of named attribute lists.
    *
    * Note: The file names are preserved, not anonymized.  This is to help with debugging and diagnosing problems.
    *
    * @param content Bytes of DICOM content.
    * @param institutionPK PK of institution for anonymizing.
    */
  private def contentToNamedAl(content: Array[Byte], institutionPK: Long): Seq[(String, AttributeList)] = {

    val bis = new ByteArrayInputStream(content)

    val list = FileUtil.writeZipToNamedByteArrays(bis.asInstanceOf[InputStream])

    def writeAl(name: String, content: Array[Byte]): (String, AttributeList) = {
      val al = new AttributeList
      val bi = new ByteArrayInputStream(content)
      val di = new DicomInputStream(bi)
      al.read(di)

      val anonAl = AnonymizeUtil.anonymizeDicom(institutionPK, al)
      (name, anonAl)
    }

    list.map(nc => writeAl(nc._1, nc._2))
  }

  private def processRun(valueMap: ValueMapT, alList: Seq[DicomFile], response: Response): Unit = {
    val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
    val seriesMakerReq = SeriesMakerReq.makeRequirements(institutionPK, alList)

    val assignBeams = getAssignedBeams(valueMap)

    if (assignBeams.isEmpty) {
      val errMsg = "No beams have been assigned to the plan."
      val form = makeAssignForm(valueMap, seriesMakerReq.right.get)
      val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
      form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      val entry = assembleSeries(valueMap, assignBeams, seriesMakerReq.right.get)

      val namedAlList = contentToNamedAl(entry.content, institutionPK)

      val runTrait = {
        val procedurePK = valueMap(procedureLabel).toInt
        val restlet = WebRun.get(procedurePK)
        val r = restlet.right.get
        r.asInstanceOf[RunTrait[RunReqClass]]
      }

      val runValueMap = Map((WebUtil.sessionLabel, Session.makeUniqueId), (WebUtil.userIdRealTag, valueMap(WebUtil.userIdRealTag)))
      runTrait.validate(runValueMap, alList = namedAlList.map(_._2), xmlList = Seq()) match {
        case Left(errorMap) =>
          val form = makeAssignForm(valueMap, seriesMakerReq.right.get)
          form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)

        case Right(_) =>
          val dir = WebUtil.sessionDir(runValueMap).get
          dir.mkdirs()
          def writeAl(nameAl: (String, AttributeList)): Unit = DicomUtil.writeAttributeListToFile(nameAl._2, new File(dir, nameAl._1), "AQA")
          namedAlList.foreach(writeAl)
          RunProcedure.runIfDataValid(runValueMap, response, runTrait, sync = false)
      }
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request, anonymizeFileNames = false)

    def buttonIs(button: FormButton): Boolean = {
      val value = valueMap.get(button.label)
      value.isDefined && value.get.equals(button.label)
    }

    try {
      val alList = dicomFilesInSession(valueMap).filter(_.attributeList.isDefined)

      0 match {
        // case _ if user.isEmpty                                                                                   => updateMach()

        case _ if buttonIs(cancelButton) =>
          response.redirectSeeOther("/SeriesMaker")

        case _ if buttonIs(nextButton) || buttonIs(resetButton) =>
          processNext(valueMap, alList, response)

        case _ if buttonIs(downloadButton) =>
          processDownload(valueMap, alList, response)

        case _ if buttonIs(runButton) =>
          processRun(valueMap, alList, response)

        case _ =>
          val form = makeUploadForm()
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
