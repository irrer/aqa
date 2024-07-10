package org.aqa.webrun.convertDicomDev

import com.pixelmed.dicom.AttributeList
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
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object SeriesMaker {
  private val path = new String((new SeriesMaker).pathOf)
}

class SeriesMaker extends Restlet with SubUrlRoot with Logging {

  private val pageTitle = "Series Maker"

  private val assignBeamsTag = "assignBeams"

  private val elapsedFormat = new SimpleDateFormat("mm:ss")

  private def colorList =
    Seq(
      "#39698C",
      "#74a892",
      "#008585",
      "#c7522a",
      "#8a508f",
      "#bc5090",
      "#ff6361",
      "#ff8531",
      "#ffa600"
    )

  class FormButtonProcedure(name: String, val procedure: Option[Procedure]) extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def nextButton = makeButton("Next", ButtonType.BtnDefault)

  private def resetButton = makeButton("Reset", ButtonType.BtnDefault)

  private val js = {
    """
      |console.log("Starting");
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
      |  console.log("drop 1 ev: " + ev);
      |  ev.preventDefault();
      |  console.log("drop 2 ev: " + ev);
      |  var data = ev.dataTransfer.getData("text");
      |  console.log("drop 3 ev: " + ev);
      |  ev.target.appendChild(document.getElementById(data));
      |  console.log("drop 4 ev: " + ev);
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

  private def formatDraggable(color: String, id: String, content: Option[Elem]): Elem = {
    val borderRadius = "border-radius:10px"
    val width = "width:250px"
    val height = "height:38px"
    val border = s"border: 2px solid $color"

    val inner: Seq[Elem] = {
      if (content.isDefined)
        Seq(<span id={id + "_drag"} draggable="true" ondragstart="drag(event)">{content.get}</span>)
      else
        Seq()
    }

    <div id={id} style={s"align:auto;$borderRadius;$border;$width;$height;"} ondrop="drop(event)" ondragover="allowDrop(event)">{inner}</div>
  }

  private def formatRtimage(color: String, id: String, elapsed_ms: Long, beamName: String): Elem = {
    val timeText = Util.formatDate(elapsedFormat, new Date(elapsed_ms))
    val index = (id.replaceAll(".*_", "").toInt + 1).toString
    val content = {
      <span>
        <span class="badge badge-secondary" style={s"background: $color;margin:2px;"}>
          <table>
            <tr>
              <td>
                <span style="vertical-align:middle;font-size:2.0em;margin-right:12px;">{index}</span>
              </td>
              <td>
                <span style="vertical-align:middle;font-size:1.0em;">{timeText}</span>
              </td>
            </tr>
          </table>
        </span>
        <b> {beamName} </b>
      </span>
    }
    formatDraggable(color, id, Some(content))
  }

  private def rtimageToHtml(color: String, id: String, first: Date, al: AttributeList, req: SeriesMakerReq): Elem = {

    val beamName: String = {
      val attr = al.get(TagByName.ReferencedBeamNumber)

      0 match {
        case _ if attr == null =>
          "-- Beam Name NA --"
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
            </thead>{group.indices.map(i => rtimageToHtml(color, s"RI_${groupIndex}_$i", first, group(i), req))}
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

    val nameList = DicomUtil.findAllSingle(req.rtplan, TagByName.BeamName).map(_.getSingleStringValueOrEmptyString).sorted.map(name => Util.normalizeBeamName(name))

    def toHtml(id: String, name: String): Elem = {
      <tr>
        <td>
          <table>
            <tr>
              <td>
                {formatDraggable("darkgrey", id, None)}
              </td>
              <td>
                <span style="margin-left:20px; width:200px;"><b>{name}</b></span>
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
        {nameList.indices.map(i => toHtml("Plan_" + i, nameList(i)))}
      </table>
    }

    new WebPlainText(label = "Plan", showLabel = false, col = 4, offset = 1, _ => content)

  }

  private def makeUploadForm(valueMap: ValueMapT): WebForm = {

    val dicomList = dicomFilesInSession(valueMap).flatMap(_.attributeList)

    val req = SeriesMakerReq.makeRequirements(dicomList)

    val buttonRow = List(nextButton, cancelButton)

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(buttonRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = 10, runScript = Some(js))

    form
  }

  private def makeAssignForm(valueMap: ValueMapT, seriesMakerReq: SeriesMakerReq): WebForm = {

    val buttonRow = List(resetButton, cancelButton)
    val alRow = List(rtimageListToHtml(valueMap, seriesMakerReq), rtplanToHtml(seriesMakerReq))

    val action = SeriesMaker.path
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(buttonRow, alRow)

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

        case _ =>
          val form = makeUploadForm(valueMap: ValueMapT)
          form.setFormResponse(valueMap, errorMap = WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)

        // case _ if machine.isEmpty                                                                                => updateMach()
        // case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        // case _ if buttonIs(valueMap, cancelButton)                                                               => updateMach()
        // case _ if buttonIs(valueMap, backButton)                                                                 => updateMach()

        // case _ if makeList.exists(make => valueMap.contains(make.makeButton.label)) => validateAndMake(valueMap, response)

        // case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

    /*
     */
  }
}
