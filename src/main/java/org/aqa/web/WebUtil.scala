package org.aqa.web

import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import scala.xml.PrettyPrinter
import scala.xml.Node
import org.restlet.data.MediaType
import scala.xml.MetaData
import scala.xml.Attribute
import scala.xml.Text
import org.restlet.data.Status
import scala.xml.Elem
import org.restlet.data.Parameter
import java.util.Date
import org.restlet.data.Form
import org.aqa.Logging
import org.restlet.data.Method
import java.io.File
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.restlet.ext.fileupload.RestletFileUpload
import java.io.InputStream
import java.io.FileOutputStream
import java.lang.Class
import edu.umro.ScalaUtil.Trace._
import org.aqa.db.User
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.Row
import org.apache.poi.ss.usermodel.Cell
import java.text.SimpleDateFormat
import java.text.ParseException
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.TagFromName
import org.aqa.db.Machine
import org.aqa.Util
import org.aqa.db.Machine.MMI
import org.aqa.DicomFile
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import com.pixelmed.dicom.DicomInputStream
import org.aqa.AnonymizeUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.CachedUser
import org.aqa.Config
import org.aqa.db.Institution
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.FileMetaInformation
import com.pixelmed.dicom.TransferSyntax
import org.aqa.db.DicomSeries

object WebUtil extends Logging {

  val bs = "\n.bs-example{ margin: 20px; }" // .bs-example{ margin: 20px;  }

  val spacer = "\n.spacer {  margin-top: 40px; }"

  private val singleQuote = "@@quote1@@"
  private val doubleQuote = "@@quote2@@"
  val amp = "@@amp@@"
  val nl = "@@nl@@"
  val openCurly = "@@openCurly@@"
  val closeCurly = "@@closeCurly@@"
  val nbsp = "@@nbsp@@"

  val titleNewline = "@@amp@@#10;"

  val aqaAliasAttr = { <div aqaalias=""/> }.attributes
  def ifAqaAliasAttr(elem: Elem, aqaAlias: Boolean) = if (aqaAlias) { elem % aqaAliasAttr } else elem
  def wrapAlias(text: String) = <span aqaalias="">{ text }</span>
  def wrapAlias(elem: Elem) = <span aqaalias="">{ elem }</span>

  def snglQuote(text: String): String = singleQuote + text + singleQuote

  def dblQuote(text: String): String = doubleQuote + text + doubleQuote

  def xmlToText(document: Node): String = {
    new PrettyPrinter(1024, 2).format(document).
      replace(singleQuote, "'").
      replace(doubleQuote, "\"").
      replace(amp, "&").
      replace(nl, "\n").
      replace(openCurly, "{").
      replace(closeCurly, "}").
      replace(nbsp, "&nbsp")
  }

  def cleanClassName(className: String) = className.substring(className.lastIndexOf('.') + 1).replace("$", "")

  def pathOf(subUrl: SubUrl.Value, className: String) = "/" + subUrl + "/" + cleanClassName(className)

  def pathOf(subUrl: SubUrl.Value, any: Any) = "/" + subUrl + "/" + cleanClassName(any.getClass.getName)

  /**
   * Use the referrer reference to build a full path from a relative path.
   */
  def relativeUrlToFullPath(response: Response, path: String): String = {
    response.getRequest.getReferrerRef.getHostIdentifier
    response.getRequest.getReferrerRef.getHostIdentifier + path
  }

  /**
   * Convert the given stream to DICOM and return the attribute list.  If it
   * is not DICOM, then return None.
   */
  private def isDicom(inputStream: InputStream): Option[AttributeList] = {
    try {
      val al = new AttributeList
      val dicomInStream = new DicomInputStream(inputStream)
      al.read(dicomInStream)
      // require it to have an SOPInstanceUID to be valid DICOM.  Throws an exception if it does not have one
      al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull.trim
      Some(al)
    } catch {
      case t: Throwable => None
    }
  }

  /**
   * Write the input stream to the file.
   */
  private def saveFile(inputStream: InputStream, file: File, request: Request) = {
    file.getParentFile.mkdirs

    val outputStream = new ByteArrayOutputStream

    val buffer = Array.ofDim[Byte](16 * 1024)
    var size = inputStream.read(buffer)
    while (size != -1) {
      outputStream.write(buffer, 0, size)
      size = inputStream.read(buffer)
    }
    outputStream.flush
    outputStream.close

    val byteInStream = new ByteArrayInputStream(outputStream.toByteArray)

    isDicom(byteInStream) match {
      case Some(al) => {
        val user = CachedUser.get(request)
        val institution = user.get.institutionPK
        val anon = AnonymizeUtil.anonymizeDicom(institution, al)
        DicomUtil.writeAttributeListToFile(anon, file, "AQA")
      }
      case _ => {
        file.delete
        file.createNewFile
        val fileOutStream = new FileOutputStream(file)
        fileOutStream.write(outputStream.toByteArray)
        fileOutStream.flush
        fileOutStream.close
      }
    }
  }

  def sessionDir(valueMap: ValueMapT): Option[File] = {
    valueMap.get(sessionLabel) match {
      case Some(sessionId) => Some(Session.idToFile(sessionId))
      case _ => None
    }
  }

  /**
   *  Parse values that are part of the URL.  The Restlet way of doing this could not be ascertained.
   */
  private def parseOriginalReference(request: Request): ValueMapT = {
    val text = request.getOriginalRef.toString
    val paramStart = text.indexOf('?')
    if (paramStart == -1)
      emptyValueMap
    else {
      def getKeyVal(kv: String): (String, String) = {
        val items = kv.split('=')
        items.size match {
          case 2 => (items(0), items(1))
          case 1 => (items(0), null)
          case _ => (null, null)
        }
      }
      text.substring(paramStart + 1).split('&').map(kv => getKeyVal(kv)).toMap
    }
  }

  private def saveFileList(request: Request): ValueMapT = {
    val valueMap = parseOriginalReference(request)

    val dir = sessionDir(valueMap) match {
      case Some(dir) => {

        val upload = new RestletFileUpload(new DiskFileItemFactory(500, dir)) // TODO change size to -1 ?
        val itemIterator = upload.getItemIterator(request.getEntity);

        val userId: String = getUser(request) match { case Some(user) => user.id; case _ => "unknown" }
        while (itemIterator.hasNext) {
          val ii = itemIterator.next
          if (!ii.isFormField) {
            val file = new File(dir, ii.getName)
            logger.info("Uploading file from user " + userId + " to " + file.getAbsolutePath)
            saveFile(ii.openStream, file, request)
          }
        }
      }
      case _ => throw new RuntimeException("Unexpected internal error. None in WebUtil.saveFileList")
    }
    valueMap
  }

  def firstPartOf(text: String, maxLen: Int): String = {
    text match {
      case _ if text == null => ""
      case _ if (text.size <= maxLen) => text
      case _ => text.substring(0, maxLen - 3) + "..."
    }
  }

  private def ensureSessionId(valueMap: ValueMapT): ValueMapT = {
    if (valueMap.get(sessionLabel).isDefined) valueMap else (valueMap ++ (Map((sessionLabel, Session.makeUniqueId))))
  }

  private def parseForm(form: Form): ValueMapT = {
    val paramList = form.toArray().toList.filter(_.isInstanceOf[Parameter]).map(_.asInstanceOf[Parameter])

    paramList.map(p => (p.getName, p.getValue)).toMap
  }

  /**
   * Return true if the request is an upload
   */
  def requestIsUpload(request: Request): Boolean = {
    val methodIsUpload = (request.getMethod == Method.POST) || (request.getMethod == Method.PUT)
    def entity = request.getEntity
    def mediaType = request.getEntity.getMediaType
    //    if (mediaType != null) { // TODO rm
    //      Trace.trace(mediaType)
    //      Trace.trace(mediaType.getMainType)
    //      Trace.trace(mediaType.getSubType)
    //      Trace.trace(mediaType.getParent)
    //      Trace.trace(mediaType.getParameters)
    //    }
    def mediaTypeIsUpload = MediaType.MULTIPART_FORM_DATA.equals(mediaType, true)

    methodIsUpload && (entity != null) && (mediaType != null) && mediaTypeIsUpload
  }

  type ValueMapT = Map[String, String]

  val emptyValueMap = Map[String, String]()

  type StyleMapT = Map[String, Style]

  val styleNone = Map[String, Style]()

  def getValueMap(request: Request): ValueMapT = {
    if (requestIsUpload(request))
      saveFileList(request)
    else
      ensureSessionId(parseOriginalReference(request) ++ parseForm(new Form(request.getEntity)))
  }

  def simpleWebPage(content: Elem, status: Status, title: String, response: Response) = {
    val indentedContent = {
      <div class="row col-md-10 col-md-offset-1">
        { content }
      </div>
    }
    response.setEntity(wrapBody(indentedContent, title), MediaType.TEXT_HTML)
    response.setStatus(status)
  }

  def notFound(response: Response) = {
    val status = Status.CLIENT_ERROR_NOT_FOUND
    val content = {
      <div>{ status.toString } Error : No such web page.</div>
    }
    simpleWebPage(content, status, "Not Found", response)
  }

  def internalFailure(response: Response, message: String): Unit = {
    val status = Status.SERVER_ERROR_INTERNAL
    val messageAsHtml = message.split('\n').map(line => <br>{ line }</br>)

    val content = {
      <div>{ status.toString } <p/> { messageAsHtml }</div>
    }
    logger.warn(Status.SERVER_ERROR_INTERNAL.toString + " shown to user " + getUserIdOrDefault(response.getRequest, "unknown") + " : " + message)
    simpleWebPage(content, status, "Not Found", response)
  }

  def internalFailure(response: Response, throwable: Throwable): Unit = {
    internalFailure(response, "Unexpected internal failure: " + throwable.getMessage + "\nStack trace:\n" + fmtEx(throwable))
  }

  val HTML_PREFIX = "<!DOCTYPE html>\n"

  def wrapBody(content: Elem, pageTitle: String, refresh: Option[Int], c3: Boolean, runScript: Option[String]): String = {

    val refreshMeta = Seq(refresh).flatten.filter(r => r > 0).map(r => { <meta http-equiv='refresh' content={ r.toString }/> })

    val c3Refs: Seq[Elem] = {
      if (c3) {
        // There are newer versions of these files but they don't seem to work in AQA.
        Seq(
          <link href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.css" rel="stylesheet"/>,
          <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js"></script>,
          <script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.js"></script>)
      } else Seq[Elem]()
    }

    val runScriptTag = "@@script@@"

    val page = {
      <html lang="en">
        <head>
          <title>{ pageTitle }</title>
          { refreshMeta }
          <link rel="icon" href="/static/images/favicon.ico?" type="image/x-icon"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap-theme.min.css"/>
          <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/min/dropzone.min.css"/>
          <link rel="stylesheet" href="/static/AQA.css"/>
          <link href="/static/bootstrap/datetime/bootstrap-datetimepicker.min.css" rel="stylesheet" media="screen"/>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/js/bootstrap.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/min/dropzone.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-timeago/1.5.4/jquery.timeago.min.js"></script>
          <script src="/static/tooltip/tooltip.js"></script>
          { c3Refs /* only include c3 when needed */ }
          <script src="/static/zoom/jquery.zoom.js"></script>
          <script type="text/javascript" src="/static/bootstrap/datetime/bootstrap-datetimepicker.js" charset="UTF-8"></script>
          <script src="/static/AQA.js"></script>
        </head>
        <body style='background-image: url("/static/images/restrictions.png"); background-repeat: repeat-x;'>
          <header class="topbar">
            <div class="row">
              <div class="col-md-1 col-md-offset-9" style="background-color: white;">
                <strong>
                  <br/>
                  <a href="/run/WebRunIndex">Run Procedure</a>
                  <br/>
                  <a href="/view/OutputList">Results</a>
                  <br/>
                  <a href="/static/admin.html">Administration</a>
                  <br/>
                  <a href="/static/doc/index/index.html">User Guides</a>
                </strong>
              </div>
              <div class="col-md-2">
                <h1 class="fill">
                  <a href="/"><img src="/static/images/logo.png" width="128"/></a>
                </h1>
              </div>
            </div>
          </header>
          { content }
          { runScriptTag }
        </body>
      </html>
    }

    val runScriptContent = runScript match {
      case Some(s) => s
      case _ => ""
    }

    val text = HTML_PREFIX + xmlToText(page).replaceAllLiterally(runScriptTag, runScriptContent)

    logger.debug("HTML delivered:\n" + text)
    text
  }

  def wrapBody(content: Elem, pageTitle: String, refresh: Option[Int]): String = wrapBody(content, pageTitle, refresh, false, None)

  def wrapBody(content: Elem, pageTitle: String): String = wrapBody(content, pageTitle, None, false, None)

  def setResponse(text: String, response: Response, status: Status): Unit = {
    response.setStatus(status)
    response.setEntity(text, MediaType.TEXT_HTML)
  }

  def respond(content: Elem, pageTitle: String, response: Response, status: Status): Unit = {
    setResponse(wrapBody(content, pageTitle), response, status)
  }

  def respond(content: Elem, title: String, response: Response): Unit = respond(content, title, response, Status.SUCCESS_OK)

  trait ToHtml {
    def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem;
    //def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = toHtml(valueMap, errorMap, None) // TODO ?
    def toHtml(valueMap: ValueMapT): Elem = toHtml(valueMap, styleNone, None)
    def toHtml: Elem = toHtml(emptyValueMap, styleNone, None)
  }

  implicit class WebRow(val colList: List[ToHtml]) extends ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      <div class="form-group">
        <div class="row">
          { colList.map(c => c.toHtml(valueMap, errorMap, response)) }
        </div>
      </div>
    }
  }

  implicit def ColList(col: ToHtml): List[ToHtml] = List(col);

  def markLiteralValue(label: String): String = "@@@@" + label + "@@@@"

  def makeAlertBox(text: String): String = {
    "alert(\"" + text.replaceAllLiterally(singleQuote, "\\'").replaceAllLiterally(doubleQuote, "\\\"") + "\");"
  }

  val sessionLabel = "session"

  class WebForm(action: String, title: Option[String], rowList: List[WebRow], fileUpload: Int) extends ToHtml {

    def this(action: String, rowList: List[WebRow]) = this(action, None, rowList, 0)
    def this(action: String, rowList: List[WebRow], fileUpload: Int) = this(action, None, rowList, fileUpload)

    val rowListWithSession = (new WebInputSession) ++ rowList

    val uploadFileInput: Option[IsInput] = if (validCol(fileUpload)) Some(new IsInput("uploadFile")) else None

    def findInput(label: String): Option[IsInput] = {
      rowList.map(r => r.colList).flatten.filter(i => i.isInstanceOf[IsInput]).map(in => in.asInstanceOf[IsInput]).find(isIn => isIn.label.equals(label))
    }

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val valueMapWithSession = if (valueMap.get(sessionLabel).isDefined) valueMap else (Map((sessionLabel, Session.makeUniqueId)) ++ valueMap)

      val mainForm =
        <form action={ action } method={ Method.POST.toString } class="form-horizontal" role="form">
          { rowListWithSession.map(r => r.toHtml(valueMapWithSession, errorMap, response)) }
        </form>;

      val alreadyUploadedFiles: Elem = {

        def fileToText(file: File, index: Int): String = {
          val name = "preloadedFile" + index
          val text =
            "var " + name + " = { name : '" + file.getName + "', size : " + file.length + " };" + nl +
              "uploadFile.emit('addedfile', " + name + ");" + nl +
              "uploadFile.emit('complete', " + name + ");" + nl
          text
        }

        val dir = Session.idToFile(valueMapWithSession(sessionLabel))

        val text: String = if (dir.isDirectory && dir.list.nonEmpty) {
          val dz = "var uploadFile = new Dropzone(\"#uploadFile\");" + nl

          val fileText = dir.listFiles.toSeq.zipWithIndex.map(fi => fileToText(fi._1, fi._2)).mkString(" ")
          (dz + fileText).replaceAllLiterally("\"", doubleQuote).replaceAllLiterally("'", singleQuote)
        } else ""

        <script>{ text }</script>
      }

      val titleHtml: Elem = {
        title match {
          case Some(t) => <h2>{ t }</h2>
          case _ => <span></span>
        }
      }

      val html = {
        if (validCol(fileUpload)) {
          val sessionId: String = valueMapWithSession.get(sessionLabel).get
          val formClass = "dropzone row " + colToName(fileUpload, 0) + " has-error"

          val uploadStyle: Style = {
            errorMap.get(uploadFileInput.get.label) match {
              case Some(sty) => sty
              case _ => new Style
            }
          }

          val uploadHtml: Elem = {
            val hasError = errorMap.get(uploadFileInput.get.label).isDefined
            val borderColor = if (hasError) "#a94442" else "#cccccc"
            val uploadForm = {
              val cssStyle = "border-color: " + borderColor + "; border-width: 1px; border-radius: 10px; margin-bottom: 15px;"
              <form action={ action + "?" + sessionLabel + "=" + sessionId } class={ formClass } id="uploadFile" style={ cssStyle }></form>
            }
            if (hasError) {
              val style: Style = errorMap.get(uploadFileInput.get.label).get
              uploadForm % style.divAttributes(uploadForm.attributes)
            } else uploadForm
          }

          val uploadForm = {
            <div class="row">
              { uploadHtml }
            </div>
            <div class="row">
              { mainForm }
              { alreadyUploadedFiles }
            </div>
          }
          uploadForm
        } else mainForm
      }

      val elem = {
        <div class={ "row " + colToName(10, 1) }>
          { titleHtml }
          { html }
        </div>
      }
      elem
    }

    def makeFormAlertBox(errorMap: StyleMapT): Option[String] = {
      val quote = "\""
      val back = """\"""
      val replace = back + quote
      def textOfError(style: Style): Option[String] = {
        if (style.isInstanceOf[Error]) {
          val text = style.asInstanceOf[Error].inputTitle.replaceAll("'", singleQuote).replaceAll("\"", doubleQuote)
          Some(text)
        } else
          None
      }

      val textErrorList = errorMap.values.map(s => textOfError(s)).flatten

      if (textErrorList.isEmpty)
        None
      else
        Some("<script>" + makeAlertBox(textErrorList.mkString("\\n\\n")) + "</script>")
    }

    def setFormResponse(valueMap: ValueMapT, errorMap: StyleMapT, pageTitle: String, response: Response, status: Status): Unit = {

      val text = wrapBody(toHtml(valueMap, errorMap, Some(response)), pageTitle, None, false, makeFormAlertBox(errorMap))

      //val text = wrapBody(toHtml(valueMap, errorMap, Some(response)), pageTitle)
      def replace(origText: String, col: Any): String = {
        if (col.isInstanceOf[IsInput]) {
          val input = col.asInstanceOf[IsInput]
          val markedLiteral = markLiteralValue(input.label)
          if (valueMap.get(input.label).isDefined && text.contains(markedLiteral)) {
            origText.replace(markedLiteral, valueMap.get(input.label).get)
          } else origText
        } else origText
      }
      val colList = rowList.map(row => row.colList).flatten
      val finalText = colList.foldLeft(text)((txt, col) => replace(txt, col))

      setResponse(finalText, response, status)
    }
  }

  def valueAsAttr(label: String, valueMap: ValueMapT): MetaData = {
    val value = valueMap.get(label)
    (if (value.isDefined) <input value={ value.get }></input> else <input></input>).attributes
  }

  def placeholderAsAttr(placeholder: String): MetaData = (<input placeholder={ placeholder }/>).attributes

  def idNameClassAsAttr(label: String): MetaData = (<input class="form-control" id={ label } name={ label }/>).attributes

  def idNameClassValueAsAttr(label: String, valueMap: ValueMapT): MetaData = {
    (<input/> % idNameClassAsAttr(label) % valueAsAttr(label, valueMap)).attributes
  }

  def htmlLabel(label: String) = <label class="control-label">{ label }</label>

  def validCol(col: Int): Boolean = (col > 0) && (col <= 12)

  def colToName(col: Int, offset: Int): String = {
    val colClass = if (validCol(col)) ("col-md-" + col) else ""
    val offsetClass = if ((offset > 0) && (offset <= 12)) (" col-md-offset-" + offset) else ""
    colClass + offsetClass
  }

  class Style {
    /**
     * Given the list of attributes for the input, return a modified list that
     * implements the desired style.
     */
    def divAttributes(metaData: MetaData): MetaData = metaData
    def inputAttributes(metaData: MetaData): MetaData = metaData

    override def toString: String = {
      val div = { <toString></toString> }
      val elem = div % divAttributes(div.attributes)
      xmlToText(elem)
    }
  }

  class Error(val inputTitle: String) extends Style {
    override def divAttributes(metaData: MetaData): MetaData = {
      val clss = metaData.get("class")
      val clssText = (if (clss.isDefined) clss.get.toString + " " else "") + "has-error"
      val attr = ((<x/> % metaData) % (<x class={ clssText } title={ inputTitle }/>.attributes)).attributes
      attr
    }
  }

  object Error {
    def make(input: IsInput, inputTitle: String) = {
      Map((input.label, new Error(inputTitle)))
    }
  }

  class Disable extends Style {
    override def inputAttributes(metaData: MetaData): MetaData = {
      ((<x/> % metaData) % (<x disabled="disabled"/>.attributes)).attributes
    }
  }

  object Disable {
    def make(input: IsInput) = Map((input.label, new Disable))
  }

  class DisableWithTitle(inputTitle: String) extends Style {
    override def divAttributes(metaData: MetaData): MetaData = {
      ((<x/> % metaData) % (<x title={ inputTitle }/>.attributes)).attributes
    }
    override def inputAttributes(metaData: MetaData): MetaData = {
      ((<x/> % metaData) % (<x disabled="disabled"/>.attributes)).attributes
    }
  }

  object DisableWithTitle {
    def make(input: IsInput, inputTitle: String) = Map((input.label, new DisableWithTitle(inputTitle)))
  }

  def wrapInput(label: String, showLabel: Boolean, html: Elem, col: Int, offset: Int, styleMap: StyleMapT): Elem = {

    val style = if (styleMap.get(label).isDefined) styleMap.get(label).get else new Style

    val div =
      <div class={ colToName(col, offset) }>
        { if (showLabel) <label class="control-label">{ label }</label> }
        { html % style.inputAttributes(html.attributes) }
      </div>

    div % style.divAttributes(div.attributes)
  }

  def stringToDouble(text: String): Option[Double] = {
    try {
      Some(text.toDouble)
    } catch {
      case t: Throwable => None
    }
  }

  class IsInput(val label: String) {
    def getValOrEmpty(valueMap: ValueMapT): String = {
      val v = valueMap.get(label)
      if (v.isDefined) {
        if (v.get == null) ""
        else v.get
      } else ""
    }

    def getInt(valueMap: ValueMapT): Option[Int] = {
      val text = getValOrEmpty(valueMap).trim
      try {
        Some(text.toInt)
      } catch {
        case t: Throwable => None
      }
    }

    def getDouble(valueMap: ValueMapT): Option[Double] = stringToDouble(getValOrEmpty(valueMap).trim)
  }

  class WebInputText(override val label: String, showLabel: Boolean, col: Int, offset: Int, placeholder: String, aqaAlias: Boolean) extends IsInput(label) with ToHtml {
    def this(label: String, showLabel: Boolean, col: Int, offset: Int, placeholder: String) = this(label, showLabel, col, offset, placeholder, false)
    def this(label: String, col: Int, offset: Int, placeholder: String) = this(label, true, col, offset, placeholder)

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="text"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
      val htmlAlias = ifAqaAliasAttr(html, aqaAlias)
      wrapInput(label, showLabel, htmlAlias, col, offset, errorMap)
    }
  }

  /**
   * Show text that the user can not change.
   */
  class WebPlainText(override val label: String, val showLabel: Boolean, col: Int, offset: Int, html: (ValueMapT) => Elem) extends IsInput(label) with ToHtml {
    def this(label: String, col: Int, offset: Int, content: String) = this(label, true, col, offset, ((Null)) => <div>{ content }</div>)
    def this(label: String, col: Int, offset: Int, content: Elem) = this(label, true, col, offset, ((Null)) => content)
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      wrapInput(label, showLabel, html(valueMap), col, offset, errorMap)
    }
  }

  class WebInputURL(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="url"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
      wrapInput(label, true, html, col, offset, errorMap)
    }
  }

  class WebInputEmail(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="email"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
      wrapInput(label, true, html, col, offset, errorMap)
    }
  }

  class WebInputPassword(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="password"/> % idNameClassAsAttr(label) % placeholderAsAttr(placeholder)
      wrapInput(label, true, html, col, offset, errorMap)
    }
  }

  class WebInputSelect(override val label: String, val showLabel: Boolean, col: Int, offset: Int, selectList: (Option[Response]) => Seq[(String, String)], aqaAlias: Boolean) extends IsInput(label) with ToHtml {

    def this(label: String, col: Int, offset: Int, selList: (Option[Response]) => Seq[(String, String)]) = this(label, false, col, offset, selList, false)

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {

      val curValue: Option[String] = if (valueMap.get(label).isDefined) valueMap.get(label) else None

      def toOption(value: String, text: String): Elem = {

        val opt = if (curValue.isDefined && curValue.get.equals(value)) {
          <option selected="selected" value={ value }>{ text }</option>
        } else {
          <option value={ value }>{ text }</option>
        }

        ifAqaAliasAttr(opt, aqaAlias)
      }

      val list = selectList(response).map(v => toOption(v._1, v._2))
      val html = <select>{ list }</select> % idNameClassValueAsAttr(label, valueMap)
      wrapInput(label, showLabel, html, col, offset, errorMap)
    }
  }

  /**
   * Optionally show a selection list based on evaluating a function.
   */
  class WebInputSelectOption(override val label: String, col: Int, offset: Int, selectList: (Option[Response]) => Seq[(String, String)], show: (ValueMapT) => Boolean) extends WebInputSelect(label, true, col, offset, selectList, true) {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      if (show(valueMap)) {
        super.toHtml(valueMap, errorMap, response)
      } else { <div></div> }
    }
  }

  /**
   * Optionally show a selection list based on evaluating a function.
   */
  class WebInputSelectMachine(override val label: String, col: Int, offset: Int) extends IsInput(label) with ToHtml {

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {

      val curValue: Option[String] = if (valueMap.get(label).isDefined) valueMap.get(label) else None

      def toOption(value: String, choice: String): Elem = {

        val opt = if (curValue.isDefined && curValue.get.equals(value)) {
          <option selected="selected" value={ value }>{ choice }</option>
        } else {
          <option value={ value }>{ choice }</option>
        }
        opt
      }

      val selectList: Seq[(String, String)] = {
        if (response.isEmpty) Seq[(String, String)]()
        else {
          val machListAll = if (userIsWhitelisted(response.get.getRequest))
            Machine.list
          else {
            val user = CachedUser.get(response.get.getRequest)
            Machine.listMachinesFromInstitution(user.get.institutionPK)
          }

          val machListAvail = machListAll.filter(m => m.serialNumber.isEmpty)
          def machToDescription(m: Machine): String = {
            val instName = AnonymizeUtil.decryptWithNonce(m.institutionPK, Institution.get(m.institutionPK).get.name_real.get)
            val machId = AnonymizeUtil.decryptWithNonce(m.institutionPK, m.id_real.get)
            instName + " : " + machId
          }
          val pair = machListAvail.map(m => (m.machinePK.get.toString, machToDescription(m)))
          pair
        }
      }

      val shouldShow: Boolean = {
        val attrListList = dicomFilesInSession(valueMap).map(df => df.attributeList).flatten

        /**
         * Get all <code>DeviceSerialNumber</code> from DICOM files, but ignore any RTPLAN (primary) serial numbers because
         * they reference the planning system, not a treatment machine.
         */
        def serialNumberOf(al: AttributeList): IndexedSeq[String] = {
          val all = DicomUtil.findAllSingle(al, TagFromName.DeviceSerialNumber).map(attr => attr.getSingleStringValueOrEmptyString).filterNot(_.equals("")).distinct
          val planDsn = {
            val dsn = {
              val attr = al.get(TagFromName.DeviceSerialNumber)
              if (attr == null) "" else attr.getSingleStringValueOrEmptyString
            }

            if ((!dsn.equals("")) && Util.isModality(al, SOPClass.RTPlanStorage))
              Seq(dsn)
            else
              Seq[String]()
          }
          all.diff(planDsn)
        }

        def isMatchingMachine = {
          // get all non-plan serial numbers, but do not allow serial numbers from previously uploaded plans
          val serialNumbers = attrListList.map(al => serialNumberOf(al)).flatten.distinct.diff(DicomSeries.planDeviceSerialNumberList)
          val machList = serialNumbers.map(sn => Machine.findMachinesBySerialNumber(sn)).flatten
          machList.nonEmpty
        }

        val ss = attrListList.nonEmpty && (!isMatchingMachine)

        ss
      }

      val html = {
        if (shouldShow) {
          val list = selectList.map(v => toOption(v._1, v._2))
          val input = { <select>{ list }</select> % idNameClassValueAsAttr(label, valueMap) }
          wrapInput(label, true, input, col, offset, errorMap)
        } else {
          {
            <div></div>
          }
        }
      }

      html
    }
  }

  class WebInputCheckbox(override val label: String, val showLabel: Boolean, title: Option[String], col: Int, offset: Int) extends IsInput(label) with ToHtml {

    def this(label: String, showLabel: Boolean, col: Int, offset: Int) = this(label, showLabel, None, col, offset)
    def this(label: String, col: Int, offset: Int) = this(label, true, None, col, offset)

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val input = <input type="checkbox"/> % idNameClassValueAsAttr(label, valueMap)
      val inputWithValue: Elem = {
        if (valueMap.get(label).isDefined && valueMap.get(label).get.equals("true")) input % (<input checked="true"/>).attributes
        else input
      }

      val html = {
        val tr = {
          <tr>
            <td>{ inputWithValue }</td>
            <td> <label style="vertical-align:middle; margin: 5px;"> { if (showLabel) label else "" }</label></td>
          </tr>
        }

        if (title.isDefined)
          <table title={ title.get }>{ tr }</table>
        else
          <table>{ tr }</table>
      }
      wrapInput(label, false, html, col, offset, errorMap)
    }
  }

  /**
   * Standard Twitter bootstrap button types.
   */
  object ButtonType extends Enumeration {
    type ButtonType = Value

    val BtnDefault = Value("btn-default")
    val BtnPrimary = Value("btn-primary")
    val BtnSuccess = Value("btn-success")
    val BtnInfo = Value("btn-info")
    val BtnWarning = Value("btn-warning")
    val BtnDanger = Value("btn-danger")
    val BtnLink = Value("btn-link")
  }

  object SubUrl extends Enumeration {
    type SubUrl = Value

    val root = Value("")
    val admin = Value("admin")
    val run = Value("run")
    val view = Value("view")
    val doc = Value("doc")

    def url(subUrl: SubUrl.Value, name: String): String = {
      ("/" + subUrl + "/" + name).replace("//", "/")
    }
  }

  trait SubUrlTrait extends Logging {
    def subUrl: SubUrl.Value

    def url(name: String): String = SubUrl.url(subUrl, name)

    private val className = this.getClass.getName
    private val cn = className.substring(className.lastIndexOf('.') + 1).replace("$", "")
    def pathOf = url(cn)
  }

  trait SubUrlRoot extends SubUrlTrait {
    override def subUrl = SubUrl.root
  }

  trait SubUrlAdmin extends SubUrlTrait {
    override def subUrl = SubUrl.admin
  }

  trait SubUrlRun extends SubUrlTrait {
    override def subUrl = SubUrl.run
  }

  trait SubUrlView extends SubUrlTrait {
    override def subUrl = SubUrl.view
  }

  trait SubUrlDoc extends SubUrlTrait {
    override def subUrl = SubUrl.doc
  }

  /**
   * An HTML button.
   *
   * @param label Name and id of button.
   *
   * @param col Number of columns to occupy (1-12)
   *
   * @param offset Offset column (0-11)
   *
   * @param primary True if this button is primary
   */
  class FormButton(override val label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: (ValueMapT) => String, buttonType: ButtonType.Value, value: String, title: Option[String]) extends IsInput(label) with ToHtml {
    def this(label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: (ValueMapT) => String, buttonType: ButtonType.Value, value: String) = this(label, col, offset, subUrl, action, buttonType, value, None)
    def this(label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: String, buttonType: ButtonType.Value, value: String) = this(label, col, offset, subUrl, (_) => action, buttonType, value)
    def this(label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: String, buttonType: ButtonType.Value) = this(label, col, offset, subUrl, action: String, buttonType, label)
    def this(label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: String) = this(label, col, offset, subUrl, action: String, ButtonType.BtnDefault, label)

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val button = { <button type="submit" class={ "btn " + buttonType.toString } action={ action(valueMap) } value={ value } name={ label }>{ label }</button> }
      wrapInput(label, false, button, col, offset, errorMap)
    }
  }

  class WebInputTextArea(label: String, col: Int, offset: Int, placeholder: String, aqaAlias: Boolean) extends IsInput(label) with ToHtml {
    def this(label: String, col: Int, offset: Int, placeholder: String) = this(label, col, offset, placeholder, false)
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val value = valueMap.get(label)

      val rowCount: Int = {
        val s = if (value.isDefined) { value.get.split("\n").size + 1 } else 3
        Math.max(3, s)
      }

      val html = // must not allow embedded blanks
        <textarea rows={ rowCount.toString }>{ if (value.isDefined) markLiteralValue(label) else "" }</textarea> % idNameClassAsAttr(label) % placeholderAsAttr(placeholder)

      val htmlAlias = ifAqaAliasAttr(html, aqaAlias)
      wrapInput(label, true, htmlAlias, col, offset, errorMap)
    }
  }

  class WebInputDatePicker(override val label: String, col: Int, offset: Int) extends IsInput(label) with ToHtml {

    /** For converting between <code>String</code> and <code>Date</code>. */
    val dateFormat = new SimpleDateFormat("yyyy MMM d")

    /** javascript date format */
    private val jsFormat = "yyyy M d"

    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {

      val value: String = valueMap.get(label) match {
        case Some(v) => v
        case _ => dateFormat.format((new Date).getTime)
      }

      val html =
        {
          <div class="input-group date form_date col-md-5" data-date="" data-date-format={ jsFormat } data-link-field={ label } data-link-format={ jsFormat }>
            <input class="form-control" size="16" type="text" id={ label } name={ label } value={ value }/>
            <span class="input-group-addon">
              <span class="glyphicon glyphicon-remove"></span>
            </span>
            <span class="input-group-addon">
              <span class="glyphicon glyphicon-calendar"></span>
            </span>
            <script type="text/javascript">
              $('.form_date').datetimepicker({ openCurly }
              weekStart: 0,          /* first day is Sunday */
                todayBtn:  1,          /* show button to go quickly to today */
                autoclose: 1,          /* close when date selected */
                todayHighlight: 1,     /* today is highlighted */
                startView: 2,          /* pick day of month (level of granularity) */
                minView: 2,            /* minimum granularity of viewing mode */
                startDate: '2010/1/1', /* minimum selectable date */
                forceParse: true       /* fix: parse to supported form */
              { closeCurly }
              );
            </script>
          </div>
        }

      wrapInput(label, true, html, col, offset, errorMap)
    }
  }

  class WebInputDateTime(label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="datetime-local"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
      wrapInput(label, true, html, col, offset, errorMap)
    }

    /** For converting between <code>String</code> and <code>Date</code>. */
    val dateTimeFormat = WebInputDateTime.dateTimeFormat

    def validateDateTime(text: String): Option[Date] = {

      def inRange(i: Int, lo: Int, hi: Int): Unit = if ((i < lo) || (i > hi)) throw new ParseException("value out of range of " + lo + " to " + hi, 0)

      try {
        val fields = text.replaceAll("[^0-9]", " ").replaceAll("  *", " ").trim.split(" ").map(t => t.toInt)
        val year = if (fields(2) < 100) fields(2) + 2000 else fields(2) // adjust year, eg: 17 to 2017

        inRange(fields(0), 1, 12) // month
        inRange(fields(1), 1, 31) // day of month (does not catch months with less than 31 days)
        inRange(year, 1970, 2100)
        inRange(fields(3), 0, 23) // hour
        inRange(fields(4), 0, 59) // minute

        val formattedText =
          fields(0).formatted("%02d") + "/" +
            fields(1).formatted("%02d") + "/" +
            year.formatted("%02d") + " " +
            fields(3).formatted("%02d") + ":" +
            fields(4).formatted("%02d")

        Some(dateTimeFormat.parse(formattedText))
      } catch {
        case t: Throwable => None
      }

    }

  }

  object WebInputDateTime {
    val dateTimeFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm")
  }

  class WebInputHidden(override val label: String) extends IsInput(label) with ToHtml {
    override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {
      val html = <input type="text" class="hidden"/> % idNameClassValueAsAttr(label, valueMap)
      val elem = { <span class="hidden">{ html }</span> }
      elem
    }
  }

  private class WebInputSession extends WebInputHidden(sessionLabel) with ToHtml;

  /**
   * Given a request, extract the user from it.
   */
  def getUser(request: Request): Option[User] = {
    val cr = request.getChallengeResponse
    if (cr == null) None
    else {
      val u = CachedUser.get(cr.getIdentifier) // backwards compatible with non-anonymized database
      if (u.isDefined) u
      else
        CachedUser.get(cr.getIdentifier)
    }
  }

  /**
   * Return true if the user is whitelisted in the configuration.
   */
  def userIsWhitelisted(userId: String): Boolean = {
    Config.UserWhiteList.map(u => u.toLowerCase.trim).contains(userId.toLowerCase.trim)
  }

  /**
   * Return true if the user is whitelisted in the configuration.
   */
  def userIsWhitelisted(request: Request): Boolean = {
    val cr = request.getChallengeResponse
    if (cr == null) false
    else {
      val userId = request.getChallengeResponse.getIdentifier.toLowerCase.trim
      userIsWhitelisted(userId)
    }
  }

  /**
   * Return true if the user is whitelisted in the configuration.
   */
  def userIsWhitelisted(response: Response): Boolean = {
    userIsWhitelisted(response.getRequest)
  }

  def getUserIdOrDefault(request: Request, dflt: String): String = {
    val cr = request.getChallengeResponse
    logger.info("challenge response given: " + (cr != null) + "    request: " + request.getResourceRef.getRemainingPart) // TODO rm
    if (cr == null) dflt else cr.getIdentifier
  }

  def excelToHtml(workbook: Workbook): String = {

    def doCell(cell: Cell): Elem = {
      val content: String = try {
        if (cell == null) "" else ExcelUtil.cellToString(cell)
      } catch {
        case t: Throwable => ""
      }
      <td>{ content }</td>
    }

    def doRow(row: Row, firstCol: Short, lastCol: Short): Elem = {
      <tr>{ (firstCol until lastCol).map(cellnum => doCell(row.getCell(cellnum))) }</tr>
    }

    def nameToId(name: String): String = name.replaceAll("[# \"'@<>]", "_")

    def sheetHeader(sheet: Sheet, active: Boolean): Elem = {
      val id = "#" + nameToId(sheet.getSheetName)
      if (active) {
        <li class="active"><a data-toggle="tab" href={ id }>{ sheet.getSheetName }</a></li>
      } else {
        <li><a data-toggle="tab" href={ id }>{ sheet.getSheetName }</a></li>
      }
    }

    def doSheetContent(sheet: Sheet, active: Boolean): Elem = {
      val rowList = (sheet.getFirstRowNum to sheet.getLastRowNum).map(rownum => sheet.getRow(rownum)).filter(row => row != null)
      val firstCol: Short =
        rowList.map(row => row.getFirstCellNum).min match {
          case min if (min >= 0) => min
          case _ => 0.toShort
        }

      val lastCol = rowList.map(row => row.getLastCellNum).max
      val classValue = if (active) "tab-pane fade in active" else "tab-pane fade"; // funny, but the Scala compiler requires a ; here

      {
        <div id={ nameToId(sheet.getSheetName) } class={ classValue }>
          <h3>{ sheet.getSheetName }</h3>
          <table class="table table-bordered">
            { (sheet.getFirstRowNum until sheet.getLastRowNum).map(rownum => sheet.getRow(rownum)).filter(row => row != null).map(row => doRow(row, firstCol, lastCol)) }
          </table>
        </div>
      }
    }

    def linkToSheet(sheet: Sheet) = {
      <a href={ "#" + sheet.getSheetName } style="margin: 40px;">{ sheet.getSheetName }</a>
    }

    val html: Elem = {
      <div style="margin: 40px;">
        <ul class="nav nav-tabs">
          { ExcelUtil.sheetList(workbook).zipWithIndex.map(si => sheetHeader(si._1, si._2 == 0)) }
        </ul>
        <div class="tab-content">
          { ExcelUtil.sheetList(workbook).zipWithIndex.map(si => doSheetContent(si._1, si._2 == 0)) }
        </div>
      </div>
    }

    WebUtil.wrapBody(html, "Leaf Offset Correction")
  }

  def fileToDicom(file: File): Option[AttributeList] = {
    try {
      if (DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
        val al = new AttributeList
        al.read(file)
        Some(al)
      } else None
    } catch {
      case t: Throwable => None
    }
  }

  def dicomFilesInSession(valueMap: ValueMapT): Seq[DicomFile] = {
    sessionDir(valueMap) match {
      case Some(dir) if (dir.isDirectory) => DicomFile.readDicomInDir(dir)
      case _ => Seq[DicomFile]()
    }
  }

  def attributeListsInSession(valueMap: ValueMapT): Seq[AttributeList] = {
    dicomFilesInSession(valueMap).map(df => df.attributeList).flatten
  }

  /**
   * Given a value map, determine which machines' DICOM files have been uploaded to the session.
   */
  def machinesInSession(valueMap: ValueMapT): Seq[Machine] = {
    try {
      attributeListsInSession(valueMap).map(al => Machine.attributeListToMachine(al)).flatten
    } catch {
      case t: Throwable =>
        Seq[Machine]()
    }
  }

  //val humanDateFormat = new SimpleDateFormat("EEE d MMM yyyy  HH:mm:ss")

  private def timeAgoFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  private val timeHumanFriendlyTimeAgoFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeAgo(prefix: String, date: Date): Elem = {
    val stdTime = timeAgoFormat.format(date)
    <time class='timeago' datetime={ stdTime }>{ prefix + " " + Util.timeHumanFriendly(date) }</time>
  }

  def timeAgo(date: Date): Elem = timeAgo("", date)

  def showMachineSelector(valueMap: ValueMapT): Boolean = {
    lazy val fileList = sessionDir(valueMap) match {
      case Some(dir) if dir.isDirectory => dir.listFiles.toSeq
      case _ => Seq[File]()
    }
    lazy val alList = attributeListsInSession(valueMap)

    lazy val machList = alList.map(al => Machine.attributeListToMachine(al)).flatten

    alList match {
      case _ if fileList.isEmpty => false
      case _ if alList.isEmpty => false
      case _ if machList.nonEmpty => false
      case _ => true
    }
  }

  def machineList(response: Option[Response]) = {
    def mmiToMachPK(mmi: MMI): String = {
      mmi.machine.machinePK match {
        case Some(pk) => pk.toString()
        case _ => "unknown"
      }
    }

    def mmiToText(mmi: MMI) = {
      val instName = if (mmi.institution.name_real.isEmpty) mmi.institution.name else AnonymizeUtil.decryptWithNonce(mmi.machine.institutionPK, mmi.institution.name_real.get)
      val machId = if (mmi.machine.id_real.isEmpty) mmi.machine.id else AnonymizeUtil.decryptWithNonce(mmi.machine.institutionPK, mmi.machine.id_real.get)
      instName + " : " + machId
    }

    def mmiToTuple(mmi: MMI) = (mmiToMachPK(mmi), mmiToText(mmi))
    def sortMMI(a: MMI, b: MMI): Boolean = { mmiToText(a).compareTo(mmiToText(b)) < 0 }
    val userIsWhitLst = response.isDefined && userIsWhitelisted(response.get.getRequest)
    val instPK = if (response.isDefined) CachedUser.get(response.get.getRequest).get.institutionPK else -2
    def shouldShow(mmi: MMI): Boolean = {
      mmi.machine.serialNumber.isEmpty && (userIsWhitLst || (instPK == mmi.machine.institutionPK))
    }
    logger.info("userIsWhitLst: " + userIsWhitLst)
    val machList = ("-1", "None") +: Machine.listWithDependencies.filter(mmi => shouldShow(mmi)).sortWith(sortMMI).map(mmi => mmiToTuple(mmi))
    machList
  }

  def stringToUrlSafe(text: String): String = text.replaceAll("[^a-zA-Z0-9]", "_")

}

