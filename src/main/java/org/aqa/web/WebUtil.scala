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
import org.aqa.Logging._
import org.restlet.data.Method
import java.io.File
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.restlet.ext.fileupload.RestletFileUpload
import java.io.InputStream
import java.io.FileOutputStream
import java.lang.Class
import edu.umro.ScalaUtil.Trace._
import org.aqa.db.User

object WebUtil {

    val bs = "\n.bs-example{ margin: 20px; }" // .bs-example{ margin: 20px;  }

    val spacer = "\n.spacer {  margin-top: 40px; }"

    private val singleQuote = "@@quote1@@"

    private val doubleQuote = "@@quote2@@"

    def snglQuote(text: String): String = singleQuote + text + singleQuote

    def dblQuote(text: String): String = doubleQuote + text + doubleQuote

    def xmlToText(document: Node): String = new PrettyPrinter(1024, 2).format(document).replace(singleQuote, "'").replace(doubleQuote, "\"")

    def cleanClassName(className: String) = className.substring(className.lastIndexOf('.') + 1).replace("$", "")

    def pathOf(subUrl: SubUrl.Value, className: String) = "/" + subUrl + "/" + cleanClassName(className)

    /**
     * Extract name/value parameters from request.
     * def getValueMap(request: Request, dir: Option[File]): ValueMapT = {
     * val objList = (new Form(request.getEntity)).toArray().toList
     * val formMap = objList.filter(o => o.isInstanceOf[Parameter]).map(o => o.asInstanceOf[Parameter]).map(p => (p.getName, p.getValue)).toMap
     *
     * val getList: List[Option[(String, String)]] = request.getResourceRef.getQueryAsForm.toArray.toList.map(s => s match {
     * case s: Parameter => Some((s.getName, s.getValue))
     * case _ => None;
     * })
     * val getMap = getList.filter(p1 => p1.isDefined).map(p2 => p2.get).toMap
     *
     * val all = formMap ++ getMap
     * all
     * }
     */

    /**
     * Write the input stream to the file.
     */
    private def saveFile(inputStream: InputStream, file: File) = {
        file.getParentFile.mkdirs
        file.delete
        file.createNewFile
        val outputStream = new FileOutputStream(file)

        val buffer = Array.ofDim[Byte](16 * 1024)
        var size = inputStream.read(buffer)
        while (size != -1) {
            outputStream.write(buffer, 0, size)
            size = inputStream.read(buffer)
        }
        outputStream.flush
        outputStream.close
    }

    def sessionDir(valueMap: ValueMapT): File = {
        val sessionId = valueMap.get(sessionLabel).get
        Session.idToFile(sessionId)
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

        val dir = sessionDir(valueMap)
        val upload = new RestletFileUpload(new DiskFileItemFactory(500, dir)) // TODO change size to -1 ?
        val itemIterator = upload.getItemIterator(request.getEntity);

        while (itemIterator.hasNext) {
            val ii = itemIterator.next
            if (!ii.isFormField) saveFile(ii.openStream, new File(dir, ii.getName))
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
        val formMap = paramList.map(p => (p.getName, p.getValue)).toMap
        ensureSessionId(formMap)
    }

    /**
     * Return true if the request is an upload
     */
    def requestIsUpload(request: Request): Boolean = {
        val methodIsUpload = (request.getMethod == Method.POST) || (request.getMethod == Method.PUT)
        def entity = request.getEntity
        def mediaType = request.getEntity.getMediaType
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
            parseOriginalReference(request) ++ parseForm(new Form(request.getEntity))
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

    def internalFailure(response: Response, message: String) = {
        val status = Status.SERVER_ERROR_INTERNAL
        val content = {
            <div>{ status.toString } <p/> { message }</div>
        }
        logWarning(Status.SERVER_ERROR_INTERNAL.toString + " shown to user " + getUserIdOrDefault(response.getRequest, "unknown") + " : " + message)
        simpleWebPage(content, status, "Not Found", response)
    }

    val HTML_PREFIX = "<!DOCTYPE html>\n"

    def wrapBody(content: Elem, pageTitle: String): String = {
        val page = {
            <html lang="en">
                <head>
                    <title>{ pageTitle }</title>
                    <link rel="stylesheet" href="/static/bootstrap/3.3.6/css/bootstrap.min.css"/>
                    <link rel="stylesheet" href="/static/bootstrap/3.3.6/css/bootstrap-theme.min.css"/>
                    <script src="/static/jquery/1.11.3/jquery.min.js"></script>
                    <script src="/static/bootstrap/3.3.6/js/bootstrap.min.js"></script>
                    <script src="/static/dropzone/dropzone-4.3.0/dist/dropzone.js"></script>
                    <link rel="stylesheet" href="/static/dropzone/dropzone-4.3.0/dist/dropzone.css"/>
                    <script src="/static/ReloadOutput.js"></script>
                </head>
                <body>
                    <header class="topbar">
                        <div class="row">
                            <div class="col-md-2 col-md-offset-9">
                                <h1 class="fill">
                                    <a href="/"><img src="/static/images/logo.png" width="128"/></a>
                                </h1>
                            </div>
                        </div>
                    </header>
                    { content }
                </body>
            </html>
        }

        val text = HTML_PREFIX + xmlToText(page)

        logFine("HTML delivered:\n" + text)
        text
    }

    def setResponse(text: String, response: Response, status: Status): Unit = {
        response.setStatus(status)
        response.setEntity(text, MediaType.TEXT_HTML)
    }

    def respond(content: Elem, pageTitle: String, response: Response, status: Status): Unit = {
        setResponse(wrapBody(content, pageTitle), response, status)
    }

    def respond(content: Elem, title: String, response: Response): Unit = respond(content, title, response, Status.SUCCESS_OK)

    trait ToHtml {
        def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = ???
        def toHtml(valueMap: ValueMapT): Elem = toHtml(valueMap, styleNone)
        def toHtml: Elem = toHtml(emptyValueMap, styleNone)
    }

    implicit class WebRow(val colList: List[ToHtml]) extends ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            <div class="form-group">
                <div class="row">
                    { colList.map(c => c.toHtml(valueMap, errorMap)) }
                </div>
            </div>
        }
    }

    implicit def ColList(col: ToHtml): List[ToHtml] = List(col);

    def markLiteralValue(label: String): String = "@@@@" + label + "@@@@"

    val sessionLabel = "session"

    class WebForm(action: String, rowList: List[WebRow], fileUpload: Int) extends ToHtml {

        def this(action: String, rowList: List[WebRow]) = this(action, rowList, 0)

        val rowListWithSession = (new WebInputSession) ++ rowList

        val uploadFileInput: Option[IsInput] = if (validCol(fileUpload)) Some(new IsInput("uploadFile")) else None

        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {

            val valueMapWithSession = if (valueMap.get(sessionLabel).isDefined) valueMap else (Map((sessionLabel, Session.makeUniqueId)) ++ valueMap)

            val mainForm =
                <form action={ action } method={ Method.POST.toString } class="form-horizontal" role="form">
                    { rowListWithSession.map(r => r.toHtml(valueMapWithSession, errorMap)) }
                </form>;

            val html = {
                if (validCol(fileUpload)) {
                    val sessionId: String = valueMapWithSession.get(sessionLabel).get
                    val formClass = "dropzone row " + colToName(fileUpload, 0)

                    <div class="row">
                        <form action={ action + "?" + sessionLabel + "=" + sessionId } class={ formClass } id="uploadFile" style="border-color: #cccccc; border-width: 1px; border-radius: 10px;"></form>
                    </div>
                    <div class="row">
                        { mainForm }
                    </div>
                }
                else mainForm
            }

            <div class={ "row " + colToName(10, 1) }>
                { html }
            </div>
        }

        def setFormResponse(valueMap: ValueMapT, errorMap: StyleMapT, pageTitle: String, response: Response, status: Status): Unit = {
            val text = wrapBody(toHtml(valueMap, errorMap), pageTitle)
            def replace(origText: String, col: Any): String = {
                if (col.isInstanceOf[IsInput]) {
                    val input = col.asInstanceOf[IsInput]
                    val markedLiteral = markLiteralValue(input.label)
                    if (valueMap.get(input.label).isDefined && text.contains(markedLiteral)) {
                        origText.replace(markedLiteral, valueMap.get(input.label).get)
                    }
                    else origText
                }
                else origText
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
    }

    class Error(inputTitle: String) extends Style {
        override def divAttributes(metaData: MetaData): MetaData = {
            val clss = metaData.get("class")
            val clssText = (if (clss.isDefined) clss.get.toString + " " else "") + "has-error"
            val attr = ((<x/> % metaData) % (<x class={ clssText } title={ inputTitle }/>.attributes)).attributes
            attr
        }
    }

    object Error {
        def make(input: IsInput, inputTitle: String) = Map((input.label, new Error(inputTitle)))
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

    class IsInput(val label: String) {
        def getValOrEmpty(valueMap: ValueMapT): String = {
            val v = valueMap.get(label)
            if (v.isDefined) {
                if (v.get == null) ""
                else v.get
            }
            else ""
        }
    }

    class WebInputText(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val html = <input type="text"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
            wrapInput(label, true, html, col, offset, errorMap)
        }
    }

    /**
     * Show text that the user can not change.
     */
    class WebPlainText(override val label: String, val showLabel: Boolean, col: Int, offset: Int, html: (Any) => Elem) extends IsInput(label) with ToHtml {
        def this(label: String, col: Int, offset: Int, content: String) = this(label, true, col, offset, ((Null)) => <div>{ content }</div>)
        def this(label: String, col: Int, offset: Int, content: Elem) = this(label, true, col, offset, ((Null)) => content)
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            wrapInput(label, showLabel, html(valueMap), col, offset, errorMap)
        }
    }

    class WebInputURL(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val html = <input type="url"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
            wrapInput(label, true, html, col, offset, errorMap)
        }
    }

    class WebInputEmail(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val html = <input type="email"/> % idNameClassValueAsAttr(label, valueMap) % placeholderAsAttr(placeholder)
            wrapInput(label, true, html, col, offset, errorMap)
        }
    }

    class WebInputPassword(override val label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val html = <input type="password"/> % idNameClassAsAttr(label) % placeholderAsAttr(placeholder)
            wrapInput(label, true, html, col, offset, errorMap)
        }
    }

    class WebInputSelect(override val label: String, col: Int, offset: Int, selectList: () => List[(String, String)]) extends IsInput(label) with ToHtml {

        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {

            val curValue: Option[String] = if (valueMap.get(label).isDefined) valueMap.get(label) else None

            def toOption(value: String, text: String): Elem = {
                if (curValue.isDefined && curValue.get.equals(value))
                    <option selected="selected" value={ value }>{ text }</option>
                else
                    <option value={ value }>{ text }</option>
            }

            val list = selectList().map(v => toOption(v._1, v._2))
            val html = <select>{ list }</select> % idNameClassValueAsAttr(label, valueMap)
            wrapInput(label, true, html, col, offset, errorMap)
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

        def url(subUrl: SubUrl.Value, name: String): String = {
            ("/" + subUrl + "/" + name).replace("//", "/")
        }
    }

    trait SubUrlTrait {
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
    class FormButton(override val label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: String, buttonType: ButtonType.Value) extends IsInput(label) with ToHtml {
        def this(label: String, col: Int, offset: Int, subUrl: SubUrl.Value, action: String) = this(label, col, offset, subUrl, action: String, ButtonType.BtnDefault)
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val button = { <button type="submit" class={ "btn " + buttonType.toString } action={ action } value={ label } name={ label }>{ label }</button> }
            wrapInput(label, false, button, col, offset, errorMap)
        }
    }

    class WebInputTextArea(label: String, col: Int, offset: Int, placeholder: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val value = valueMap.get(label)
            val common = { <input class="form-control" id={ label } name={ label }/> }

            val html = // must not allow embedded blanks
                <textarea rows="3">{ if (value.isDefined) markLiteralValue(label) else "" }</textarea> % idNameClassAsAttr(label) % placeholderAsAttr(placeholder)

            wrapInput(label, true, html, col, offset, errorMap)
        }
    }

    class WebInputHidden(override val label: String) extends IsInput(label) with ToHtml {
        override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT): Elem = {
            val html = <input type="text" class="hidden"/> % idNameClassValueAsAttr(label, valueMap)
            <span class="hidden">{ html }</span>
        }
    }

    private class WebInputSession extends WebInputHidden(sessionLabel) with ToHtml;

    /**
     * Given a request, extract the user from it.
     */
    def getUser(request: Request): Option[User] = {
        val cr = request.getChallengeResponse
        if (cr == null) None
        else User.getUserById(cr.getIdentifier)
    }

    def getUserIdOrDefault(request: Request, dflt: String): String = {
        val cr = request.getChallengeResponse
        if (cr == null) dflt else cr.getIdentifier
    }

    def main(args: Array[String]): Unit = {

        val empty = <a></a>;
        val noAttr = empty.attributes
        println("noAttr: " + noAttr)

        val one = <a foo='bar' goo='gar'></a>;
        val oneAttr = one.attributes
        println("oneAttr: " + oneAttr)

        val x0 = (<aa></aa>) % noAttr
        val x1 = (<aa></aa>) % oneAttr

        def show(key: String, met: MetaData): Unit = {
            val v = met.get(key)
            val text = if (v.isDefined) v.get.toString else "None"
            println(key + " : " + text)
        }

        show("foo", oneAttr)

        val md = oneAttr.remove("foo")
        val newFoo = <a foo='LOO'/>.attributes
        val one2 = <a/> % newFoo
        show("foo", one2.attributes)

        val newFoo3 = <a foo='ReplaceMe'/>
        show("foo", newFoo3.attributes)
        val one3 = newFoo3 % (<a foo='RRROOO'/>.attributes)
        show("foo", one3.attributes)

        /*
        println("x0: " + xmlToText(x0))
        println("x1: " + xmlToText(x1))

        val b = ButtonType.BtnInfo
        println("b.toString: " + b.toString)
        val text = "11\n55\n90890890"
        val stuff = <aa>{ text }</aa>
        println(xmlToText(stuff))
        */
    }
}

