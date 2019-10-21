package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import org.aqa.db.EPID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.AQA
import org.aqa.Config
import edu.umro.util.OpSys
import org.aqa.db.Institution
import org.aqa.AnonymizeUtil
import org.aqa.db.Machine
import org.aqa.db.User
import com.pixelmed.dicom.TagFromName
import org.aqa.db.DicomAnonymous

object AnonymousTranslate {
  private val path = new String((new AnonymousTranslate).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

  /**
   * List of special characters that, when constructing a JavaScript string, have to be properly escaped.  The
   * map is between the character in the original string and the corresponding string it must be replaced with.
   */
  private val specialCharMap = {
    List(
      ("\\", "\\\\"),
      ("\b", "\\b"),
      ("\f", "\\f"),
      ("\n", "\\n"),
      ("\r", "\\r"),
      ("\t", "\\t"),
      ("\"", "\\\"")).toMap
  }

  /**
   * Escape characters that are considered special characters in JavaScript for use in JavaScript strings.
   */
  def escapeSpecialJsChars(text: String): String = {
    def fix(c: String) = {
      if (specialCharMap.contains(c))
        specialCharMap(c)
      else
        c
    }
    val fixed = text.map(c => c.toString).map(c => fix(c)).mkString("")
    fixed
  }
}

/**
 * Generate a json or html table that translates between alias id's and real id's.  The
 * contents of the table are based on the user's credentials, which is what ever
 * belongs to the user's institution.
 */
class AnonymousTranslate extends Restlet with SubUrlRoot with Logging {

  private val emptyTable = "[]"

  private case class Translate(institutionPK: Long, alias: String, real: String, use: String) {
    def toJson: String = "{ \"alias\": \"" + alias + "\", \"real\": \"" + AnonymousTranslate.escapeSpecialJsChars(AnonymizeUtil.decryptWithNonce(institutionPK, real)) + "\" }"

    def toHtml: Elem = {
      val j = { <tr><td>{ use }</td><td>{ alias }</td><td>{ AnonymizeUtil.decryptWithNonce(institutionPK, real) }</td></tr> }
      j
    }
  }

  private val emptyList = Seq[Translate]()

  private def getInstitution(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doInst(inst: Institution) = {
      val name = new Translate(inst.institutionPK.get, inst.name, inst.name_real.get, "Institution Name")
      val url = new Translate(
        inst.institutionPK.get,
        AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasUrlPrefixId, inst.institutionPK.get), inst.url_real, "Institution URL")
      val description = new Translate(
        inst.institutionPK.get,
        AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasDescriptionPrefixId, inst.institutionPK.get), inst.description_real, "Institution Description")
      Seq(name, url, description)
    }

    val list = if (isWhitelisted) Institution.list else Seq(Institution.get(institutionPK)).flatten

    list.filter(inst => inst.name_real.isDefined).map(inst => doInst(inst)).flatten
  }

  private def getMachine(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    def doMach(mach: Machine) = {
      val name = new Translate(mach.institutionPK, mach.id, mach.id_real.get, "Machine ID")
      val url = new Translate(mach.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mach.machinePK.get), mach.notes, "Machine Notes")
      Seq(name, url)
    }

    val list = if (isWhitelisted) Machine.list else Machine.listMachinesFromInstitution(institutionPK).filter(m => m.id_real.isDefined)
    list.map(mach => doMach(mach)).flatten
  }

  private def getUser(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doUser(user: User) = {
      val id = new Translate(user.institutionPK, user.id, user.id_real.get, "User ID")
      val fullName = new Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasFullNamePrefixId, user.userPK.get), user.fullName_real, "User Name")
      val email = new Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasEmailPrefixId, user.userPK.get), user.email_real, "User Email")
      Seq(id, fullName, email)
    }

    val userList = if (isWhitelisted) User.list else User.listUsersFromInstitution(institutionPK).filter(m => m.id_real.isDefined)

    userList.map(user => doUser(user)).flatten
  }

  private val attributesToTranslate = Seq(TagFromName.PatientName, TagFromName.DeviceSerialNumber)

  private def getDicomAttributes(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    val dicomAnonList: Seq[DicomAnonymous] = {
      if (isWhitelisted) DicomAnonymous.getAttributesByTag(attributesToTranslate)
      else DicomAnonymous.getAttributesByTag(institutionPK, attributesToTranslate)
    }

    val instMap = Institution.list.map(inst => (inst.institutionPK.get, inst.name)).toMap

    dicomAnonList.map(da => new Translate(
      da.institutionPK,
      da.value,
      da.value_real,
      "DICOM Attr " + da.attributeTag))
  }

  private def getJson(list: Seq[Translate], response: Response) = {
    val jsonTable = list.map(t => t.toJson).mkString("[\n", ",\n", "\n]\n")
    response.setEntity(jsonTable, MediaType.APPLICATION_JSON)
    response.setStatus(Status.SUCCESS_OK)
  }

  private def getHtml(list: Seq[Translate], userId: String, response: Response) = {
    val content = {
      <div class="row col-md-10 col-md-offset-1">
        <h2>List of Aliases and Values Viewable by User <i>{ userId }</i></h2>
        <table class="table table-striped">
          <thead>
            <tr>
              <th>Used As</th>
              <th>Alias</th>
              <th>Value</th>
            </tr>
          </thead>
          { list.map(t => t.toHtml) }
        </table>
      </div>
    }

    val text = WebUtil.wrapBody(content, "Translation of Aliased Values")

    response.setEntity(text, MediaType.TEXT_HTML)
    response.setStatus(Status.SUCCESS_OK)
  }

  private def getTranslationList(request: Request): Seq[Translate] = {
    WebUtil.getUser(request) match {
      case Some(user) => {
        val isWhitelisted = WebUtil.userIsWhitelisted(request)
        getInstitution(user.institutionPK, isWhitelisted) ++
          getMachine(user.institutionPK, isWhitelisted) ++
          getUser(user.institutionPK, isWhitelisted) ++
          getDicomAttributes(user.institutionPK, isWhitelisted)
      }
      case _ => Seq[Translate]()
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    val userId = WebUtil.getUserIdOrDefault(request, "guest")
    try {
      val list = getTranslationList(request)
      if (valueMap.get("html").isDefined)
        getHtml(list, userId, response)
      else
        getJson(list, response)

    } catch {
      case t: Throwable => {
        //  WebUtil.internalFailure(response, t)
        logger.warn("Ignoring unexpected exception: " + fmtEx(t))
        response.setEntity(emptyTable, MediaType.APPLICATION_JSON)
        response.setStatus(Status.SUCCESS_OK)
      }
    }
  }
}
