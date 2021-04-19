package org.aqa.web

import com.pixelmed.dicom.TagFromName
import org.aqa.AnonymizeUtil
import org.aqa.Logging
import org.aqa.db.DicomAnonymous
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.User
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import scala.xml.Elem

object AnonymousTranslate {
  private val path = new String((new AnonymousTranslate).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)

  /**
    * List of special characters that, when constructing a JavaScript string, have to be properly escaped.  The
    * map is between the character in the original string and the corresponding string it must be replaced with.
    */
  private val specialCharMap = {
    List(("\\", "\\\\"), ("\b", "\\b"), ("\f", "\\f"), ("\n", "\\n"), ("\r", "\\r"), ("\t", "\\t"), ("\"", "\\\"")).toMap
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
      val e = {
        <tr>
          <td>{alias}</td>
          <td>{AnonymizeUtil.decryptWithNonce(institutionPK, real)}</td>
          <td>{use}</td>
        </tr>
      }
      e
    }
  }

  private def getInstitution(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doInst(inst: Institution) = {
      val name = Translate(inst.institutionPK.get, inst.name, inst.name_real.get, "Institution Name")
      val url = Translate(inst.institutionPK.get, AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasUrlPrefixId, inst.institutionPK.get), inst.url_real, "Institution URL")
      val description =
        Translate(
          inst.institutionPK.get,
          AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasDescriptionPrefixId, inst.institutionPK.get),
          inst.description_real,
          "Institution Description"
        )
      Seq(name, url, description)
    }

    val list = if (isWhitelisted) Institution.list else Seq(Institution.get(institutionPK)).flatten

    list.filter(inst => inst.name_real.isDefined).flatMap(inst => doInst(inst))
  }

  private def getMachine(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    def doMach(mach: Machine) = {
      val name = Translate(mach.institutionPK, mach.id, mach.id_real.get, "Machine ID")
      val url = Translate(mach.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mach.machinePK.get), mach.notes, "Machine Notes")
      Seq(name, url)
    }

    val list = if (isWhitelisted) Machine.list else Machine.listMachinesFromInstitution(institutionPK).filter(m => m.id_real.isDefined)
    list.flatMap(mach => doMach(mach))
  }

  private def getUser(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doUser(user: User) = {
      val id = Translate(user.institutionPK, user.id, user.id_real.get, "User ID")
      val fullName = Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasFullNamePrefixId, user.userPK.get), user.fullName_real, "User Name")
      val email = Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasEmailPrefixId, user.userPK.get), user.email_real, "User Email")
      Seq(id, fullName, email)
    }

    val userList = if (isWhitelisted) User.list else User.listUsersFromInstitution(institutionPK).filter(m => m.id_real.isDefined)

    userList.flatMap(user => doUser(user))
  }

  private val attributesToTranslate = Seq(TagFromName.PatientName, TagFromName.DeviceSerialNumber)

  private def getDicomAttributes(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    val dicomAnonList: Seq[DicomAnonymous] = {
      if (isWhitelisted) DicomAnonymous.getAttributesByTag(attributesToTranslate)
      else DicomAnonymous.getAttributesByTag(institutionPK, attributesToTranslate)
    }

    dicomAnonList.map(da => Translate(da.institutionPK, da.value, da.value_real, "DICOM Attr " + da.attributeTag))
  }

  private def putJson(list: Seq[Translate], response: Response): Unit = {
    val jsonTable = list.map(t => t.toJson).mkString("[\n", ",\n", "\n]\n")
    response.setEntity(jsonTable, MediaType.APPLICATION_JSON)
    response.setStatus(Status.SUCCESS_OK)
  }

  private def putHtml(list: Seq[Translate], userId: String, response: Response): Unit = {
    val content = {
      <div class="row col-md-10 col-md-offset-1">
        <h2>List of Aliases and Values Viewable by User <i>{userId}</i></h2>
        <table class="table table-striped">
          <thead>
            <tr>
              <th>Alias</th>
              <th>Value</th>
              <th>Used As</th>
            </tr>
          </thead>
          {list.map(t => t.toHtml)}
        </table>
      </div>
    }

    val text = WebUtil.wrapBody(content, "Translation of Aliased Values")

    response.setEntity(text, MediaType.TEXT_HTML)
    response.setStatus(Status.SUCCESS_OK)
  }

  private def getTranslationList(request: Request): Seq[Translate] = {
    WebUtil.getUser(request) match {
      case Some(user) =>
        val isWhitelisted = WebUtil.userIsWhitelisted(request)
        getInstitution(user.institutionPK, isWhitelisted) ++
          getMachine(user.institutionPK, isWhitelisted) ++
          getUser(user.institutionPK, isWhitelisted) ++
          getDicomAttributes(user.institutionPK, isWhitelisted)
      case _ => Seq[Translate]()
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    val userId = WebUtil.getUserIdOrDefault(request, "guest")
    try {
      val list = getTranslationList(request)
      if (valueMap.contains("html"))
        putHtml(list, userId, response)
      else
        putJson(list, response)

    } catch {
      case t: Throwable =>
        //  WebUtil.internalFailure(response, t)
        logger.warn("Ignoring unexpected exception: " + fmtEx(t))
        response.setEntity(emptyTable, MediaType.APPLICATION_JSON)
        response.setStatus(Status.SUCCESS_OK)
    }
  }
}
