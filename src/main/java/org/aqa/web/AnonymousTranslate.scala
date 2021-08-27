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

package org.aqa.web

import com.pixelmed.dicom.TagFromName
import org.aqa.AnonymizeUtil
import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.DicomAnonymous
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.User
import org.aqa.web.AnonymousTranslate.clearCache
import org.aqa.web.AnonymousTranslate.institutionPKofUser
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.util.Date
import scala.annotation.tailrec
import scala.xml.Elem

object AnonymousTranslate extends Logging {
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

  /** Cache entries older than this are considered stale. */
  private val cacheTimeout_ms: Int = 60 * 60 * 1000

  private val whiteListedInstitution: Long = -1
  private val unknownInstitution: Long = -2

  private def institutionPKofUser(userId: String): Long = {
    if (userIsWhitelisted(userId))
      whiteListedInstitution
    else {
      val user = CachedUser.get(userId)
      if (user.isDefined)
        user.get.institutionPK
      else unknownInstitution
    }
  }

  private case class TranslateCache(institutionPK: Long, date: Date, content: String) {
    def isValid: Boolean = {
      val timeout = date.getTime + cacheTimeout_ms
      timeout > System.currentTimeMillis()
    }
  }

  private val cache = scala.collection.mutable.Map[Long, TranslateCache]()

  /**
    * If there in an entry in the cache corresponding to the given institution, then
    * remove it.  This is called from external functions that are informing the cache
    * that content has changed (such as a machine id changed) which makes the cache
    * invalid.
    *
    * Also clear the whitelisted cache because it has also become invalid.
    *
    * @param institutionPK The institution whose content has changed.
    */
  def clearCache(institutionPK: Long): Unit = {
    logger.info("Clearing AnonymousTranslate cache for institution " + institutionPK)
    cache.synchronized {
      if (cache.contains(institutionPK)) {
        cache.remove(institutionPK)
      }
      if (cache.contains(whiteListedInstitution))
        cache.remove(whiteListedInstitution)
    }
  }

  /**
    * Put an entry in the cache.
    * @param userId Real user id.
    * @param content json content
    */
  private def putToCache(userId: String, content: String): Unit = {
    val institutionPK = institutionPKofUser(userId)
    cache.synchronized {
      cache.put(institutionPK, TranslateCache(institutionPK, new Date, content))
    }
  }

  /**
    * Attempt to get the content from cache.  It must be json and it must not be stale.
    * @param isHtml True if the user is asking for the content as HTML (not json)
    * @param userId Real id of user.
    * @param response HTML response.  Put content here if it is found in the cache.
    * @return
    */
  private def getFromCache(isHtml: Boolean, userId: String, response: Response): Boolean =
    cache.synchronized {

      @tailrec
      def clean(): Unit = {
        val expired = cache.values.find(tc => !tc.isValid)
        if (expired.isDefined) {
          cache.remove(expired.get.institutionPK)
          clean()
        }
      }

      clean()

      if (!isHtml) {
        val institutionPK = institutionPKofUser(userId)
        val tc = cache.get(institutionPK)
        if (tc.isDefined) {
          response.setEntity(tc.get.content, MediaType.TEXT_HTML)
          response.setStatus(Status.SUCCESS_OK)
          true
        } else
          false
      } else
        false

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

  private val attributesToTranslate = Seq(TagFromName.PatientName, TagFromName.PatientID, TagFromName.DeviceSerialNumber)

  private def getDicomAttributes(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    val dicomAnonList: Seq[DicomAnonymous] = {
      if (isWhitelisted) DicomAnonymous.getAttributesByTag(attributesToTranslate)
      else DicomAnonymous.getAttributesByTag(institutionPK, attributesToTranslate)
    }

    dicomAnonList.map(da => Translate(da.institutionPK, da.value, da.value_real, "DICOM Attr " + da.attributeTag))
  }

  private def putJson(list: Seq[Translate], userId: String, response: Response): Unit = {
    val jsonTable = list.map(t => t.toJson).mkString("[\n", ",\n", "\n]\n")
    response.setEntity(jsonTable, MediaType.APPLICATION_JSON)
    response.setStatus(Status.SUCCESS_OK)
    AnonymousTranslate.putToCache(userId, jsonTable)
  }

  private def putHtml(list: Seq[Translate], userId: String, response: Response): Unit = {
    // AnonymousTranslate.expireCache(userId)
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

    // Clear the cache here as a way to give the user some control over its freshness.
    clearCache(institutionPKofUser(userId))
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
    val start = System.currentTimeMillis()
    val valueMap = getValueMap(request)
    val isHtml = valueMap.contains("html")
    val userId = WebUtil.getUserIdOrDefault(request, "guest")

    // Attempt to get content from cache because it is faster.
    if (!AnonymousTranslate.getFromCache(isHtml, userId, response)) {
      try {
        val list = getTranslationList(request)
        if (isHtml)
          putHtml(list, userId, response)
        else
          putJson(list, userId, response)

      } catch {
        case t: Throwable =>
          //  WebUtil.internalFailure(response, t)
          logger.warn("Ignoring unexpected exception: " + fmtEx(t))
          response.setEntity(emptyTable, MediaType.APPLICATION_JSON)
          response.setStatus(Status.SUCCESS_OK)
      }
    }
    logger.info("AnonymousTranslate table retrieval time in ms: " + (System.currentTimeMillis() - start))
  }
}
