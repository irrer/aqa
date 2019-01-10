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

object AnonymousTranslate {
  private val path = new String((new AnonymousTranslate).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

/**
 * Generate a json table that translates between alias id's and real id's.  The
 * contents of the table are based on the user's credentials, which is what ever
 * belongs to the user's institution.
 */
class AnonymousTranslate extends Restlet with SubUrlRoot with Logging {

  private val emptyTable = "[]"

  private case class Translate(institutionPK: Long, alias: String, real: String) {
    def toJson = "{ \"alias\": \"" + alias + "\", \"real\": \"" + AnonymizeUtil.decryptWithNonce(institutionPK, real) + "\" }"
  }

  private val emptyList = Seq[Translate]()

  private def getInstitution(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doInst(inst: Institution) = {
      val name = new Translate(inst.institutionPK.get, inst.name, inst.name_real.get)
      val url = new Translate(
        inst.institutionPK.get,
        AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasUrlPrefixId, inst.institutionPK.get), inst.url_real)
      val description = new Translate(
        inst.institutionPK.get,
        AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasDescriptionPrefixId, inst.institutionPK.get), inst.description_real)
      Seq(name, url, description)
    }

    val list = if (isWhitelisted) Institution.list else Seq(Institution.get(institutionPK)).flatten

    list.filter(inst => inst.name_real.isDefined).map(inst => doInst(inst)).flatten
  }

  private def getMachine(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {
    def doMach(mach: Machine) = {
      val name = new Translate(mach.institutionPK, mach.id, mach.id_real.get)
      val url = new Translate(mach.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mach.machinePK.get), mach.notes)
      Seq(name, url)
    }

    val list = if (isWhitelisted) Machine.list else Machine.listMachinesFromInstitution(institutionPK).filter(m => m.id_real.isDefined)
    list.map(mach => doMach(mach)).flatten
  }

  private def getUser(institutionPK: Long, isWhitelisted: Boolean): Seq[Translate] = {

    def doUser(user: User) = {
      val id = new Translate(user.institutionPK, user.id, user.id_real.get)
      val fullName = new Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasFullNamePrefixId, user.userPK.get), user.fullName_real)
      val email = new Translate(user.institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasEmailPrefixId, user.userPK.get), user.email_real)
      Seq(id, fullName, email)
    }

    val userList = if (isWhitelisted) User.list else User.listUsersFromInstitution(institutionPK).filter(m => m.id_real.isDefined)

    userList.map(user => doUser(user)).flatten
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {

      val jsonTable: String = WebUtil.getUser(request) match {
        case Some(user) => {
          val isWhitelisted = WebUtil.userIsWhitelisted(request)
          val list = getInstitution(user.institutionPK, isWhitelisted) ++ getMachine(user.institutionPK, isWhitelisted) ++ getUser(user.institutionPK, isWhitelisted)
          list.map(t => t.toJson).mkString("[\n", ",\n", "\n]\n")
        }
        case _ => emptyTable
      }

      response.setEntity(jsonTable, MediaType.APPLICATION_JSON)
      response.setStatus(Status.SUCCESS_OK)
      logger.info("request: " + request) // TODO

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
