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

  private def getInstitution(institutionPK: Long): Seq[Translate] = {
    Institution.get(institutionPK) match {
      case Some(inst) => {
        if (inst.name_real.isDefined) {
          val name = new Translate(institutionPK, inst.name, inst.name_real.get)
          val url = new Translate(
            institutionPK,
            AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasUrlPrefixId, inst.institutionPK.get), inst.url_real)
          val description = new Translate(
            institutionPK,
            AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasDescriptionPrefixId, inst.institutionPK.get), inst.description_real)
          Seq(name, url, description)
        } else emptyList
      }
      case _ => emptyList
    }
  }

  private def getMachine(institutionPK: Long): Seq[Translate] = {
    def doMach(mach: Machine) = {
      val name = new Translate(institutionPK, mach.id, mach.id_real.get)
      val url = new Translate(institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mach.machinePK.get), mach.notes)
      Seq(name, url)
    }
    Machine.listMachinesFromInstitution(institutionPK).filter(m => m.id_real.isDefined).map(mach => doMach(mach)).flatten
  }

  private def getUser(institutionPK: Long): Seq[Translate] = {

    def doUser(user: User) = {
      val id = new Translate(institutionPK, user.id, user.id_real.get)
      val fullName = new Translate(institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasFullNamePrefixId, user.userPK.get), user.fullName_real)
      val email = new Translate(institutionPK, AnonymizeUtil.aliasify(AnonymizeUtil.userAliasEmailPrefixId, user.userPK.get), user.email_real)
      Seq(id, fullName, email)
    }

    User.listUsersFromInstitution(institutionPK).filter(m => m.id_real.isDefined).map(user => doUser(user)).flatten
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {

      val jsonTable: String = WebUtil.getUser(request) match {
        case Some(user) => {
          val list =
            getInstitution(user.institutionPK) ++ getMachine(user.institutionPK) ++ getUser(user.institutionPK)
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
