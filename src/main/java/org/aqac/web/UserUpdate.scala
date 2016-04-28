package org.aqac.web

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
import org.aqac.db.User
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
import org.aqac.Logging._
import org.aqac.db.Institution.InstitutionTable
import org.aqac.db.Institution

class UserUpdate extends Restlet {

    val userPKTag = "userPK"

    private val path = WebUtil.pathOf(this.getClass.getName)

    private val pageTitleCreate = "Create User"

    private val pageTitleEdit = "Edit User"

    private val fullName = new WebInputText("Name", 6, 0, "Full name of user, eg: Marie Curie (required)")

    private val email = new WebInputEmail("Email", 6, 0, "Email address (required)")

    def listInst() = Institution.list.toList.map(i => (i.institutionPK.get.toString, i.name))

    private val institution = new WebInputSelect("Institution", 6, 0, listInst)

    private val password = new WebInputPassword("Password", 6, 0, "")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, path, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val userPK = new WebInputHidden(userPKTag)

    private val formCreate = new WebForm(path, List(List(fullName), List(email), List(institution), List(password), List(createButton, cancelButton)))

    private val formEdit = new WebForm(path, List(List(fullName), List(email), List(institution), List(password), List(saveButton, cancelButton, deleteButton, userPK)))

    private def redirect(response: Response, valueMap: Map[String, String]) = {
        val pk = userPK.getValOrEmpty(valueMap)
        val suffix =
            if (pk.size > 0) { "?" + userPK.label + "=" + pk }
            else
                ""
        response.redirectSeeOther(path + suffix)
    }

    private def emptyName(valueMap: Map[String, String]): Map[String, Style] = {
        val nameText = valueMap.get(fullName.label).get.trim
        val isEmpty = nameText.trim.size == 0
        if (isEmpty) {
            Error.make(fullName, "Name can not be empty")
            //formCreate.setFormResponse(Some(valueMap), Some(errMap), pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
        else styleNone
    }

    /**
     * Validate the email.
     *
     * An email must be provided.
     *
     * Multiple users may not have the same email (ignore case, white space trimmed)
     */
    private def validateEmail(valueMap: Map[String, String]): Map[String, Style] = {
        val emailText = email.getValOrEmpty(valueMap).trim
        val pk = userPK.getValOrEmpty(valueMap)
        val user = if (emailText.size > 0) User.findUserByEmail(emailText) else None

        val pkSame = user.isDefined && user.get.userPK.get.toString.equals(pk)
        val emailSame = user.isDefined && user.get.email.equalsIgnoreCase(emailText)

        0 match {
            case _ if (emailText.size == 0) => Error.make(email, "Name can not be empty")
            case _ if ((!pkSame) && emailSame) => Error.make(email, "That email is used by " + user.get.fullName)
            case _ => Map[String, Style]()
        }
    }

    private def updateUser(user: User): Unit = {
        User.query.insertOrUpdate(user)
    }

    /**
     * Save changes made to form.
     */
    private def save(valueMap: Map[String, String], response: Response): Unit = {
        val errMap = emptyName(valueMap) ++ validateEmail(valueMap)
        if (errMap.isEmpty) {
            (createUserFromParameters(valueMap)).insertOrUpdate
            UserList.redirect(response)
        }
        else {
            formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Create a new user
     */
    private def createUserFromParameters(valueMap: Map[String, String]): User = {
        val userPK: Option[Long] = {
            val e = valueMap.get(userPKTag)
            if (e.isDefined) Some(e.get.toLong) else None
        }
        val fullNameText = valueMap.get(fullName.label).get.trim
        val emailText = valueMap.get(email.label).get.trim
        val passwordText = valueMap.get(password.label).get.trim

        val institutionPK = valueMap.get(institution.label).get.toLong

        val hashedPassword = "hashedPassword" // TODO crypto stuff for passwordText
        val passwordSalt = "passwordSalt" // TODO crypto stuff for passwordText
        new User(userPK, fullNameText, emailText, institutionPK, hashedPassword, passwordSalt)
    }

    /**
     * Show this when user asks to create a new user from user list.
     */
    private def emptyForm(response: Response) = {
        formCreate.setFormResponse(Map[String, String](), styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    /**
     * Call this when user has clicked create button.  If everything is ok, then create the new user,
     * otherwise show the same screen and communicate the error.
     */
    private def create(valueMap: Map[String, String], response: Response) = {
        val errMap = emptyName(valueMap) ++ validateEmail(valueMap)

        if (errMap.isEmpty) {
            val user = createUserFromParameters(valueMap)
            user.insert
            UserList.redirect(response)
        }
        else {
            formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    private def edit(user: User, response: Response) = {
        val valueMap = Map(
            (userPK.label, user.userPK.get.toString),
            (fullName.label, user.fullName),
            (email.label, user.email),
            (institution.label, Institution.get(user.institutionPK).get.name))
        formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    }

    private def getReference(valueMap: Map[String, String]): Option[User] = {
        val value = valueMap.get(userPKTag)
        try {
            if (value.isDefined) {
                val user = User.get(value.get.toLong)
                if (user.isDefined) {
                    user
                }
                else
                    None
            }
            else
                None
        }
        catch {
            case _: Throwable => None
        }
    }

    private def isDelete(valueMap: Map[String, String], response: Response): Boolean = {
        if (buttonIs(valueMap, deleteButton)) {
            val value = valueMap.get(userPKTag)
            if (value.isDefined) {
                User.delete(value.get.toLong)
                UserList.redirect(response)
                true
            }
            else
                false
        }
        else
            false

    }

    private def buttonIs(valueMap: Map[String, String], button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    /**
     * Determine if the incoming request is to edit an existing user.
     */
    private def isEdit(valueMap: Map[String, String], response: Response): Boolean = {
        val user = getReference(valueMap)
        val value = valueMap.get(userPKTag)
        if (user.isDefined) {
            edit(user.get, response)
            true
        }
        else
            false
    }

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => UserList.redirect(response)
                case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, response)
                case _ if isDelete(valueMap, response) => Nil
                case _ if isEdit(valueMap, response) => Nil
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }
}