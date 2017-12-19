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
import org.aqa.db.User
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
import org.aqa.db.Institution.InstitutionTable
import org.aqa.db.Institution
import org.aqa.db.UserRole
import org.aqa.Util
import org.aqa.db.CachedUser

object UserUpdate {
    val userPKTag = "userPK"
}

class UserUpdate extends Restlet with SubUrlAdmin with Logging {
    private val pageTitleCreate = "Create User"

    private val pageTitleEdit = "Edit User"

    private val id = new WebInputText("Id", 2, 0, "User Id")

    private val fullName = new WebInputText("Name", 4, 0, "Full name of user, eg: Marie Curie (required)")

    private val email = new WebInputEmail("Email", 4, 0, "Email address (required)")

    def listInst() = Institution.list.toList.map(i => (i.institutionPK.get.toString, i.name))

    private val institution = new WebInputSelect("Institution", 2, 0, listInst)

    private val password = new WebInputPassword("Password", 4, 0, "")
    private val verifyPassword = new WebInputPassword("Verify Password", 4, 0, "")

    def listRole() = UserRole.values.toList.filter(r => r != UserRole.publik).map(v => (v.toString, v.toString))
    private val role = new WebInputSelect("Role", 2, 0, listRole)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val changePasswordButton = makeButton("Change Password", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val userPK = new WebInputHidden(UserUpdate.userPKTag)

    private val formCreate = new WebForm(pathOf, List(List(id, fullName), List(email, institution), List(role), List(password), List(verifyPassword), List(createButton, cancelButton)))

    private val formEdit = new WebForm(pathOf, List(List(id, fullName), List(email, institution), List(role), List(saveButton, cancelButton, deleteButton, changePasswordButton, userPK)))

    private def redirect(response: Response, valueMap: ValueMapT) = {
        val pk = userPK.getValOrEmpty(valueMap)
        val suffix =
            if (pk.size > 0) { "?" + userPK.label + "=" + pk }
            else
                ""
        response.redirectSeeOther(pathOf + suffix)
    }

    private def emptyId(valueMap: ValueMapT): StyleMapT = {
        val idText = valueMap.get(id.label).get.trim
        val isEmpty = idText.trim.size == 0
        if (isEmpty) {
            Error.make(id, "Id can not be empty")
            //formCreate.setFormResponse(Some(valueMap), Some(errMap), pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
        else styleNone
    }

    private def emptyName(valueMap: ValueMapT): StyleMapT = {
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
    private def validateEmail(valueMap: ValueMapT): StyleMapT = {
        val emailText = email.getValOrEmpty(valueMap).trim
        val pk = userPK.getValOrEmpty(valueMap)
        val user = if (emailText.size > 0) User.findUserByEmail(emailText) else None

        val pkSame = user.isDefined && user.get.userPK.get.toString.equals(pk)
        val emailSame = user.isDefined && user.get.email.equalsIgnoreCase(emailText)

        0 match {
            case _ if (emailText.size == 0) => Error.make(email, "Name can not be empty")
            case _ if ((!pkSame) && emailSame) => Error.make(email, "That email is used by " + user.get.fullName)
            case _ => styleNone
        }
    }

    private def updateUser(user: User): Unit = {
        User.query.insertOrUpdate(user)
    }

    /**
     * Save changes made to form.
     */
    private def save(valueMap: ValueMapT, response: Response): Unit = {
        val errMap = emptyId(valueMap) ++ emptyName(valueMap) ++ validateEmail(valueMap)
        if (errMap.isEmpty) {
            val user = reconstructUserFromParameters(valueMap)
            // remove old credentials just in case the user role was changed.
            CachedUser.remove(user.id)
            user.insertOrUpdate
            UserList.redirect(response)
        }
        else {
            formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Create a new user
     */
    private def createUserFromParameters(valueMap: ValueMapT): User = {
        val idText = valueMap.get(id.label).get.trim
        val fullNameText = valueMap.get(fullName.label).get.trim
        val emailText = valueMap.get(email.label).get.trim
        val passwordText = valueMap.get(password.label).get.trim

        val institutionPK = valueMap.get(institution.label).get.toLong

        val passwordSalt = Util.randomSecureHash
        val hashedPassword = AuthenticationVerifier.hashPassword(passwordText, passwordSalt)
        val roleText = valueMap.get(role.label).get
        new User(None, idText, fullNameText, emailText, institutionPK, hashedPassword, passwordSalt, roleText, None)
    }

    /**
     * Get save parameters for a user
     */
    private def reconstructUserFromParameters(valueMap: ValueMapT): User = {
        val userPK = valueMap.get(UserUpdate.userPKTag).get.toLong
        val user = User.get(userPK).get

        val idText = valueMap.get(id.label).get.trim
        val fullNameText = valueMap.get(fullName.label).get.trim
        val emailText = valueMap.get(email.label).get.trim
        val institutionPK = valueMap.get(institution.label).get.toLong

        val passwordSalt = user.passwordSalt
        val hashedPassword = user.hashedPassword
        val roleText = valueMap.get(role.label).get
        new User(Some(userPK), idText, fullNameText, emailText, institutionPK, hashedPassword, passwordSalt, roleText, user.termsOfUseAcknowledgment)
    }

    /**
     * Show this when user asks to create a new user from user list.
     */
    private def emptyForm(response: Response) = {
        formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    private def validatePasswords(valueMap: ValueMapT): StyleMapT = {
        val err = AuthenticationVerifier.judgePassword(valueMap.get(password.label).get)
        if (err.isDefined) Error.make(password, err.get)
        else {
            if (valueMap.get(password.label).get.equals(valueMap.get(verifyPassword.label).get)) styleNone
            else Error.make(verifyPassword, "Passwords do not match")
        }
    }

    /**
     * Call this when user has clicked create button.  If everything is ok, then create the new user,
     * otherwise show the same screen and communicate the error.
     */
    private def create(valueMap: ValueMapT, response: Response) = {
        val errMap = emptyName(valueMap) ++ validateEmail(valueMap) ++ validatePasswords(valueMap)

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
            (id.label, user.id),
            (fullName.label, user.fullName),
            (email.label, user.email),
            (role.label, user.role),
            (institution.label, user.institutionPK.toString))
        formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    }

    private def getReference(valueMap: ValueMapT): Option[User] = {
        val value = valueMap.get(UserUpdate.userPKTag)
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

    private def isDelete(valueMap: ValueMapT, response: Response): Boolean = {
        if (buttonIs(valueMap, deleteButton)) {
            val value = valueMap.get(UserUpdate.userPKTag)
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

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    /**
     * Determine if the incoming request is to edit an existing user.
     */
    private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
        val user = getReference(valueMap)
        val value = valueMap.get(UserUpdate.userPKTag)
        if (user.isDefined) {
            edit(user.get, response)
            true
        }
        else
            false
    }

    private def changePassword(valueMap: ValueMapT, response: Response): Unit = {
        val pk = valueMap.get(userPK.label)
        if (pk.isDefined) response.redirectSeeOther(SetPassword.path + "?" + UserUpdate.userPKTag + "=" + pk.get)
        else {
            val msg = "No user specified to set password from UserUpdate"
            logger.warn(msg)
            internalFailure(response, msg)
        }
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => UserList.redirect(response)
                case _ if buttonIs(valueMap, changePasswordButton) => changePassword(valueMap, response)
                case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, response)
                case _ if isDelete(valueMap, response) => Nil
                case _ if isEdit(valueMap, response) => Nil
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, t)
            }
        }
    }
}
