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
import org.aqa.Crypto
import org.aqa.AnonymizeUtil

object UserUpdate {
  val userPKTag = "userPK"
}

class UserUpdate extends Restlet with SubUrlAdmin with Logging {
  private val pageTitleCreate = "Create User"

  private val pageTitleEdit = "Edit User"

  private val id = new WebInputText("Id", 2, 0, "User Id")

  private val fullName = new WebInputText("Name", 4, 0, "Full name of user, eg: Marie Curie (required)")

  private val email = new WebInputEmail("Email", 4, 0, "Email address (required)")

  def listInst(response: Option[Response]): Seq[(String, String)] = {
    if (userIsWhitelisted(response.get.getRequest)) {
      // Institution.list.toList.map(i => (i.institutionPK.get.toString, AnonymizeUtil.decryptWithNonce(i.institutionPK.get, i.name_real.get)))
      Institution.list.toList.map(i => (i.institutionPK.get.toString, i.name))
    } else {
      val user = CachedUser.get(response.get.getRequest)
      if (user.isDefined) {
        val institutionPK = user.get.institutionPK
        val inst = Institution.get(institutionPK).get
        Seq((institutionPK.toString, inst.name))
      } else
        Seq[(String, String)](); // if no user defined, then no access to institutions
    }

  }

  private val institution = new WebInputSelect("Institution", 2, 0, listInst, true)

  private val password = new WebInputPassword("Password", 4, 0, "")
  private val verifyPassword = new WebInputPassword("Verify Password", 4, 0, "")

  def listRole(response: Option[Response]) = UserRole.values.toList.filter(r => r != UserRole.publik).map(v => (v.toString, v.toString))
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
    } else styleNone
  }

  private def emptyName(valueMap: ValueMapT): StyleMapT = {
    val nameText = valueMap.get(fullName.label).get.trim
    val isEmpty = nameText.trim.size == 0
    if (isEmpty) {
      Error.make(fullName, "Name can not be empty")
      //formCreate.setFormResponse(Some(valueMap), Some(errMap), pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else styleNone
  }

  /**
   * Validate the email.
   *
   * An email must be provided.
   *
   * Should be of the form: user@url
   *
   */
  private def validateEmail(valueMap: ValueMapT): StyleMapT = {
    val emailText = email.getValOrEmpty(valueMap).trim

    if (emailText.matches("..*@..*")) styleNone
    else Error.make(email, "Invalid email format.  Must be of the form: user@url")

  }

  private def updateUser(user: User): Unit = {
    User.query.insertOrUpdate(user)
  }

  /**
   * Construct a user from parameters, changing only the fields that the user modified.
   */
  private def updateExistingUserFromParameters(user: User, valueMap: ValueMapT): User = {
    val institutionPK = valueMap.get(institution.label).get.toLong

    def decry(encrypted: String) = AnonymizeUtil.decryptWithNonce(institutionPK, encrypted)
    def encry(clearText: String) = AnonymizeUtil.encryptWithNonce(institutionPK, clearText)

    val user1 = {
      val newId = valueMap.get(id.label).get.trim
      if (decry(user.id_real.get).equals(newId))
        user
      else
        user.copy(id_real = Some(encry(newId)))
    }

    val user2 = {
      val newfullName = valueMap.get(fullName.label).get.trim
      if (decry(user.fullName_real).equals(newfullName))
        user1
      else
        user1.copy(fullName_real = encry(newfullName))
    }

    val user3 = {
      val newEmail = valueMap.get(email.label).get.trim
      if (decry(user.email_real).equals(newEmail))
        user2
      else
        user2.copy(email_real = encry(newEmail))
    }

    val roleText = valueMap.get(role.label).get

    val user4 = user3.copy(role = roleText)

    user4
  }

  /**
   * Construct a user from parameters
   */
  private def constructNewUserFromParameters(valueMap: ValueMapT): User = {
    val institutionPK = valueMap.get(institution.label).get.toLong
    val id_realText = AnonymizeUtil.encryptWithNonce(institutionPK, valueMap.get(id.label).get.trim)
    val fullName_realText = AnonymizeUtil.encryptWithNonce(institutionPK, valueMap.get(fullName.label).get.trim)
    val email_realText = AnonymizeUtil.encryptWithNonce(institutionPK, valueMap.get(email.label).get.trim)
    val passwordText = valueMap.get(password.label).get.trim

    val passwordSalt = Crypto.randomSecureHash
    val hashedPassword = AuthenticationVerifier.hashPassword(passwordText, passwordSalt)
    val roleText = valueMap.get(role.label).get
    new User(None, "unknown", Some(id_realText), fullName_realText, email_realText, institutionPK, hashedPassword, passwordSalt, roleText, None)
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
   * Check to see if the user is authorized to create a user in the given institution, and if so, return true.
   *
   * If not, then set the response.
   */
  private def checkAuthorization(response: Response, institutionPK: Long): Boolean = {
    val user = getUser(response.getRequest)
    val ok = user.isDefined &&
      ((user.get.institutionPK == institutionPK) || WebUtil.userIsWhitelisted(response.getRequest))

    if (!ok) {
      val content = {
        <div>
          <h2>You are not authorized to make this change.</h2>
          To make changes to users you must be a member of the same institution as that user.
        </div>
      }
      val text = WebUtil.wrapBody(content, "Not Authorized")
      WebUtil.setResponse(text, response, Status.CLIENT_ERROR_UNAUTHORIZED)
    }

    ok
  }

  /**
   * Save changes made to form.
   */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = emptyId(valueMap) ++ emptyName(valueMap) ++ validateEmail(valueMap)
    if (errMap.isEmpty) {
      val existingUser = User.get(valueMap.get(userPK.label).get.toLong).get
      val user = updateExistingUserFromParameters(existingUser, valueMap)
      if (checkAuthorization(response, user.institutionPK)) {
        // remove old credentials just in case the user role was changed.
        CachedUser.remove(user.id)
        user.insertOrUpdate
        UserList.redirect(response)
      }
    } else {
      formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
   * Call this when user has clicked create button.  If everything is ok, then create the new user,
   * otherwise show the same screen and communicate the error.
   */
  private def create(valueMap: ValueMapT, response: Response) = {
    val errMap = emptyName(valueMap) ++ validateEmail(valueMap) ++ validatePasswords(valueMap)

    if (errMap.isEmpty) {
      val user = constructNewUserFromParameters(valueMap)
      if (checkAuthorization(response, user.institutionPK)) {
        val userWithPk = user.insert
        val aliasId = AnonymizeUtil.aliasify(AnonymizeUtil.userAliasPrefixId, userWithPk.userPK.get)
        userWithPk.copy(id = aliasId).insertOrUpdate
        UserList.redirect(response)
      }
    } else {
      formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  private def edit(user: User, response: Response) = {

    def decrypt(text: String): String = AnonymizeUtil.decryptWithNonce(user.institutionPK, text)

    val valueMap = Map(
      (userPK.label, user.userPK.get.toString),
      (id.label, decrypt(user.id_real.get)),
      (fullName.label, decrypt(user.fullName_real)),
      (email.label, decrypt(user.email_real)),
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
        } else
          None
      } else
        None
    } catch {
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
      } else
        false
    } else
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
    } else
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
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
