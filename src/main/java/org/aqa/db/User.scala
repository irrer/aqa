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

package org.aqa.db

import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Crypto
import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.web.AnonymousTranslate

import java.sql.Timestamp

case class User(
    userPK: Option[Long], // primary key
    id: String, // alias login name
    id_real: Option[String], // alias login name, encrypted
    fullName_real: String, // full name, as in 'Louis Pasteur', encrypted
    email_real: String, // email for contacting, encrypted
    institutionPK: Long, // institution that this user belongs to
    hashedPassword: String, // cryptographically hashed password
    passwordSalt: String, // salt used for hashing password
    role: String, // user role which defines authorization
    termsOfUseAcknowledgment: Option[Timestamp] // time at which user agreed to the legal terms of the service, or 'None' if they never did.
) {

  def insert: User = {
    val insertQuery = User.query returning User.query.map(_.userPK) into ((user, userPK) => user.copy(userPK = Some(userPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    AnonymousTranslate.clearCache(institutionPK)
    result
  }

  def insertOrUpdate(): Int = {
    val count = Db.run(User.query.insertOrUpdate(this))
    AnonymousTranslate.clearCache(institutionPK)
    count
  }

  /**
    * Update the termsOfUseAcknowledgment, possibly to None.  Returns number of records updated, which
    * should always be one.  If it is zero then it is probably because the object is not in the database.
    */
  def updateTermsOfUseAcknowledgment(timestamp: Option[Timestamp]): Int = {
    Db.run(User.query.filter(_.userPK === userPK.get).map(u => u.termsOfUseAcknowledgment).update(timestamp))
  }

  def getRole: Option[UserRole.Value] = UserRole.stringToUserRole(role)

  override def toString: String = {
    def fmt(text: String) = text.take(6) + "..."
    "userPK: " + (if (userPK.isDefined) userPK.get else "None") +
      "  id: " + id +
      "  id_real: " + (if (id_real.isDefined) fmt(id_real.get) else "None") +
      "  fullName_real: " + fmt(fullName_real) +
      "  email_real: " + fmt(email_real) +
      "  instPK: " + institutionPK +
      "  hashedPswd: " + fmt(hashedPassword) +
      "  pswdSalt: " + fmt(passwordSalt) +
      "  role: " + role +
      "  useAck: " + (if (termsOfUseAcknowledgment.isDefined) termsOfUseAcknowledgment.get else "None")
  }

  /**
    * Determine if this user is an admin for their respective institution
    */
  def isAdmin: Boolean = {
    id_real.isDefined && AnonymizeUtil.decryptWithNonce(institutionPK, id_real.get).toLowerCase.contains(User.adminIndicator)
  }

  def getRealId: Option[String] = {
    if (id_real.isDefined)
      Some(AnonymizeUtil.decryptWithNonce(institutionPK, id_real.get))
    else
      None
  }
}

object User extends Logging {

  val adminIndicator = "admin"

  class UserTable(tag: Tag) extends Table[User](tag, "user") {

    def userPK = column[Long]("userPK", O.PrimaryKey, O.AutoInc)
    def id = column[String]("id") // TODO add O.Unique constraint
    def id_real = column[Option[String]]("id_real")
    def fullName_real = column[String]("fullName_real")
    def email_real = column[String]("email_real")
    def institutionPK = column[Long]("institutionPK")
    def hashedPassword = column[String]("hashedPassword")
    def passwordSalt = column[String]("passwordSalt")
    def role = column[String]("role")
    def termsOfUseAcknowledgment = column[Option[Timestamp]]("termsOfUseAcknowledgment")

    def * = (userPK.?, id, id_real, fullName_real, email_real, institutionPK, hashedPassword, passwordSalt, role, termsOfUseAcknowledgment) <> (User.apply _ tupled, User.unapply _)

    def institutionFK = foreignKey("User_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[UserTable]

  def get(userPK: Long): Option[User] = {
    val action = for {
      user <- query if user.userPK === userPK
    } yield user
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get the of user with the given id.  Comparison is case insensitive.
    */
  def getUserById(idRaw: String): Option[User] = {
    val id = idRaw.trim.toLowerCase
    val action = query.filter(_.id.toLowerCase === id)
    val seq = Db.run(action.result)
    logger.info("getUserById search for user with id " + idRaw + " found " + seq.size + " matches.")
    seq.headOption
  }

  /**
    * Construct a user from parameters.
    *
    * @param institutionPK Institution PK
    *
    * @param id user id (not encrypted)
    *
    * @param fullName user's full name (not encrypted)
    *
    * @param email user's email (not encrypted)
    *
    * @param passwordText user's email (not encrypted)
    *
    * @param roleText User role (admin, guest, etc) as text
    */
  def insertNewUser(institutionPK: Long, id: String, fullName: String, email: String, passwordText: String, roleText: String): User = {
    val passwordSalt = Crypto.randomSecureHash
    val hashedPassword = CachedUser.hashPassword(passwordText, passwordSalt)
    val id_realText = AnonymizeUtil.encryptWithNonce(institutionPK, id)
    val fullName_realText = AnonymizeUtil.encryptWithNonce(institutionPK, fullName)
    val email_realText = AnonymizeUtil.encryptWithNonce(institutionPK, email)

    val tmpUser = new User(None, "unknown", Some(id_realText), fullName_realText, email_realText, institutionPK, hashedPassword, passwordSalt, roleText, None)
    val userWithPk = tmpUser.insert
    val aliasId = AnonymizeUtil.aliasify(AnonymizeUtil.userAliasPrefixId, userWithPk.userPK.get)
    userWithPk.copy(id = aliasId).insertOrUpdate()
    val finalUser = User.get(userWithPk.userPK.get).get
    finalUser
  }

  /** Get the list of users with the given role. */
  def getUserListByRole(role: UserRole.Value): Seq[User] = {
    val action = query.filter(_.role === role.toString)
    Db.run(action.result)
  }

  /** List all users. */
  def list: List[User] = Db.run(query.result).toList

  case class UserInstitution(user: User, institution: Institution) {}

  /**
    * Get a list of all users with institution.
    *
    * @param instPK: If defined, get only from this institution, otherwise get all.
    */
  def listWithDependencies(instPK: Option[Long]): Seq[UserInstitution] = {
    val action = for {
      user <- query
      institution <- Institution.query if user.institutionPK === institution.institutionPK
    } yield (user, institution)

    val filtered = {
      if (instPK.isDefined) {
        action.filter(ui => ui._1.institutionPK === instPK.get)
      } else action
    }

    Db.run(filtered.result).map(ui => UserInstitution(ui._1, ui._2))
  }

  def delete(userPK: Long): Int = {
    val user = get(userPK)
    val q = query.filter(_.userPK === userPK)
    val action = q.delete
    val count = Db.run(action)
    if (user.isDefined) AnonymousTranslate.clearCache(user.get.institutionPK)
    count
  }

  def listUsersFromInstitution(institutionPK: Long): Seq[User] = {
    val action = query.filter(u => u.institutionPK === institutionPK)
    Db.run(action.result)
  }

  /**
    * Get the number of users that belong to the given institution.
    */
  def numberOfUsersInInstitution(institutionPK: Long): Int = {
    val action = query.filter(_.institutionPK === institutionPK).length
    Db.run(action.result)
  }

  /**
    * Get the admin for the given institution if there is one.
    */
  def getInstitutionAdminUser(institutionPK: Long): Option[User] = {
    listUsersFromInstitution(institutionPK).find(_.isAdmin)
  }

  /**
    * Make (and insert) admin user for given institution if there is not one already.
    *
    * If there is already a user in the institution, then their email is used, otherwise a fake one is made with "@unknown.edu".
    *
    * A random password is created.
    *
    * The user is created as an admin.
    */
  def getOrMakeInstitutionAdminUser(institutionPK: Long): User = {
    getInstitutionAdminUser(institutionPK) match {
      case Some(user) => user // there already is one.
      case _ =>
        def decrypt(text: String) = AnonymizeUtil.decryptWithNonce(institutionPK, text)

        val institutionName = decrypt(Institution.get(institutionPK).get.name_real.get)
        val adminId = adminIndicator + institutionName.trim.replaceAll("[^0-9a-zA-Z]", "_") // make an id
        val email = {
          listUsersFromInstitution(institutionPK).headOption match {
            case Some(u) => AnonymizeUtil.decryptWithNonce(institutionPK, u.email_real)
            case _       => adminId + "@unknown.edu"
          }
        }

        val passwordText = Crypto.makeRandomCipherKey.take(32)

        insertNewUser(institutionPK, adminId, adminId, email, passwordText, UserRole.admin.toString)
    }
  }

  def main(args: Array[String]): Unit = {
    Config.validate
    DbSetup.init
    //    println("======== usingInstitution(1): " + numberOfUsersInInstitution(1))
    //    println("======== usingInstitution(5): " + numberOfUsersInInstitution(5))
    //    println("======== user: " + get(5))
    //    println("======== user delete: " + delete(5))
    if (true) {
      val adminUser = getOrMakeInstitutionAdminUser(1)
      println("admin user: " + adminUser)
    }
  }
}
