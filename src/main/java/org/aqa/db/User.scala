package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.AuthenticationVerifier
import java.sql.Timestamp
import org.aqa.Crypto
import org.aqa.AnonymizeUtil

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
    result
  }

  def insertOrUpdate = Db.run(User.query.insertOrUpdate(this))

  /**
   * Update the termsOfUseAcknowledgment, possibly to None.  Returns number of records updated, which
   * should always be one.  If it is zero then it is probably because the object is not in the database.
   */
  def updateTermsOfUseAcknowledgment(timestamp: Option[Timestamp]) = {
    Db.run(User.query.filter(_.userPK === userPK.get).map(u => (u.termsOfUseAcknowledgment)).update((timestamp)))
  }

  def getRole = UserRole.stringToUserRole(role)

  override def toString = {
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
}

object User extends Logging {

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

    def * = (
      userPK.?,
      id,
      id_real,
      fullName_real,
      email_real,
      institutionPK,
      hashedPassword,
      passwordSalt,
      role,
      termsOfUseAcknowledgment) <> ((User.apply _)tupled, User.unapply _)

    def institutionFK = foreignKey("institutionPK", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[UserTable]

  def get(userPK: Long): Option[User] = {
    val action = for {
      user <- query if user.userPK === userPK
    } yield (user)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
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
    val hashedPassword = AuthenticationVerifier.hashPassword(passwordText, passwordSalt)
    val id_realText = AnonymizeUtil.encryptWithNonce(institutionPK, id)
    val fullName_realText = AnonymizeUtil.encryptWithNonce(institutionPK, fullName)
    val email_realText = AnonymizeUtil.encryptWithNonce(institutionPK, email)

    val tmpUser = new User(None, "unknown", Some(id_realText), fullName_realText, email_realText, institutionPK, hashedPassword, passwordSalt, roleText, None)
    val userWithPk = tmpUser.insert
    val aliasId = AnonymizeUtil.aliasify(AnonymizeUtil.userAliasPrefixId, userWithPk.userPK.get)
    userWithPk.copy(id = aliasId).insertOrUpdate
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

  case class UserInstitution(user: User, institution: Institution);

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

    Db.run(filtered.result).map(ui => new UserInstitution(ui._1, ui._2))
  }

  def delete(userPK: Long): Int = {
    val q = query.filter(_.userPK === userPK)
    val action = q.delete
    Db.run(action)
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

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("======== usingInstitution(1): " + numberOfUsersInInstitution(1))
    println("======== usingInstitution(5): " + numberOfUsersInInstitution(5))
    println("======== user: " + get(5))
    println("======== user delete: " + delete(5))
  }
}
