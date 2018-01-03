package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.AuthenticationVerifier
import java.sql.Timestamp

case class User(
  userPK: Option[Long], // primary key
  id: String, // login name
  fullName: String, // full name, as in 'Louis Pasteur'
  email: String, // email for contacting
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

}

object User extends Logging {
  class UserTable(tag: Tag) extends Table[User](tag, "user") {

    def userPK = column[Long]("userPK", O.PrimaryKey, O.AutoInc)
    def id = column[String]("id")
    def fullName = column[String]("fullName")
    def email = column[String]("email")
    def institutionPK = column[Long]("institutionPK")
    def hashedPassword = column[String]("hashedPassword")
    def passwordSalt = column[String]("passwordSalt")
    def role = column[String]("role")
    def termsOfUseAcknowledgment = column[Option[Timestamp]]("termsOfUseAcknowledgment")

    def * = (
      userPK.?,
      id,
      fullName,
      email,
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
   * Given their email, determine whether the given user is in the database.
   */
  def findUserByEmail(emailRaw: String): Option[User] = {
    val email = emailRaw.trim.toLowerCase
    val action = query.filter(_.email.toLowerCase === email)
    val seq = Db.run(action.result)

    val user = if (seq.size > 0) Some(seq(0)) else None
    val textResult: String = if (user.isDefined) user.get.toString else "None"
    logger.info("findUserByEmail search for user with email " + emailRaw + " + found " + textResult)
    user
  }

  /**
   * Get the of user with the given id.  Comparison is case insensitive.
   */
  def getUserById(idRaw: String): Option[User] = {
    val id = idRaw.trim.toLowerCase
    val action = query.filter(_.id.toLowerCase === id)
    val seq = Db.run(action.result)
    logger.info("getUserById search for user with id " + idRaw + " + found " + seq.size + " matches.")
    seq.headOption
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
   * Get a list of all users with institution name.
   */
  def listWithDependencies: Seq[UserInstitution] = {
    val action = for {
      user <- query
      institution <- Institution.query if user.institutionPK === institution.institutionPK
    } yield (user, institution)
    Db.run(action.result).map(ui => new UserInstitution(ui._1, ui._2))
  }

  def delete(userPK: Long): Int = {
    val q = query.filter(_.userPK === userPK)
    val action = q.delete
    Db.run(action)
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
