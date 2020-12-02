package org.aqa.db

import org.aqa.Config
import org.restlet.Request
import org.aqa.AnonymizeUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.Level2Ldap
import org.aqa.Logging
import org.aqa.Crypto
import org.aqa.web.AuthenticationVerifier
import org.restlet.Response

class CachedUser(val user: User) {
  val timeout = System.currentTimeMillis + Config.AuthenticationTimeoutInMs

  override def toString = {
    val timeLeft = timeout - System.currentTimeMillis
    timeLeft + " : " + user.id
  }
}

/**
 * Keep a cache of authenticated users so that the database and/or LDAP do not need to
 * be consulted for every operation.  Entries in the cache have an expiration timeout.
 */
object CachedUser extends Logging {
  private val cache = scala.collection.mutable.HashMap[String, CachedUser]()

  private def cacheToString = {
    cache.synchronized({
      "cache size: " + cache.size + "\n    " + cache.keySet.map(k => k + " : " + cache(k)).mkString("\n    ")
    })
  }

  private def put(id: String, user: User): Unit = {
    cache.synchronized({
      cache.put(id, new CachedUser(user))
    })
  }

  private def getFromCache(id: String): Option[CachedUser] = {
    cache.synchronized({
      cache.get(id)
    })
  }

  /**
   * Given information gathered from LDAP, create a user automatically.
   */
  private def createUserWithLdap(id: String, secret: String, userInfo: Level2Ldap.UserInfo): Option[User] = {
    try {
      val institutionPK = Institution.getInstitutionByRealName(Config.LdapInstitutionName).get.institutionPK.get
      val roleText = Config.LdapRole
      val user = User.insertNewUser(institutionPK, id, userInfo.firstName + " " + userInfo.lastName, userInfo.email, secret, roleText)
      Some(user)
    } catch {
      case t: Throwable => {
        logger.error("Unexpected exception while automatically creating user for LDAP: " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Get the user with the given id.  By getting it from cache.  Users in the cache have
   * already had their passwords validated.
   */
  def get(id: String): Option[User] = {
    getFromCache(id) match {
      case Some(cu) => Some(cu.user)
      case _ => None
    }
  }

  /**
   * Perform a secure hash on a password.
   */
  def hashPassword(secret: String, passwordSalt: String): String = Crypto.secureHash(passwordSalt + secret)

  /**
   * Check to see if the password is correct.
   */
  def validatePassword(user: User, secret: String): Boolean = hashPassword(secret, user.passwordSalt).equals(user.hashedPassword)

  /**
   * The user is not cached.  Look in the database and in LDAP.
   */
  private def findOrCreateUser(id: String, secret: String): Option[User] = {

    lazy val dbValue = {
      val userList = User.list.filter(u => u.id_real.nonEmpty).filter(u => AnonymizeUtil.decryptWithNonce(u.institutionPK, u.id_real.get).equals(id))
      if (userList.nonEmpty) {
        put(id, userList.head)
      }
      userList.headOption
    }

    lazy val isInDb = dbValue.isDefined

    lazy val dbIsValid = isInDb && validatePassword(dbValue.get, secret)

    lazy val ldapEnabled = Config.LdapUrl.isDefined

    /**
     * True if the user has been validated by LDAP.  This means that their
     *  password must be correct and they must belong to at least one of the
     *  LDAP groups.
     */
    lazy val ldapIsValid: Boolean = {
      if (ldapEnabled) {
        Level2Ldap.getGroupListOfUser(id, secret) match {
          case Right(groupSet) => groupSet.intersect(Config.LdapGroupList.toSet).nonEmpty
          case Left(msg) => {
            logger.warn("Error getting group information for user " + id + " : " + msg)
            false
          }
          case _ => false
        }
      } else
        false
    }

    lazy val ldapUserInfo: Option[Level2Ldap.UserInfo] = {
      if (ldapEnabled) {
        Level2Ldap.getUserInfo(id, secret) match {
          case Right(userInfo) => Some(userInfo)
          case Left(msg) => {
            logger.warn("Error getting group information for user " + id + " : " + msg)
            None
          }
          case _ => None
        }
      } else
        None
    }

    /**
     * Construct a user from the DB value but with the LDAP password and put it into the cache.  This
     * happens when a user is created in the database and then later logs in with their Level 2
     * password instead of their database password.  It also covers the case where a user was created
     * when they logged using LDAP, and later changed their LDAP password.
     */
    def ldapFromDbValue: Option[User] = {
      val ldapUser = dbValue.get.copy(hashedPassword = hashPassword(secret, dbValue.get.passwordSalt))
      put(id, ldapUser)
      Some(ldapUser)
    }

    /**
     * Create a user from the LDAP information and put it in the database.  Also put it in the cache.
     */
    def ldapToDb: Option[User] = {
      if (ldapUserInfo.isDefined) {
        createUserWithLdap(id, secret, ldapUserInfo.get) match {
          case Some(user) => {
            user.insertOrUpdate
            put(id, user)
            logger.info("New user has been automatically created because they were authenticated by LDAP: " + user)
            Some(user)
          }
          case _ => None
        }
      } else
        None
    }

    val user: Option[User] = 0 match {
      case _ if isInDb && dbIsValid => dbValue // user is in database and password from database is valid
      case _ if isInDb && ldapIsValid => ldapFromDbValue // user is in database and password from LDAP is valid
      case _ if ldapIsValid => ldapToDb // user is in LDAP and valid, so put them in the database
      case _ => None
    }
    user

  }

  /**
   * Get <code>User</code> from request.  If found and it is valid, then put it in the cache.
   */
  def get(id: String, secret: String): Option[User] = {
    removeExpired
    val isInCache = getFromCache(id).isDefined
    // Do a quick check to see if it is in the cache and the password matches.  If this
    // works (which will be the usual case), then we are done.
    val userOpt = if (isInCache && validatePassword(getFromCache(id).get.user, secret))
      Some(getFromCache(id).get.user)
    else
      findOrCreateUser(id, secret)
    userOpt

  }

  /**
   * Get <code>User</code> from request.  If found and it is valid, then put it in the cache.
   */
  def get(request: Request): Option[User] = {
    removeExpired
    val cr = request.getChallengeResponse
    if (cr == null) // If no credentials, then no access.
      None
    else
      get(cr.getIdentifier, new String(cr.getSecret))
  }

  /**
   * Convenience function.
   */
  def get(response: Response): Option[User] = get(response.getRequest)

  /**
   * Remove expired entries from cache.
   */
  private def removeExpired: Unit = {
    cache.synchronized({
      val now = System.currentTimeMillis
      val expired = cache.filter(c => c._2.timeout < now).map(c1 => c1._1)
      cache --= expired
    })
  }

  /**
   * Remove all entries from cache.
   */
  def clear: Unit = {
    cache.synchronized({
      cache.clear
    })
  }

}