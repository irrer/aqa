package org.aqa.db

import org.aqa.Config
import org.restlet.Request
import org.aqa.AnonymizeUtil

class CachedUser(val user: User) {
  val timeout = System.currentTimeMillis + Config.AuthenticationTimeoutInMs
}

object CachedUser {
  private val cache = scala.collection.mutable.HashMap[String, CachedUser]()

  def put(id: String, user: User): Unit = {
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
   * Get the user with the given id.  Try first getting it from cache, and if
   * that fails then from the database.  If getting from the database works, then
   * put it in the cache.
   */
  def get(id: String): Option[User] = {
    clean
    val canonId = id.toLowerCase.trim
    getFromCache(id) match {
      case Some(cu) => Some(cu.user)
      case _ => {
        User.getUserById(id) match {
          case Some(user) => {
            put(id, user)
            Some(user)
          }
          case _ => {
            val userList = User.list.filter(u => u.id_real.nonEmpty).filter(u => AnonymizeUtil.decryptWithNonce(u.institutionPK, u.id_real.get).equals(id))
            if (userList.nonEmpty) {
              put(id, userList.head)
            }
            userList.headOption
          }
        }
      }
    }
  }

  /**
   * Get user from request.
   */
  def get(request: Request): Option[User] = {
    val cr = request.getChallengeResponse
    if (cr == null) None
    else get(cr.getIdentifier)
  }

  /**
   * Remove expired entries from cache.
   */
  def clean: Unit = {
    cache.synchronized({
      val now = System.currentTimeMillis
      val expired = cache.filter(c => c._2.timeout < now).map(c1 => c1._1)
      cache --= expired
    })
  }

  def remove(id: String): Option[CachedUser] = {
    cache.synchronized({
      cache.remove(id)
    })
  }

  def main(args: Array[String]): Unit = {
  }

}