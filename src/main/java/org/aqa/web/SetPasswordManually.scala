package org.aqa.web

import org.restlet.security.Verifier
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.User
import org.aqa.db.DbSetup
import org.aqa.db.UserRole
import scala.io.StdIn
import org.aqa.Crypto

object SetPasswordManually {

  // These values will set a password to 'foo' :
  // newSalt     : 70d59c36318434688b0d07f8e43601176178f334397a80774a68820c521120b6012e2f17e59051c5b48c6414749519153957a3e9e49352e810c2c5a7edaf5b77
  // newHashedPW : 7257e25108abd527c1181d73bb07d37fa02cbd930dbf87d8278f6eaed0f8e4dd85d91c13e61e01e333ef0c82db90fb6f7e4731abea59309299844cea787b6aab

  def main(args: Array[String]): Unit = {
    val dummy = Config.validate
    DbSetup.init
    print("Enter user id: ")
    val id = StdIn.readLine
    print("Enter password (no restrictions): ")
    val password = StdIn.readLine
    val user = User.getUserById(id)
    if (user.isEmpty) {
      println("No such user " + id)
      System.exit(1)
    }

    val newSalt = Crypto.randomSecureHash
    val newHashedPW = AuthenticationVerifier.hashPassword(password, newSalt)
    println("newSalt     : " + newSalt)
    println("newHashedPW : " + newHashedPW)

    val ou = user.get
    val newUser = new User(ou.userPK, ou.id, None, ou.fullName_real, ou.email_real, ou.institutionPK, newHashedPW, newSalt, ou.role, None)
    val count = newUser.insertOrUpdate
    println("newSalt     : " + newSalt)
    println("newHashedPW : " + newHashedPW)
    if (count == 1) println("success") else println("failed")
    System.exit(0)
  }

}
