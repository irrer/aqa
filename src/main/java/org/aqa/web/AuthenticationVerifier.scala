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

package org.aqa.web

import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.UserRole
import org.aqa.Config
import org.restlet.security.Verifier
import org.restlet.Request
import org.restlet.Response

class AuthenticationVerifier(getRequestedRole: (Request, Response) => UserRole.Value) extends Verifier with Logging {

  override def verify(request: Request, response: Response): Int = {
    val requestedRole = getRequestedRole(request, response)
    if (requestedRole.id == UserRole.publik.id) Verifier.RESULT_VALID // let anyone into public areas
    else {
      request.getChallengeResponse match {
        case null => Verifier.RESULT_MISSING // The CachedUser.get function checks for this, but checking it here allows for finer grained reporting of the credentials.
        case challResp =>
          val user = CachedUser.get(request)

          val status: Int = {
            CachedUser.get(request) match {
              case Some(user) => Verifier.RESULT_VALID
              case _          => Verifier.RESULT_UNKNOWN
            }
          }
          status
      }
    }

    //Verifier.RESULT_VALID  // uncomment this line for testing to allow users to use the system without authenticating
  }
}

object AuthenticationVerifier {

  private def containsNonAlpha(passwordText: String): Boolean = passwordText.toLowerCase.replaceAll("[a-z]", "").nonEmpty

  /**
    * Only permit high quality passwords.
    */
  def judgePassword(password: String): Option[String] = {
    0 match {
      case _ if password.length < Config.MinPasswordSize => Some("Password must be at least " + Config.MinPasswordSize + " characters long.")
      case _ if !containsNonAlpha(password)              => Some("Password must contain at least one non-alpha character.")
      case _                                             => None // good enough
    }
  }

  def verifierResultToString(result: Int): Option[String] = {
    result match {
      case Verifier.RESULT_INVALID     => Some("RESULT_INVALID")
      case Verifier.RESULT_MISSING     => Some("RESULT_MISSING")
      case Verifier.RESULT_STALE       => Some("RESULT_STALE")
      case Verifier.RESULT_UNSUPPORTED => Some("RESULT_UNSUPPORTED")
      case Verifier.RESULT_UNKNOWN     => Some("RESULT_UNKNOWN")
      case Verifier.RESULT_VALID       => Some("RESULT_VALID")
      case _                           => None
    }
  }

}
