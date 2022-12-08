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

/**
  * Define roles of users, each of which authorizes them to do do various things.
  */
object UserRole extends Enumeration {

  val publik = Value // can look at 'about us' type of information
  val guest = Value // can look at data but not run procedures
  val user = Value // can run procedures
  val dev = Value // can develop procedures
  val admin = Value // can change configuration

  /**
    * Convert text to a UserRole.  Is case insensitive.
    */
  def stringToUserRole(text: String): Option[UserRole.Value] = UserRole.values.find(ur => ur.toString.equalsIgnoreCase(text))

  /**
    * For testing only.
    */
  def main(args: Array[String]): Unit = {

    def foo(ur: UserRole.Value): Unit = {
      println("foo ur: " + ur.id + "  " + ur)
    }

    println("equality ==     : " + (stringToUserRole("guest").get == UserRole.guest))
    println("equality eq     : " + (stringToUserRole("guest").get.eq(UserRole.guest)))
    println("equality equals : " + (stringToUserRole("guest").get.equals(UserRole.guest)))

    values.toArray.toList.map(ur => println("ur: " + ur.id + "  " + ur))

    foo(UserRole.admin)

    println("stringToUserRole AdMin : " + stringToUserRole("AdMin"))

    println("stringToUserRole foo : " + stringToUserRole("foo"))
  }

}
