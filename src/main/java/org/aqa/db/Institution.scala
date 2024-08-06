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

import edu.umro.ScalaUtil.FileUtil
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.web.AnonymousTranslate
import org.aqa.webrun.phase2.phase2csv.MetadataCache

case class Institution(
    institutionPK: Option[Long], // primary key
    name: String, // alias name of institution
    name_real: Option[String], // real name of institution, encrypted
    url_real: String, // web address, encrypted
    description_real: String // any extra information, encrypted
) {

  def insert: Institution = {
    MetadataCache.invalidate()
    val insertQuery = Institution.query returning Institution.query.map(_.institutionPK) into ((institution, institutionPK) => institution.copy(institutionPK = Some(institutionPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    if (institutionPK.isDefined) AnonymousTranslate.clearCache(institutionPK.get)
    result
  }

  def insertOrUpdate(): Int = {
    MetadataCache.invalidate()
    val count = Db.run(Institution.query.insertOrUpdate(this))
    if (institutionPK.isDefined) AnonymousTranslate.clearCache(institutionPK.get)
    count
  }

  def fileName: String = Institution.fileName(name)

  def getRealName: String = {
    AnonymizeUtil.decryptWithNonce(institutionPK.get, name_real.get)
  }
}

object Institution extends Logging {

  class InstitutionTable(tag: Tag) extends Table[Institution](tag, "institution") {

    def institutionPK = column[Long]("institutionPK", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name", O.Unique, O.Length(400)) // SQL Server requires that unique columns be of limited length.
    def name_real = column[Option[String]]("name_real")
    def url_real = column[String]("url_real")
    def description_real = column[String]("description_real")

    def * = (institutionPK.?, name, name_real, url_real, description_real) <> (Institution.apply _ tupled, Institution.unapply)
  }

  val query = TableQuery[InstitutionTable]

  def fileName(name: String): String = FileUtil.replaceInvalidFileNameCharacters(name, '_')

  def get(institutionPK: Long): Option[Institution] = {
    val action = for {
      inst <- query if inst.institutionPK === institutionPK
    } yield inst

    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all institutions.
    */
  def list: Seq[Institution] = Db.run(query.result)

  /**
    * Given its name, get the corresponding institution.
    */
  def getInstitutionByName(name: String): Option[Institution] = {
    val trimmedName = name.trim
    val action = for {
      inst <- query if inst.name === trimmedName
    } yield inst

    val list = Db.run(action.result)

    val inst = list.headOption
    logger.debug("Institution with " + trimmedName + " : " + inst)
    inst
  }

  /**
    * Given its real name, get the corresponding institution.
    */
  def getInstitutionByRealName(name: String): Option[Institution] = {
    val trimmedName = name.trim
    val inst = list.filter(_.name_real.isDefined).find(i => AnonymizeUtil.decryptWithNonce(i.institutionPK.get, i.name_real.get).trim.equalsIgnoreCase(trimmedName))
    inst
  }

  def deletable(instPK: Long): Option[String] = {
    if (User.numberOfUsersInInstitution(instPK) == 0) None else Some("There are users in this institution")
  }

  def delete(institutionPK: Long): Int = {
    MetadataCache.invalidate()
    val q = query.filter(_.institutionPK === institutionPK)
    val action = q.delete
    val count = Db.run(action)
    AnonymousTranslate.clearCache(institutionPK)
    count
  }

  def main(args: Array[String]): Unit = {
    Config.validate
    DbSetup.init
    println("======== listInstitutions: " + list.map(i => println("\n >>>>>>>> " + i)))
    println("======== inst: " + get(5))
    println("======== inst delete: " + delete(5))
  }
}
