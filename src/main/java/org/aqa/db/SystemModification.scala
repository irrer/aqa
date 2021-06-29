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

import Db.driver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.sql.Timestamp

/**
 * Describe a modification made to the system infrastructure.
 */

case class SystemModification(
  systemModificationPK: Option[Long], // primary key
  date: Timestamp, // time of systemModification
  userPK: Long, // user that did the modification
  summary: String, // synopsis of modification
  description: String // details of modification
) {

  def insert: SystemModification = {
    val insertQuery = SystemModification.query returning SystemModification.query.map(_.systemModificationPK) into ((systemModification, systemModificationPK) => systemModification.copy(systemModificationPK = Some(systemModificationPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(SystemModification.query.insertOrUpdate(this))
}

object SystemModification extends Logging {

  class SystemModificationTable(tag: Tag) extends Table[SystemModification](tag, "systemModification") {

    def systemModificationPK = column[Long]("systemModificationPK", O.PrimaryKey, O.AutoInc)
    def date = column[Timestamp]("date")
    def userPK = column[Long]("userPK")
    def summary = column[String]("summary")
    def description = column[String]("description")

    def * = (
      systemModificationPK.?,
      date,
      userPK,
      summary,
      description) <>
      ((SystemModification.apply _)tupled, SystemModification.unapply _)
    def userFK = foreignKey("SystemModification_userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[SystemModificationTable]

  def get(systemModificationPK: Long): Option[SystemModification] = {
    val action = for {
      inst <- query if inst.systemModificationPK === systemModificationPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  case class SystemModificationComposite(systemModification: SystemModification, user: User);

  /**
   * Get a list of all users with institution name.
   */
  def listWithDependencies: Seq[SystemModificationComposite] = {
    val action = for {
      sysMod <- query
      user <- User.query if sysMod.userPK === user.userPK
    } yield (sysMod, user)
    Db.run(action.result).map(smc => new SystemModificationComposite(smc._1, smc._2))
  }

  /**
   * Get a list of all systemModifications.
   */
  def list: Seq[SystemModification] = Db.run(query.result)

  /**
   * Get the one with the largest time stamp if it exists.
   */
  def getLatest: Option[SystemModification] = {
    val latest = Db.run(query.sortBy(_.date.reverse).take(1).result)
    latest.headOption
  }

  /**
   * Get the number of rows.
   */
  def getSize = Db.run(query.size.result)

  def delete(systemModificationPK: Long): Int = {
    val q = query.filter(_.systemModificationPK === systemModificationPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("======== listSystemModifications: " + list.map(i => println("\n >>>>>>>> " + i)))
    println("======== inst: " + get(5))
    println("======== inst delete: " + delete(5))
  }
}
