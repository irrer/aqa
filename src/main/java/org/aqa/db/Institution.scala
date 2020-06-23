package org.aqa.db

import Db.driver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import org.aqa.Crypto
import org.aqa.AnonymizeUtil

case class Institution(
  institutionPK: Option[Long], // primary key
  name: String, // alias name of institution
  name_real: Option[String], // real name of institution, encrypted
  url_real: String, // web address, encrypted
  description_real: String // any extra information, encrypted
) {

  def insert: Institution = {
    val insertQuery = Institution.query returning Institution.query.map(_.institutionPK) into ((institution, institutionPK) => institution.copy(institutionPK = Some(institutionPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(Institution.query.insertOrUpdate(this))

  def fileName = Institution.fileName(name)
}

object Institution extends Logging {

  class InstitutionTable(tag: Tag) extends Table[Institution](tag, "institution") {

    def institutionPK = column[Long]("institutionPK", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name", O.Unique, O.Length(400)) // SQL Server requires that unique columns be of limited length.
    def name_real = column[Option[String]]("name_real")
    def url_real = column[String]("url_real")
    def description_real = column[String]("description_real")

    def * = (institutionPK.?, name, name_real, url_real, description_real) <> ((Institution.apply _)tupled, Institution.unapply _)
  }

  val query = TableQuery[InstitutionTable]

  def fileName(name: String): String = FileUtil.replaceInvalidFileNameCharacters(name, '_')

  def get(institutionPK: Long): Option[Institution] = {
    val action = for {
      inst <- query if inst.institutionPK === institutionPK
    } yield (inst)

    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
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
    } yield (inst)

    val list = Db.run(action.result)

    val inst = if (list.isEmpty) None else Some(list.head)
    logger.debug("Institution with " + trimmedName + " : " + inst)
    inst
  }

  /**
   * Given its real name, get the corresponding institution.
   */
  def getInstitutionByRealName(name: String): Option[Institution] = {
    val trimmedName = name.trim
    val inst = list.find(i => AnonymizeUtil.decryptWithNonce(i.institutionPK.get, i.name_real.get).equalsIgnoreCase(trimmedName))
    inst
  }

  def deletable(instPK: Long): Option[String] = {
    if (User.numberOfUsersInInstitution(instPK) == 0) None else Some("There are users in this institution")
  }

  def delete(institutionPK: Long): Int = {
    val q = query.filter(_.institutionPK === institutionPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("======== listInstitutions: " + list.map(i => println("\n >>>>>>>> " + i)))
    println("======== inst: " + get(5))
    println("======== inst delete: " + delete(5))
  }
}
