package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil

case class Institution(
        institutionPK: Option[Long], // primary key
        name: String, // name of institution
        url: String, // web address
        description: String // any extra information
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

object Institution {
    class InstitutionTable(tag: Tag) extends Table[Institution](tag, "institution") {

        def institutionPK = column[Long]("institutionPK", O.PrimaryKey, O.AutoInc)
        def name = column[String]("name")
        def url = column[String]("url")
        def description = column[String]("description")

        def * = (institutionPK.?, name, url, description) <> ((Institution.apply _)tupled, Institution.unapply _)
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
     * Given its name, get the corresponding institution.
     */
    def getInstitutionByName(name: String): Option[Institution] = {
        val trimmedName = name.trim
        val action = for {
            inst <- query if inst.name === trimmedName
        } yield (inst)

        val list = Db.run(action.result)

        val inst = if (list.isEmpty) None else Some(list.head)
        logInfo("Institution with " + trimmedName + " : " + inst)
        inst
    }

    /**
     * Get a list of all institutions.
     */
    def list: Seq[Institution] = Db.run(query.result)

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
