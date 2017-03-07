package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil

case class Machine(
        machinePK: Option[Long], // primary key
        id: String, // uniquely identifying name within hosting institution
        machineTypePK: Long, // type of machine foreign key
        multileafCollimatorPK: Long, // collimator
        epidPK: Long, // EPID
        institutionPK: Long, // institution that this machine belongs to
        serialNumber: String,
        imagingBeam2_5_mv: Boolean,
        onboardImager: Boolean,
        sixDimTabletop: Boolean,
        respiratoryManagement: Boolean,
        developerMode: Boolean,
        notes: String // optional further information
        ) {

    def insert = {
        val insertQuery = Machine.query returning Machine.query.map(_.machinePK) into ((machine, machinePK) => machine.copy(machinePK = Some(machinePK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(Machine.query.insertOrUpdate(this))

    def fileName = Machine.fileName(id)

}

object Machine {
    class MachineTable(tag: Tag) extends Table[Machine](tag, "machine") {

        def machinePK = column[Long]("machinePK", O.PrimaryKey, O.AutoInc)
        def id = column[String]("id")
        def machineTypePK = column[Long]("machineTypePK")
        def multileafCollimatorPK = column[Long]("multileafCollimatorPK")
        def epidPK = column[Long]("epidPK")
        def institutionPK = column[Long]("institutionPK")
        def notes = column[String]("notes")
        def serialNumber = column[String]("serialNumber")
        def imagingBeam2_5_mv = column[Boolean]("imagingBeam2_5_mv")
        def onboardImager = column[Boolean]("onboardImager")
        def sixDimTabletop = column[Boolean]("sixDimTabletop")
        def respiratoryManagement = column[Boolean]("respiratoryManagement")
        def developerMode = column[Boolean]("developerMode")

        def * = (
            machinePK.?,
            id,
            machineTypePK,
            multileafCollimatorPK,
            epidPK,
            institutionPK,
            serialNumber,
            imagingBeam2_5_mv,
            onboardImager,
            sixDimTabletop,
            respiratoryManagement,
            developerMode,

            notes) <> ((Machine.apply _)tupled, Machine.unapply _)

        def machineTypeFK = foreignKey("machineTypePK", machineTypePK, MachineType.query)(_.machineTypePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
        def multileafCollimatorFK = foreignKey("multileafCollimatorPK", multileafCollimatorPK, MultileafCollimator.query)(_.multileafCollimatorPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
        def epidFK = foreignKey("epidPK", epidPK, EPID.query)(_.epidPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
        def institutionFK = foreignKey("institutionPK", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    }

    def fileName(id: String): String = FileUtil.replaceInvalidFileNameCharacters(id, '_')

    val query = TableQuery[MachineTable]

    def get(machinePK: Long): Option[Machine] = {
        val action = query.filter(m => m.machinePK === machinePK)
        val list = Db.run(action.result)
        list.headOption
    }

    /**
     * Get a list of all machines.
     */
    def list: Seq[Machine] = Db.run(query.result)

    /** Dependent types */
    case class MMI(machine: Machine, machineType: MachineType, institution: Institution)

    def listWithDependencies: Seq[MMI] = {
        val action = for {
            machine <- query
            machineType <- MachineType.query if machineType.machineTypePK === machine.machineTypePK
            institution <- Institution.query if institution.institutionPK === machine.institutionPK
        } yield (machine, machineType, institution)
        Db.run(action.result).map(mmi => new MMI(mmi._1, mmi._2, mmi._3))
    }

    def listMachinesFromInstitution(institutionPK: Long): Seq[Machine] = {
        val action = query.filter(m => m.institutionPK === institutionPK)
        val seq = Db.run(action.result)
        seq
    }

    def delete(machinePK: Long): Int = {
        val q = query.filter(_.machinePK === machinePK)
        val action = q.delete
        Db.run(action)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        println("======== machine: " + get(5))
        //println("======== machine delete: " + delete(5))
    }
}
