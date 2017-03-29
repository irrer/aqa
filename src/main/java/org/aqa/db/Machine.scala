package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import edu.umro.util.Utility

case class Machine(
        machinePK: Option[Long], // primary key
        id: String, // uniquely identifying name within hosting institution
        machineTypePK: Long, // type of machine foreign key
        configurationDirectory: Option[String], // directory containing configuration files unique to this machine
        multileafCollimatorPK: Long, // collimator
        epidPK: Long, // EPID
        institutionPK: Long, // institution that this machine belongs to
        serialNumber: Option[String],
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

    def configDir: Option[File] = if (configurationDirectory.isDefined) Some(Machine.getConfigDir(configurationDirectory.get)) else None
}

object Machine {
    class MachineTable(tag: Tag) extends Table[Machine](tag, "machine") {

        def machinePK = column[Long]("machinePK", O.PrimaryKey, O.AutoInc)
        def id = column[String]("id")
        def machineTypePK = column[Long]("machineTypePK")
        def configurationDirectory = column[Option[String]]("configurationDirectory")
        def multileafCollimatorPK = column[Long]("multileafCollimatorPK")
        def epidPK = column[Long]("epidPK")
        def institutionPK = column[Long]("institutionPK")
        def notes = column[String]("notes")
        def serialNumber = column[Option[String]]("serialNumber")
        def imagingBeam2_5_mv = column[Boolean]("imagingBeam2_5_mv")
        def onboardImager = column[Boolean]("onboardImager")
        def sixDimTabletop = column[Boolean]("sixDimTabletop")
        def respiratoryManagement = column[Boolean]("respiratoryManagement")
        def developerMode = column[Boolean]("developerMode")

        def * = (
            machinePK.?,
            id,
            machineTypePK,
            configurationDirectory,
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

    private lazy val machConfigBaseDir: File = {
        val dir = new File(Config.DataDir, Config.machineConfigurationDirName)
        dir.mkdirs
        dir
    }

    def getConfigDir(configurationDirectory: String) = new File(machConfigBaseDir, configurationDirectory)

    /**
     * Build a name and use it to create a new configuration directory for the given machine.  The name
     * is made of institution + machine id + serial number
     */
    private def initConfigDir(machine: Machine, serialNumber: String): String = {
        val instName = Institution.get(machine.institutionPK).get.name.trim
        val rawName = (instName + "_" + machine.id + "_" + serialNumber).replace(' ', '_')
        val name = FileUtil.replaceInvalidFileNameCharacters(rawName, '_')
        val dir = getConfigDir(name)
        dir.mkdirs
        name
    }

    private def deleteConfigDir(machine: Machine) = {
        machine.configDir match {
            case Some(dir) => Utility.deleteFileTree(dir)
            case _ =>
        }
    }

    /**
     * Set the serial number for the machine and create the corresponding configuration directory.  Also put the
     *  configuration directory name in the machine's database entry.  Return true on success.
     */
    def setSerialNumber(machPK: Long, sr: String): Boolean = {
        try {
            val machine = Machine.get(machPK).get
            val cfgDirName = initConfigDir(machine, sr)
            Db.run(Machine.query.filter(_.machinePK === machPK).map(m => (m.configurationDirectory, m.serialNumber)).update(Some(cfgDirName), Some(sr))) match {
                case 1 => true
                case _ => false
            }
        }
        catch {
            case t: Throwable => {
                logWarning("Unable to make machine configuration dir for machinePK " + machPK + " : " + fmtEx(t))
                false
            }
        }
    }

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

    def findMachinesBySerialNumber(serNo: String): Seq[Machine] = {
        val sn = serNo.trim
        val action = query.filter(m => m.serialNumber === sn)
        val seq = Db.run(action.result)
        seq
    }

    /**
     * Delete the machine from the database.  If that is successful, then also delete its configuration directory.
     */
    def delete(machinePK: Long): Int = {
        val machine = get(machinePK)
        if (machine.isDefined) {
            val q = query.filter(_.machinePK === machinePK)
            val action = q.delete
            val count = Db.run(action)
            deleteConfigDir(machine.get)
            count
        }
        else 0
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        println("======== machine: " + get(5))
        //println("======== machine delete: " + delete(5))
    }
}
