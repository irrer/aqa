package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Util
import org.aqa.web.AuthenticationVerifier
import java.sql.Date
import org.aqa.Logging._
import java.sql.Timestamp
import org.aqa.run.ProcedureStatus
import scala.collection.mutable.ArrayBuffer
import org.aqa.Config

/** Establish connection to the database and ensure that tables are created. */

object DbSetup {

    private def makeDummyInstitution: Institution = new Institution(None, "InstitutionName", Util.aqaUrl, "No description")

    private def ensureAtLeastOneInstitution: Institution = {
        Institution.list.headOption match {
            case Some(inst) => inst
            case None => {
                makeDummyInstitution.insert
            }
        }
    }

    private def makeAdminUser: User = {
        val defaultUser = "admin"
        val defaultPassword = "admin"

        val email = defaultUser + "@" + Util.aqaDomain
        val passwordSalt = Util.randomSecureHash
        val hashedPassword = AuthenticationVerifier.hashPassword(defaultPassword, passwordSalt)

        val institutionPK = ensureAtLeastOneInstitution.institutionPK.get
        val adminUser = new User(None, "admin", "An Administrator", email, institutionPK, hashedPassword, passwordSalt, UserRole.admin.toString)
        adminUser.insert
    }

    /** Ensure that the system has at least one admin user.  If there is not one, then create one. */
    private def ensureAdminUser = {
        User.getUserListByRole(UserRole.admin).headOption match {
            case Some(user) => user
            case None => makeAdminUser
        }
    }

    /**
     * Initialize database by creating tables in dependency order.
     */
    def init = {
        val valid = Config.validate // force configuration to be read

        val list = List(Institution.query,
            User.query,
            Procedure.query,
            MachineType.query,
            MultileafCollimator.query,
            EPID.query,
            Machine.query,
            Input.query,
            MaintenanceRecord.query,
            Output.query,
            CentralAxis.query,
            LeafOffsetCorrection.query,
            LeafTransmission.query)

        list.map(q => Db.createTableIfNonexistent(q.asInstanceOf[TableQuery[Table[_]]]))

        //        Db.createTableIfNonexistent(Institution.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(User.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(Procedure.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(MachineType.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(MultileafCollimator.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(EPID.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(Machine.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(Input.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(MainentanceRecord.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(Output.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(CentralAxis.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(LeafOffsetCorrection.query.asInstanceOf[TableQuery[Table[_]]])
        //        Db.createTableIfNonexistent(LeafTransmission.query.asInstanceOf[TableQuery[Table[_]]])

        ensureAdminUser
    }

    /**
     * Replace this with something that compares the schema of the database to the schema of a newly made database.  TODO
     * Make one row in each table and then delete this.  This verifies that the
     *  definition in the database matches the definition in the code.
     *
     *  This test might leave garbage in the database.  Do not use it.  Need to compare schema instead.
     */
    def smokeTest: Boolean = {

        case class Undo(undoFunc: () => _, name: String) {
            def doUndo: Boolean = {
                try {
                    println("Undoing " + name + " ...")
                    undoFunc()
                    println("Undid " + name)
                    true
                }
                catch {
                    case t: Throwable =>
                        logSevere("delete failure: " + fmtEx(t))
                        false
                }

            }
        }

        val undoList = new ArrayBuffer[Undo]()

        try {
            init

            val timestamp = new Timestamp(System.currentTimeMillis)

            val institution = (new Institution(None, "InstitutionName", "https://URL.org", "No description")).insert
            undoList += new Undo({ () => Institution.delete(institution.institutionPK.get) }, "institution " + institution.institutionPK.get)

            val user = makeAdminUser
            undoList += new Undo({ () => User.delete(user.userPK.get) }, "user ")

            val procedure = (new Procedure(None, "procedureName", "version", 0.toFloat, new Date(System.currentTimeMillis), user.userPK.get, "webInterface", "procedure notes")).insert
            undoList += new Undo({ () => Procedure.delete(procedure.procedurePK.get) }, "procedure " + procedure.procedurePK.get)

            val machineType = (new MachineType(None, "manufacturer", "model", "version", "notes")).insert
            undoList += new Undo({ () => MachineType.delete(machineType.machineTypePK.get) }, "machineType " + machineType.machineTypePK.get)

            val multileafCollimator = (new MultileafCollimator(None, "manufacturer", "model", "version", 0, 0, 0, 0, 0, 0, 0, 0, "notes")).insert
            undoList += new Undo({ () => MultileafCollimator.delete(multileafCollimator.multileafCollimatorPK.get) }, "multileafCollimator " + multileafCollimator.multileafCollimatorPK.get)

            val machine = (new Machine(None, "id", machineType.machineTypePK.get, multileafCollimator.multileafCollimatorPK, None, institution.institutionPK.get, "notes")).insert
            undoList += new Undo({ () => Machine.delete(machine.machinePK.get) }, "machine " + machine.machinePK.get)

            val input = (new Input(None, Some("dir"), timestamp, user.userPK, machine.machinePK, Some("PatientID"), Some(timestamp))).insert
            undoList += new Undo({ () => Input.delete(input.inputPK.get) }, "input " + input.inputPK.get)

            val output = (new Output(None, input.inputPK.get, "dir", procedure.procedurePK.get, user.userPK, timestamp, Some(timestamp), Some(timestamp), Some(timestamp), machine.machinePK, ProcedureStatus.fail.toString, false.toString)).insert
            undoList += new Undo({ () => Output.delete(output.outputPK.get) }, "output " + output.outputPK.get)

            val centralAxis = (new CentralAxis(None, output.outputPK.get, 0)).insert
            undoList += new Undo({ () => CentralAxis.delete(centralAxis.centralAxisPK.get) }, "centralAxis " + centralAxis.centralAxisPK.get)

            val leafOffsetCorrection = (new LeafOffsetCorrection(None, output.outputPK.get, "section", 0, 0)).insert
            undoList += new Undo({ () => LeafOffsetCorrection.delete(leafOffsetCorrection.leafOffsetCorrectionPK.get) }, "leafOffsetCorrection " + leafOffsetCorrection.leafOffsetCorrectionPK.get)

            val leafTransmission = (new LeafTransmission(None, output.outputPK.get, "section", 0, 0)).insert
            undoList += new Undo({ () => LeafOffsetCorrection.delete(leafTransmission.leafTransmissionPK.get) }, "leafTransmission " + leafTransmission.leafTransmissionPK.get)

            undoList.reverse.map(u => u.doUndo).find(r => !r).isEmpty
        }
        catch {
            case t: Throwable => {
                logSevere("Failed database smoke test: " + fmtEx(t))
                undoList.reverse.map(u => u.doUndo)
                false
            }
        }
    }

    def main(args: Array[String]): Unit = {
        println("smokeTest passes: " + smokeTest)
    }
}