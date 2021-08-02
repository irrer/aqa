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

import java.io.File
import edu.umro.util.Utility
import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.web.AnonymousTranslate

case class Machine(
  machinePK: Option[Long], // primary key
  id: String, // alias identifying name
  id_real: Option[String], // real name uniquely identifying name within hosting institution
  machineTypePK: Long, // type of machine foreign key
  configurationDirectory: Option[String], // directory containing configuration files unique to this machine
  multileafCollimatorPK: Long, // collimator
  epidPK: Long, // EPID
  institutionPK: Long, // institution that this machine belongs to
  serialNumber: Option[String], // encrypted version of the machine's serial number.  Becomes defined when a user associates data with it via the web interface.
  imagingBeam2_5_mv: Boolean, // True if this is supported.  Defaults to false.
  onboardImager: Boolean, // True if this is supported.  Defaults to false.
  table6DOF: Boolean, // True if (six degrees of freedom table) is supported.  Defaults to false.
  respiratoryManagement: Boolean, // True if this is supported.  Defaults to false.
  developerMode: Boolean, // True if this is supported.  Defaults to false.
  active: Boolean, // True if the machine is actively being used.  Defaults to true.  Setting to false may exclude this machine's data from some reports.
  notes: String // optional further information
) {

  def insert = {
    val insertQuery = Machine.query returning Machine.query.map(_.machinePK) into ((machine, machinePK) => machine.copy(machinePK = Some(machinePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    AnonymousTranslate.clearCache(institutionPK)
    result
  }

  def insertOrUpdate = {
    val count = Db.run(Machine.query.insertOrUpdate(this))
    AnonymousTranslate.clearCache(institutionPK)
    count
  }

  def fileName = Machine.fileName(id)

  def configDir: Option[File] = if (configurationDirectory.isDefined) Some(Machine.getConfigDir(configurationDirectory.get)) else None

  override def toString = {
    def fmt(text: String) = text.take(8) + "..."
    def fmtB(boo: Boolean) = boo.toString.subSequence(0, 1)
    "machinePK: " + (if (machinePK.isDefined) machinePK.get else "None") +
      "  id: " + id +
      "  id_real: " + (if (id_real.isDefined) fmt(id_real.get) else "None") +
      "  machineTypePK: " + machineTypePK +
      "  config dir : " + (if (configurationDirectory.isDefined) fmt(configurationDirectory.get) else "None") +
      "  MLC PK: " + multileafCollimatorPK +
      "  epidPK: " + epidPK +
      "  institutionPK: " + institutionPK +
      "  serialNumber: " + (if (serialNumber.isDefined) serialNumber.get else "None") +
      "  image 2_5_mv: " + fmtB(imagingBeam2_5_mv) +
      "  onboardImager: " + fmtB(onboardImager) +
      "  6DOF Table: " + fmtB(table6DOF) +
      "  resp mgmt: " + fmtB(respiratoryManagement) +
      "  dev mode: " + fmtB(developerMode) +
      "  active: " + fmtB(active) +
      "  notes: " + fmt(notes)
  }

}

object Machine extends Logging {

  class MachineTable(tag: Tag) extends Table[Machine](tag, "machine") {

    def machinePK = column[Long]("machinePK", O.PrimaryKey, O.AutoInc)
    def id = column[String]("id") // TODO add O.Unique constraint
    def id_real = column[Option[String]]("id_real")
    def machineTypePK = column[Long]("machineTypePK")
    def configurationDirectory = column[Option[String]]("configurationDirectory")
    def multileafCollimatorPK = column[Long]("multileafCollimatorPK")
    def epidPK = column[Long]("epidPK")
    def institutionPK = column[Long]("institutionPK")
    def notes = column[String]("notes")
    def serialNumber = column[Option[String]]("serialNumber")
    def imagingBeam2_5_mv = column[Boolean]("imagingBeam2_5_mv")
    def onboardImager = column[Boolean]("onboardImager")
    def table6DOF = column[Boolean]("table6DOF")
    def respiratoryManagement = column[Boolean]("respiratoryManagement")
    def developerMode = column[Boolean]("developerMode")
    def active = column[Boolean]("active")

    def * = (
      machinePK.?,
      id,
      id_real,
      machineTypePK,
      configurationDirectory,
      multileafCollimatorPK,
      epidPK,
      institutionPK,
      serialNumber,
      imagingBeam2_5_mv,
      onboardImager,
      table6DOF,
      respiratoryManagement,
      developerMode,
      active,
      notes) <> ((Machine.apply _)tupled, Machine.unapply _)

    def machineTypeFK = foreignKey("Machine_machineTypePKConstraint", machineTypePK, MachineType.query)(_.machineTypePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def multileafCollimatorFK = foreignKey("Machine_multileafCollimatorPKConstraint", multileafCollimatorPK, MultileafCollimator.query)(_.multileafCollimatorPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def epidFK = foreignKey("Machine_epidPKConstraint", epidPK, EPID.query)(_.epidPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def institutionFK = foreignKey("Machine_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
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
      logger.info("Setting serial number to " + sr + " for machine " + machine.id)
      Db.run(Machine.query.filter(_.machinePK === machPK).map(m => (m.configurationDirectory, m.serialNumber)).update(Some(cfgDirName), Some(sr))) match {
        case 1 => true
        case _ => false
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unable to make machine configuration dir for machinePK " + machPK + " : " + fmtEx(t))
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
  case class MMI(machine: Machine, machineType: MachineType, institution: Institution, mlc: MultileafCollimator, epid: EPID)

  /**
   * Get machine related info.
   *
   * @param instPK: If defined, only get machines from this institution, otherwise get from all institutions.
   */
  def listWithDependencies(instPK: Option[Long]): Seq[MMI] = {

    val action = for {
      machine <- query
      machineType <- MachineType.query if machineType.machineTypePK === machine.machineTypePK
      institution <- Institution.query if institution.institutionPK === machine.institutionPK
      mlc <- MultileafCollimator.query if mlc.multileafCollimatorPK === machine.multileafCollimatorPK
      epid <- EPID.query if epid.epidPK === machine.epidPK
    } yield (machine, machineType, institution, mlc, epid)

    val filtered = {
      if (instPK.isDefined) action.filter(c => c._3.institutionPK === instPK.get)
      else action
    }

    Db.run(filtered.result).map(mmi => new MMI(mmi._1, mmi._2, mmi._3, mmi._4, mmi._5))
  }

  def listMachinesFromInstitution(institutionPK: Long): Seq[Machine] = {
    val action = query.filter(m => m.institutionPK === institutionPK)
    val seq = Db.run(action.result)
    seq
  }

  def listMachinesWithCollimator(multileafCollimatorPK: Long): Seq[Machine] = {
    val action = query.filter(m => m.multileafCollimatorPK === multileafCollimatorPK)
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
      AnonymousTranslate.clearCache(machine.get.institutionPK)
      count
    } else 0
  }

  /**
   * Given an attribute list, determine which machines' DICOM files match according to DeviceSerialNumber.
   */
  def attributeListToMachine(attributeList: AttributeList): Option[Machine] = {
    try {
      Util.attributeListToDeviceSerialNumber(attributeList) match {
        case Some(serNo) => Machine.findMachinesBySerialNumber(serNo).headOption
        case _ => None
      }
    } catch {
      case t: Throwable =>
        None
    }
  }
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("======== machine: " + get(5))
    //println("======== machine delete: " + delete(5))
  }
}
