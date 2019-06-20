package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File

case class MachineBeamEnergy(
  machineBeamEnergyPK: Option[Long], // primary key
  machinePK: Long, // machine primary key
  photonEnergy_MeV: Option[Double], // photon energy in million electron volts
  maxDoseRate_MUperMin: Option[Double], // dose rate in MU / minute
  fffEnergy_MeV: Option[Double] // flat filter free energy in million electron volts
) {

  def insert: MachineBeamEnergy = {
    val insertQuery = MachineBeamEnergy.query returning MachineBeamEnergy.query.map(_.machineBeamEnergyPK) into ((machineBeamEnergy, machineBeamEnergyPK) => machineBeamEnergy.copy(machineBeamEnergyPK = Some(machineBeamEnergyPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(MachineBeamEnergy.query.insertOrUpdate(this))

  override def equals(o: Any): Boolean = {
    val other = o.asInstanceOf[MachineBeamEnergy]
    photonEnergy_MeV.equals(other.photonEnergy_MeV) &&
      maxDoseRate_MUperMin.equals(other.maxDoseRate_MUperMin) &&
      fffEnergy_MeV.equals(other.fffEnergy_MeV)
  }

  def isFFF = fffEnergy_MeV.isDefined && (fffEnergy_MeV.get > 0)

  override def toString = {
    def show(d: Option[Double]) = if (d.isDefined) Util.fmtDbl(d.get) else "none"

    "PK: " + machineBeamEnergyPK +
      "    machinePK: " + machinePK +
      "    photonEnergy_MeV: " + show(photonEnergy_MeV) +
      "    maxDoseRate_MUperMin: " + show(maxDoseRate_MUperMin) +
      "    fff: " + isFFF
  }
}

object MachineBeamEnergy {
  class MachineBeamEnergyTable(tag: Tag) extends Table[MachineBeamEnergy](tag, "machineBeamEnergy") {

    def machineBeamEnergyPK = column[Long]("machineBeamEnergyPK", O.PrimaryKey, O.AutoInc)
    def machinePK = column[Long]("machinePK")
    def photonEnergy_MeV = column[Option[Double]]("photonEnergy_MeV")
    def maxDoseRate_MUperMin = column[Option[Double]]("maxDoseRate_MUperMin")
    def fffEnergy_MeV = column[Option[Double]]("fffEnergy_MeV")

    def * = (
      machineBeamEnergyPK.?,
      machinePK,
      photonEnergy_MeV,
      maxDoseRate_MUperMin,
      fffEnergy_MeV) <> ((MachineBeamEnergy.apply _)tupled, MachineBeamEnergy.unapply _)

    def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineBeamEnergyTable]

  def get(machineBeamEnergyPK: Long): Option[MachineBeamEnergy] = {
    val action = for {
      inst <- MachineBeamEnergy.query if inst.machineBeamEnergyPK === machineBeamEnergyPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByMachine(machinePK: Long): Seq[MachineBeamEnergy] = {
    val action = for {
      inst <- MachineBeamEnergy.query if inst.machinePK === machinePK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  /**
   * Get a list of all machineBeamEnergies.
   */
  def list = Db.run(query.result)

  def delete(machineBeamEnergyPK: Long): Int = {
    val q = query.filter(_.machineBeamEnergyPK === machineBeamEnergyPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByMachinePK(machinePK: Long): Int = {
    val q = query.filter(_.machinePK === machinePK)
    val action = q.delete
    Db.run(action)
  }

  def sorter(a: MachineBeamEnergy, b: MachineBeamEnergy): Boolean = {
    def srtDef(mbe: (MachineBeamEnergy) => Option[Double]): Option[Boolean] = {
      for (ea <- mbe(a); eb <- mbe(b)) yield (ea < eb)
    }

    def srtNotDef(mbe: (MachineBeamEnergy) => Option[Double]): Option[Boolean] = {
      { (mbe(a), mbe(b)) } match {
        case (Some(a), _) => Some(false)
        case (_, Some(b)) => Some(true)
        case _ => None
      }
    }
    Seq(srtDef(_.photonEnergy_MeV), srtDef(_.maxDoseRate_MUperMin), srtDef(_.fffEnergy_MeV)).flatten.headOption match {
      case Some(bool) => bool
      case _ => {
        Seq(srtNotDef(_.photonEnergy_MeV), srtNotDef(_.maxDoseRate_MUperMin), srtNotDef(_.fffEnergy_MeV)).flatten.headOption match {
          case Some(b) => b
          case _ => false
        }
      }
    }
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("MachineBeamEnergy.main done")
  }
}
