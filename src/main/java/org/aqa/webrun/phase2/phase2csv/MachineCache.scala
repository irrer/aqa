package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.EPID
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator

class MachineCache {

  /** Map of machines by machinePK. */
  private val machineMap = Machine.list.sortBy(_.institutionPK).map(m => (m.machinePK.get, m)).toMap

  /** Map to retrieve anonymized name of institution by institutionPK. */
  private val institutionNameMap = Institution.list.map(i => (i.institutionPK.get, i.name)).sortBy(_._2).toMap

  /** Map of machine types and what they are called. */
  private val machineTypeMap = MachineType.list.map(mt => (mt.machineTypePK.get, mt.toName)).toMap

  /** Map of collimator types. */
  private val collimatorTypeMap = MultileafCollimator.list.map(c => (c.multileafCollimatorPK.get, c.model)).toMap

  /** Map of EPID types. */
  private val epidTypeMap = EPID.list.map(e => (e.epidPK.get, e.model)).toMap

  private case class MachCol(header: String, toText: Long => String) {}

  private val colList = Seq(
    MachCol("Institution", (m: Long) => institutionNameMap(machineMap(m).institutionPK)),
    MachCol("Machine", (m: Long) => machineMap(m).id),
    MachCol("Type", (m: Long) => machineTypeMap(machineMap(m).machineTypePK)),
    MachCol("Collimator", (m: Long) => collimatorTypeMap(machineMap(m).multileafCollimatorPK)),
    MachCol("EPID", (m: Long) => epidTypeMap(machineMap(m).epidPK))
  )

  private def machineToColumns(machinePK: Long): String = {
    colList.map(c => c.toText(machinePK)).mkString(",")
  }


  /** The CSV headers for machines. */
  val headerText: String = colList.map(c => c.header).mkString(",")

  /** Map machinePK to its CSV text. */
  val machineCsvMap: Map[Long, String] = machineMap.keys.map(m => (m, machineToColumns(m))).toMap

}
