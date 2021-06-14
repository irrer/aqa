package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.EPID
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.db.User

/**
  * Container for getting names/ids of institutions and machine information.
  */
class MetadataCache {

  /** Map of machines by machinePK. */
  val machineMap: Map[Long, Machine] = Machine.list.sortBy(_.institutionPK).map(m => (m.machinePK.get, m)).toMap

  /** Map to retrieve anonymized name of institution by institutionPK. */
  val institutionNameMap: Map[Long, String] = Institution.list.map(i => (i.institutionPK.get, i.name)).sortBy(_._2).toMap

  /** Map of machine types and what they are called. */
  val machineTypeMap: Map[Long, String] = MachineType.list.map(mt => (mt.machineTypePK.get, mt.toName)).toMap

  /** Map of collimator types. */
  val collimatorTypeMap: Map[Long, String] = MultileafCollimator.list.map(c => (c.multileafCollimatorPK.get, c.model)).toMap

  /** Map of EPID types. */
  val epidTypeMap: Map[Long, String] = EPID.list.map(e => (e.epidPK.get, e.model)).toMap

  /** Map of users. */
  val userMap: Map[Long, String] = User.list.map(u => (u.userPK.get, u.id)).toMap

}

object MetadataCache {
  lazy val metadataCache = new MetadataCache
}
