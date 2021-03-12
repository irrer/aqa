package org.aqa.webrun.phase2.phase2csv

import org.aqa.Util
import org.aqa.db.EPID
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.db.Output
import org.aqa.db.User

class PrefixCsv {

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

  /** Map of users. */
  private val userMap = User.list.map(u => (u.userPK.get, u.id)).toMap

  private case class PrefixCol(header: String, toText: Output => String) {}

  private val colList = Seq(
    PrefixCol("Institution", (o: Output) => institutionNameMap(machineMap(o.machinePK.get).institutionPK)),
    PrefixCol("Machine", (o: Output) => machineMap(o.machinePK.get).id),
    PrefixCol("Type", (o: Output) => machineTypeMap(machineMap(o.machinePK.get).machineTypePK)),
    PrefixCol("Collimator", (o: Output) => collimatorTypeMap(machineMap(o.machinePK.get).multileafCollimatorPK)),
    PrefixCol("EPID", (o: Output) => epidTypeMap(machineMap(o.machinePK.get).epidPK)),
    PrefixCol("Acquisition", (o: Output) => Util.standardDateFormat.format(o.dataDate.get)),
    PrefixCol("Analysis", (o: Output) => Util.standardDateFormat.format(o.startDate)),
    PrefixCol("URL", (o: Output) => "https://automatedqualityassurance.org/view/ViewOutput?outputPK=" + o.outputPK.get),
    PrefixCol("Uploaded By", (o: Output) => userMap(o.userPK.get))
  )

  def prefixToText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
