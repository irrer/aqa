package org.aqa.stats

abstract class MachineValue {
  val name: String
  def get(machinePK: Long, beamName: Option[String] = None): Seq[Double]
}
