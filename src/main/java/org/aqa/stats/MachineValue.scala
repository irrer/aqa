package org.aqa.stats

abstract class MachineValue {
  val name: String
  def get(machinePK: Long): Seq[Double]
}
