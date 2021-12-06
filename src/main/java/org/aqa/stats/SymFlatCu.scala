package org.aqa.stats

import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.SymmetryAndFlatness

abstract class SymFlatCu extends MachineValue {
  def get(machinePK: Long, cuOf: (SymmetryAndFlatness) => Double): Unit = {
    val outList = Output.getByMachineAndProcedure(machinePK, Procedure.ProcOfPhase2.get.procedurePK.get)
    ???
  }
}
