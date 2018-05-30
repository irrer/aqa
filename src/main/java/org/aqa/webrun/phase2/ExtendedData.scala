package org.aqa.webrun.phase2

import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Input
import org.aqa.db.Institution
import org.aqa.db.Procedure
import org.aqa.db.User

/**
 * Convenience class for passing around commonly used data, which is basically all of
 * the database objects associated with a given Output object.
 */
case class ExtendedData(output: Output, input: Input, machine: Machine, institution: Institution, procedure: Procedure, user: User)

object ExtendedData {

  def get(output: Output): ExtendedData = {

    val input = Input.get(output.inputPK).get
    val machine = Machine.get(output.machinePK.get).get
    val institution = Institution.get(machine.institutionPK).get
    val procedure = Procedure.get(output.procedurePK).get
    val user = User.get(output.userPK.get).get

    new ExtendedData(output, input, machine, institution, procedure, user)
  }
}