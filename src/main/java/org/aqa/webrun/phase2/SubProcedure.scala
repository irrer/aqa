package org.aqa.webrun.phase2

import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.db.Machine
import org.restlet.Request
import org.restlet.Response
import org.aqa.webrun.RunRequirements
import org.aqa.db.OutputChild

trait SubProcedure {
  def validate(sessionDir: File, machine: Option[Machine]): Either[String, RunRequirements];
  def run(sessionDir: File, machine: Option[Machine], runRequirements: RunRequirements, request: Request, response: Response): (ProcedureStatus.Value, Seq[OutputChild]);
}