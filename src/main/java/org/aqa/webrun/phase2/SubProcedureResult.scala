package org.aqa.webrun.phase2

import scala.xml.Elem
import org.aqa.run.ProcedureStatus

abstract class SubProcedureResult(val summary: Elem, val status: ProcedureStatus.Value, val name: String) {
}
