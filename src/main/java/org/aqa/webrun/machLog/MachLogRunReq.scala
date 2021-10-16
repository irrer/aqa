package org.aqa.webrun.machLog

import org.aqa.db.MachineLog
import org.aqa.run.RunReqClass

case class MachLogRunReq(machineLogList: Seq[MachineLog]) extends RunReqClass {}
