package org.aqa.webrun.machLog

import org.aqa.run.RunReqClass

import scala.xml.Elem

case class MachLogRunReq(machineLogList: Seq[Elem]) extends RunReqClass {}
