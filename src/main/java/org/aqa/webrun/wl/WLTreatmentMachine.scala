package org.aqa.webrun.wl


import edu.umro.ScalaUtil.Logging

import scala.collection.mutable.ArrayBuffer
import scala.xml.Node

class WLTreatmentMachine(document: Node) extends Logging {

  private val valueText = new ArrayBuffer[String]

  private def logText(name: String, value: String) = valueText += (name + ": " + value)

  private def mainText(name: String): String = {
    val value = (document \ name).head.text
    logText(name, value)
    value
  }

  private def mainTextOpt(name: String): Option[String] = {
    try {
      Some(mainText(name))
    } catch {
      case _: Throwable => None
    }
  }

  val MachineId: String = mainText("MachineId")
  val MachineName: String = mainText("MachineName")
  val DeviceSerialNumber: Option[String] = mainTextOpt("DeviceSerialNumber")
  val XCorrected: Double = mainText("XCorrected").toDouble
  val YCorrected: Double = mainText("YCorrected").toDouble

  override def toString: String = {
    valueText.foldLeft("TreatmentMachine Configuration values:")((b, t) => b + "\n    " + t)
  }

  logger.info(toString)
}
