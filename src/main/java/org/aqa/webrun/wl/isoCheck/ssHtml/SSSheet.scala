package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.Logging

import scala.xml.Elem

abstract class SSSheet extends Logging {

  val name: String

  def make(): Elem

}
