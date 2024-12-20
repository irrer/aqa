package org.aqa.webrun.wl.wlMonthly.ssHtml

import scala.xml.Elem

abstract class SSSheet {

  val name: String

  def make(): Elem

}
