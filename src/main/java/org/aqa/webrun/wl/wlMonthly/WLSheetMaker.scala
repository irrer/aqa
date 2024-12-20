package org.aqa.webrun.wl.wlMonthly

import scala.xml.Elem

trait WLSheetMaker {

  /** Name of sheet. */
  val sheetName: String

  /**
    * Make the sheet(s).
    *
    * @return List of sheets.
    */
  def make(): Elem

}
