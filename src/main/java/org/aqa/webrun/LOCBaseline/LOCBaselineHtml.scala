package org.aqa.webrun.LOCBaseline

import org.aqa.Logging
import org.aqa.db.Output
import org.aqa.webrun.ExtendedData

import java.io.File
import scala.xml.Elem

object LOCBaselineHtml extends Logging {

  def makeHtml(extendedData: ExtendedData, runReq: LOCBaselineRunReq): Elem = {
    // TODO
    ???
  }

  def write(extendedData: ExtendedData, runReq: LOCBaselineRunReq) = {

    val content = makeHtml(extendedData, runReq)
    val file = new File(Output.displayFilePrefix + ".html")
    ???

  }

}
