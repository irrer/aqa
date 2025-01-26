package org.aqa.webrun.wl.isoCheck
import org.apache.poi.xssf.streaming.SXSSFSheet

import scala.xml.Elem

case class WLSheet(sheet: SXSSFSheet, html: Elem) {}
