package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.DbSetup

class VMAT_T3MLCSpeedCsv extends VMATCsv {
  override def beamNameMLC() = "T3MLCSpeed"
  override def beamNameOpen() = "T3 Open"
  override def centerList_mm() = Seq(-51, -31, -11, 9, 29, 49)
}

object VMAT_T3MLCSpeedCsv {
  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new VMAT_T3MLCSpeedCsv).updateFiles()
  }
}

