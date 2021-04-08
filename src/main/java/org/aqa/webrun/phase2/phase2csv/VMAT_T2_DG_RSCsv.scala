package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.DbSetup

class VMAT_T2_DG_RSCsv extends VMATCsv {
  override def beamNameMLC() = "T2-DG-RS"
  override def beamNameOpen() = "T2 Open"
  override def centerList_mm() = Seq(-51, -31, -11, 9, 29, 49)
}

object VMAT_T2_DG_RSCsv {
  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new VMAT_T2_DG_RSCsv).updateFiles()
  }
}
