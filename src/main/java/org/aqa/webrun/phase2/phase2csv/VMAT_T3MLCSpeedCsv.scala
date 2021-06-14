package org.aqa.webrun.phase2.phase2csv

class VMAT_T3MLCSpeedCsv extends VMATCsv {
  override def beamNameMLC() = "T3MLCSpeed"
  override def beamNameOpen() = "T3 Open"
  override def centerList_mm() = Seq(-45, -15, 15, 45)
}
