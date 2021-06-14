package org.aqa.webrun.phase2.phase2csv

class VMAT_T2_DR_GSCsv extends VMATCsv {
  override def beamNameMLC() = "T2-DR-GS"
  override def beamNameOpen() = "T2 Open"
  override def centerList_mm() = Seq(-51, -31, -11, 9, 29, 49, 69)
}
