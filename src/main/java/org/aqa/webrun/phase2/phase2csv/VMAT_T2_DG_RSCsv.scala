package org.aqa.webrun.phase2.phase2csv

class VMAT_T2_DG_RSCsv extends VMATCsv {
  override def beamNameMLC() = "T2-DG-RS"
  override def beamNameOpen() = "T2 Open"
  override def centerList_mm() = Seq(-51, -31, -11, 9, 29, 49, 69)
}
