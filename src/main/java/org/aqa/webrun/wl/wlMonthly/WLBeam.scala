package org.aqa.webrun.wl.wlMonthly

import com.pixelmed.dicom.AttributeList
import org.aqa.db.Machine
import org.aqa.db.WinstonLutz

case class WLBeam(wl: WinstonLutz, al: AttributeList, machine: Machine) {

  val acquisitionDateTime = WLXlsxUtil.acq(al)

}
