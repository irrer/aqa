package org.aqa.webrun.wl.isoCheck

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.db.Machine
import org.aqa.db.MachineWL
import org.aqa.db.WinstonLutz
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.acq

import java.util.Date

/**
  * A list of all data columns in the spreadsheet.
  *
  * @param machine       Data is from this machine.
  * @param firstDateTime Acquisition date+time of first slice in series.
  */
case class WLColumnList(machine: Machine, firstDateTime: Date) {

  private def fieldNameOf(al: AttributeList): String = {
    val gantry = Util.angleRoundedTo90(Util.gantryAngle(al)).formatted("%03d")
    val collimator = Util.angleRoundedTo90(Util.collimatorAngle(al)).formatted("%03d")

    val elapsed_ms = acq(al).getTime - firstDateTime.getTime

    val minute = (" " + ((elapsed_ms / (60 * 1000)) % 60).formatted("%d")).takeRight(2)
    val second = ((elapsed_ms / 1000) % 60).formatted("%02d")

    val fn = s"G$gantry C$collimator $minute:$second"
    fn
  }

  private def passFail(wl: WinstonLutz): String = {
    val limit = MachineWL.getMachineWLOrDefault(machine.machinePK.get).passLimit_mm
    if (wl.errorXY_mm.abs < limit) "Passed" else "Failed"
  }

  val columnList: Seq[WLColumn] = {
    val list: Seq[WLColumn] = Seq(
      // @formatter:off
      new WLColumnMachine("machine id",                        machine.getRealId, machine.id),
      new WLColumnText("field name",                           (_: WinstonLutz, al: AttributeList) => fieldNameOf(al)),
      new WLColumnText( "status",                              (wl: WinstonLutz, _: AttributeList) => passFail(wl)),
      new WLColumnAlNegAngle("table angle",                    TagByName.PatientSupportAngle),
      new WLColumnAlAngle("gantry angle",                      TagByName.GantryAngle),
      new WLColumnAlAngle("coll angle",                        TagByName.BeamLimitingDeviceAngle),
      new WlColumnWlNumeric("X offset corrected box-ball",     _.errorX_mm),
      new WlColumnWlNumeric("Y offset corrected box-ball",     _.errorY_mm),
      new WlColumnWlNumeric("XY offset corrected",             _.errorXY_mm),
      new WlColumnWlNumeric("X box center corrected",          _.boxCenterX_mm),
      new WlColumnWlNumeric("Y box center corrected",          _.boxCenterY_mm),
      new WlColumnWlNumeric("X tongue and groove correction",  _ => 0.0),
      new WlColumnWlNumeric("Y tongue and groove correction",  _ => 0.0),
      new WlColumnWlNumeric("X ball center",                   _.ballX_mm),
      new WlColumnWlNumeric("Y ball center",                   _.ballY_mm),
      new WlColumnWlNumeric("box left uncorrected",            _.leftEdge_mm),
      new WlColumnWlNumeric("box right uncorrected",           _.rightEdge_mm),
      new WlColumnWlNumeric("box top uncorrected",             _.topEdge_mm),
      new WlColumnWlNumeric("box bottom uncorrected",          _.bottomEdge_mm),
      new WlColumnWlNumeric("X box center uncorrected",        _.boxCenterX_mm),
      new WlColumnWlNumeric("Y box center uncorrected",        _.boxCenterY_mm),
      new WLColumnAlAnonText("Patient ID",                     TagByName.PatientID, machine. institutionPK : Long),
      new WLColumnAlAnonText("Patient Name",                   TagByName.PatientName, machine.institutionPK),
      new WLColumnAlAnonText("Instance (slice) UID",           TagByName.SOPInstanceUID, machine.institutionPK),
      new WLColumnAlAnonText("Series UID",                     TagByName.SeriesInstanceUID, machine.institutionPK)
      // @formatter:on
    )

    list
  }


}
