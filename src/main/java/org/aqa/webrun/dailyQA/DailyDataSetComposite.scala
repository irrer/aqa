package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import org.aqa.AngleType
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MachineDailyQA
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus

case class DailyDataSetComposite(composite: BBbyEPIDComposite, cbct: BBbyCBCT, machine: Machine, output: Output, bbByEpid: Seq[BBbyEPID], cbctDicomSeries: DicomSeries) extends Logging {

  // unique reference to this set of data
  val checksum: String = {
    val pkSeq = Seq(composite.bbByEPIDCompositePK, cbct.bbByCBCTPK, machine.machinePK, output.outputPK).flatten ++
      bbByEpid.map(_.bbByEPIDPK.get).sorted
    pkSeq.map(_.toString).mkString(" ")
  }

  private def byType(angleType: AngleType.Value) = bbByEpid.filter(b => AngleType.isAngleType(b.gantryAngle_deg, angleType))

  val vertList: Seq[BBbyEPID] = byType(AngleType.vertical)
  val horzList: Seq[BBbyEPID] = byType(AngleType.horizontal)
  val machineDailyQA: MachineDailyQA = MachineDailyQA.getMachineDailyQAOrDefault(machine.machinePK.get)

  /**
    * If the machine supports X-Ray Offsets, then they must NOT be equal to 0,0,-500
    *
    * @return True if ok.
    */
  def xRayOffsetOk(): Boolean = {
    def anyOffsetOk: Boolean = {
      try {
        def offsetOk(offset: Array[Double]): Boolean = (offset.head != 0) || (offset(1) != 0) || (offset(2) != -500)
        val offsetList = bbByEpid.map(_.attributeList.get(TagByName.XRayImageReceptorTranslation).getDoubleValues)
        val anyOk = offsetList.map(offsetOk).reduce(_ || _)
        anyOk
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected error while checking X-Ray Offsets: " + fmtEx(t))
          false
      }
    }
    (!machineDailyQA.requireXRayOffset) || anyOffsetOk
  }

  /**
    * Determine if the values are pass, warning, or failure.  Pass means that all values are less than or equal to the
    * pass limit.  Fail means that at least one value is over the limit.  For CBCT there is a single pass/fail limit.
    * For EPID and composite values there are a pass and warning limits.  If any value exceeds the warning limit then it
    * means failure.  Exceeding the pass limit but not the warning limit means warning.  If all values are under the pass
    * limit the it means pass.  Note that the absolute value of values is considered when comparing to the pass or warning
    * limits.
    */
  val status: ProcedureStatus.ProcedureStatus = {

    def exceedsWarning(d: Option[Double]): Boolean = d.isDefined && d.get.abs > machineDailyQA.warningLimit_mm

    def exceedsPass(d: Option[Double]): Boolean = d.isDefined && d.get.abs > machineDailyQA.passLimit_mm

    val vertZ = (vertList.head.epid3DZ_mm - cbct.err_mm.getZ).abs
    val horzZ = (horzList.head.epid3DZ_mm - cbct.err_mm.getZ).abs

    val sliceThickness = {
      val at = cbct.attributeList.get(TagFromName.SliceThickness)
      if (at != null)
        Some(at.getDoubleValues.head)
      else
        None
    }

    val s = 0 match {

      case _ if cbct.err_mm.getX.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail
      case _ if cbct.err_mm.getY.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail
      case _ if cbct.err_mm.getZ.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail

      case _ if exceedsWarning(composite.xAdjusted_mm) => ProcedureStatus.fail
      case _ if exceedsWarning(composite.yAdjusted_mm) => ProcedureStatus.fail
      case _ if exceedsWarning(composite.zAdjusted_mm) => ProcedureStatus.fail

      case _ if vertZ > machineDailyQA.warningLimit_mm => ProcedureStatus.fail
      case _ if horzZ > machineDailyQA.warningLimit_mm => ProcedureStatus.fail

      case _ if composite.offsetAdjusted_mm.isEmpty                                  => ProcedureStatus.fail
      case _ if composite.offsetAdjusted_mm.get.abs > machineDailyQA.warningLimit_mm => ProcedureStatus.fail

      case _ if composite.xAdjusted_mm.isEmpty => ProcedureStatus.fail
      case _ if composite.yAdjusted_mm.isEmpty => ProcedureStatus.fail
      case _ if composite.zAdjusted_mm.isEmpty => ProcedureStatus.fail

      case _ if composite.tableXlateral_mm.isEmpty      => ProcedureStatus.fail
      case _ if composite.tableYvertical_mm.isEmpty     => ProcedureStatus.fail
      case _ if composite.tableZlongitudinal_mm.isEmpty => ProcedureStatus.fail

      case _ if !xRayOffsetOk => ProcedureStatus.fail

      // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      case _ if exceedsPass(composite.xAdjusted_mm) => ProcedureStatus.warning
      case _ if exceedsPass(composite.yAdjusted_mm) => ProcedureStatus.warning
      case _ if exceedsPass(composite.zAdjusted_mm) => ProcedureStatus.warning

      case _ if vertZ > machineDailyQA.passLimit_mm => ProcedureStatus.warning
      case _ if horzZ > machineDailyQA.passLimit_mm => ProcedureStatus.warning

      case _ if sliceThickness.isDefined && (sliceThickness.get > Config.BBbyCBCTMaximumSliceThickness_mm) => ProcedureStatus.warning

      case _ if composite.offsetAdjusted_mm.get.abs > machineDailyQA.passLimit_mm => ProcedureStatus.warning

      // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      case _ => ProcedureStatus.pass
    }
    //logger.info("DailyDataSetComposite machine: " + machine.id + " status: " + s)
    s
  }
}
