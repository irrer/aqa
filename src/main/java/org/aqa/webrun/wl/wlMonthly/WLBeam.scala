package org.aqa.webrun.wl.wlMonthly

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.Logging
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.rnd

import java.util.Date

case class WLBeam(wl: WinstonLutz, al: AttributeList) extends Logging {
  val gantryAngle: Int = WLXlsxUtil.angleRounded(Util.gantryAngle(al))
  val collimatorAngle: Int = WLXlsxUtil.angleRounded(Util.collimatorAngle(al))
  val tableAngle: Int = WLXlsxUtil.angleRounded(DicomUtil.findAllSingle(al, TagByName.PatientSupportAngle).head.getDoubleValues.head)
  val acquisition: Date = WLXlsxUtil.acq(al)

  def matches(gantry: Int, collimator: Int, table: Int): Boolean = {
    (gantryAngle == gantry) && (collimatorAngle == collimator) && (tableAngle == table)
  }

  val caX: Option[Double] = {
    val value = gantryAngle match {
      case 0   => Some(wl.errorX_mm)
      case 180 => Some(-wl.errorX_mm)
      case _   => None
    }
    value.map(rnd)
  }

  val caY: Option[Double] = {
    val value = gantryAngle match {
      case 90  => Some(wl.errorX_mm)
      case 270 => Some(-wl.errorX_mm)
      case _   => None
    }
    value.map(rnd)
  }

  val caZ: Option[Double] = Some(-wl.errorY_mm).map(rnd)

}

object WLBeam extends Logging {

  // List of DB items and their attribute list
  def makePairList(runReq: WLRunReq, dbList: Seq[WinstonLutz]): Seq[WLBeam] = {
    def makePair(wl: WinstonLutz): Option[WLBeam] = {
      // there will probably be at most one matching file, but occasionally there may be more than one (often if
      // there was an error delivering the first one).  So take the last one delivered.
      runReq.epidList.filter(al => Util.sopOfAl(al).equals(wl.rtimageUID)).sortBy(al => WLXlsxUtil.acq(al)).lastOption match {
        case Some(al) => Some(WLBeam(wl, al))
        case _        => None
      }
    }
    dbList.flatMap(makePair).sortBy(_.acquisition)
  }

  def findGCT(pairList: Seq[WLBeam], gantry: Int, collimator: Int, table: Int): Option[WLBeam] = {
    pairList.find(pair => pair.matches(gantry, collimator, table))
  }
}
