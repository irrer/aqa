package org.aqa.webrun.wl.isoCheck

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.Logging
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.flip
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.rnd

import java.util.Date

case class WLBeam(wl: WinstonLutz, al: AttributeList) extends Logging {

  /** Analysis A */
  val gantryAngle: Int = WLXlsxUtil.angleRounded(wl.gantryAngle_deg)

  /** Analysis B */
  val collimatorAngle: Int = WLXlsxUtil.angleRounded(wl.collimatorAngle_deg)

  /** Analysis C */
  val tableAngle: Int = WLXlsxUtil.angleRounded(wl.tableAngle_deg.get)

  /** Data date */
  val dataDate: Date = wl.dataDate

  def matches(gantry: Int, collimator: Int, isoTable: Int): Boolean = {
    (gantryAngle == gantry) && (collimatorAngle == collimator) && (tableAngle == isoTable)
  }

  /** Analysis F */
  val caX: Option[Double] = {
    val value = gantryAngle match {
      case 0   => Some(wl.errorX_mm)
      case 180 => Some(-wl.errorX_mm)
      case _   => None
    }
    value.map(rnd)
  }

  /** Analysis G */
  val caY: Option[Double] = {
    val value = gantryAngle match {
      case 90  => Some(wl.errorX_mm)
      case 270 => Some(-wl.errorX_mm)
      case _   => None
    }
    value.map(rnd)
  }

  /** Analysis H */
  val caZ: Option[Double] = Some(-wl.errorY_mm).map(rnd)

  private val radians: Double = Math.toRadians(flip(tableAngle))

  /** cosine of isoTable angle */
  val cos: Double = Math.cos(radians)

  /** sine of isoTable angle */
  val sin: Double = Math.sin(radians)
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
    dbList.flatMap(makePair).sortBy(_.dataDate)
  }

  def findGCT(pairList: Seq[WLBeam], gantry: Int, collimator: Int, isoTable: Int): Option[WLBeam] = {
    pairList.find(pair => pair.matches(gantry, collimator, isoTable))
  }
}
