package org.aqa.webrun.wl.wlMonthly

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.Logging
import org.aqa.webrun.wl.WLRunReq

import java.util.Date

case class WLPairDbAl(wl: WinstonLutz, al: AttributeList) extends Logging {
  private val gantryAngle: Int = WLXlsxUtil.angleRounded(Util.gantryAngle(al))
  private val collimatorAngle: Int = WLXlsxUtil.angleRounded(Util.collimatorAngle(al))
  private val tableAngle: Int = WLXlsxUtil.angleRounded(DicomUtil.findAllSingle(al, TagByName.PatientSupportAngle).head.getDoubleValues.head)
  val acquisition: Date = WLXlsxUtil.acq(al)

  def matches(gantry: Int, collimator: Int, table: Int): Boolean = {
    (gantryAngle == gantry) && (collimatorAngle == collimator) && (tableAngle == table)
  }
}

object WLPairDbAl extends Logging {

  // List of DB items and their attribute list
  def makePairList(runReq: WLRunReq, dbList: Seq[WinstonLutz]): Seq[WLPairDbAl] = {
    def makePair(wl: WinstonLutz): Option[WLPairDbAl] = {
      // there will probably be at most one matching file, but occasionally there may be more than one (often if
      // there was an error delivering the first one).  So take the last one delivered.
      runReq.epidList.filter(al => Util.sopOfAl(al).equals(wl.rtimageUID)).sortBy(al => WLXlsxUtil.acq(al)).lastOption match {
        case Some(al) => Some(WLPairDbAl(wl, al))
        case _        => None
      }
    }
    dbList.flatMap(makePair).sortBy(_.acquisition)
  }

  def findGCT(pairList: Seq[WLPairDbAl], gantry: Int, collimator: Int, table: Int): Option[WLPairDbAl] = {
    pairList.find(pair => pair.matches(gantry, collimator, table))
  }
}
