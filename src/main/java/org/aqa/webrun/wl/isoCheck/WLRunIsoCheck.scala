package org.aqa.webrun.wl.isoCheck

import org.aqa.web.WebUtil
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.db.IsoCheck
import org.aqa.db.WinstonLutzGeneric

import scala.xml.Elem

object WLRunIsoCheck extends Logging {

  /**
    * Perform isoCheck processing if the required data is there.
    * @param extendedData metadata
    * @param runReq DICOM slices
    * @param wlList analysis results.
    * @return HTML snippet for access to results.
    */
  def run(extendedData: ExtendedData, runReq: WLRunReq, wlList: Seq[WinstonLutzGeneric]): Elem = {

    val wlMap = new WLMap(wlList)

    val isoCheck = WLIsoCheck.make(wlMap)
    val collimator = WLCollimator.make(wlMap)
    val isoTable = WLIsoTable.make(wlMap)

    // only do this if the required data is there.
    if (isoCheck.isDefined) {

      val wlIsoCheckHTML = org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML(extendedData, runReq, wlMap, isoCheck.get, collimator.get, isoTable)

      val isoCheckDb: IsoCheck = (collimator, isoTable) match {
        case (Some(col), Some(table)) =>
          IsoCheck(
            isoCheckPK = None,
            outputPK = extendedData.outputPK,
            dX_mm = Some(table.get_dXT__0_Optimized),
            dZ_mm = Some(table.get_dZT__0_Optimized),
            tableX_mm = Some(table.get_IsoTable_X_Optimized),
            tableZ_mm = Some(table.get_IsoTable_Z_Optimized),
            collX_mm = Some(col.get_Coll_X_Optimized),
            collZ_mm = Some(col.get_Coll_Z_Optimized)
          )

        case (Some(col), _) =>
          IsoCheck(
            isoCheckPK = None,
            outputPK = extendedData.outputPK,
            dX_mm = None,
            dZ_mm = None,
            tableX_mm = None,
            tableZ_mm = None,
            collX_mm = Some(col.get_Coll_X_Optimized),
            collZ_mm = Some(col.get_Coll_Z_Optimized)
          )
        case (_, Some(table)) =>
          IsoCheck(
            isoCheckPK = None,
            outputPK = extendedData.outputPK,
            dX_mm = Some(table.get_dXT__0_Optimized),
            dZ_mm = Some(table.get_dZT__0_Optimized),
            tableX_mm = Some(table.get_IsoTable_X_Optimized),
            tableZ_mm = Some(table.get_IsoTable_Z_Optimized),
            collX_mm = None,
            collZ_mm = None
          )
        case (_, _) =>
          IsoCheck(
            isoCheckPK = None,
            outputPK = extendedData.outputPK,
            dX_mm = None,
            dZ_mm = None,
            tableX_mm = None,
            tableZ_mm = None,
            collX_mm = None,
            collZ_mm = None
          )
      }

      isoCheckDb.insert
      logger.info("Inserted IsoCheck row into database.")

      val htmlRef = wlIsoCheckHTML.mainPage()

      val elem = {
        val nl = WebUtil.titleNewline
        val title = s"""To re-calculate spreadsheet in Excel, use$nl CTRL-ALT-F9,and then run the solver on$nl each of the Analysis and Collimator sheets."""
        <div style="text-align: center; border: 2px solid #777777; margin-top:10px;" title={title}>
          <div style="text-align: center; border: 1px solid lightgrey; margin10px;">
            {htmlRef}
          </div>
        </div>
      }

      elem
    } else {
      // not a isoCheck data set
      <span></span>
    }
  }

}
