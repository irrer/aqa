package org.aqa.webrun.wl.wlMonthly

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.apache.poi.xssf.usermodel.XSSFCell
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.aqa.db.WinstonLutz
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLRunReq
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.AnonymizeUtil
import org.aqa.Logging

import java.util.Date
import scala.util.Random

case class WLData2(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook: XSSFWorkbook) extends Logging {

  private def acquisition(al: AttributeList): Date = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get

  // get the date+time of the first slice
  private val firstDateTime: Date = runReq.epidList.map(acquisition).min

  private def fieldNameOf(al: AttributeList): String = {
    val gantry = Util.angleRoundedTo90(Util.gantryAngle(al)).formatted("%03d")
    val collimator = Util.angleRoundedTo90(Util.collimatorAngle(al)).formatted("%03d")

    val elapsed_ms = acquisition(al).getTime - firstDateTime.getTime

    val minute = (" " + ((elapsed_ms / (60 * 1000)) % 60).formatted("%d")).takeRight(2)
    val second = ((elapsed_ms / 1000) % 60).formatted("%02d")

    val fn = s"G$gantry C$collimator $minute:$second"
    fn
  }

  private val sheet: XSSFSheet = workbook.getSheetAt(1)

  private def updateTitleRow(): Unit = {
    val row = sheet.getRow(0)

    val dataDateCell = row.getCell(1)
    val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)
    dataDateCell.setCellValue(s"Data Date: $dataDateText")

    val analysisDateCell = row.getCell(2)
    val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)
    analysisDateCell.setCellValue(s"Data Date: $analysisDateText")
  }

  private abstract case class Col( //
  ) {
    def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit
  }

  private class ColText(toVal: (WinstonLutz, AttributeList) => String) extends Col() {
    private def toText(wl: WinstonLutz, al: AttributeList): String = toVal(wl, al)

    override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toText(wl, al))
  }

  private class ColAlDbl(tag: AttributeTag) extends Col() {
    private def toVal(al: AttributeList): Double = DicomUtil.findAllSingle(al, tag).head.getDoubleValues.head

    override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(al))
  }

  private class ColWlDbl(toVal: WinstonLutz => Double) extends Col() {
    override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(wl))
  }

  private class ColAlAnonText(tag: AttributeTag) extends Col() {

    private def toText(wl: WinstonLutz, al: AttributeList): String = {
      val text = AnonymizeUtil.deAnonymizeAttribute(extendedData.institution.institutionPK.get, al.get(tag)) match {
        case Some(attr) => attr.getSingleStringValueOrEmptyString
        case _          => "NA"
      }
      text
    }

    override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toText(wl, al))
  }

  private val random = new Random
  private def rando(): Double = {
    random.nextInt(10) * .1
  }

  private val columnList: Seq[Col] = {
    val list: Seq[Col] = Seq(
      // @formatter:off
      new ColText((_: WinstonLutz, _: AttributeList) => extendedData.machine.getRealId),              //  machine id
      new ColText((_: WinstonLutz, al: AttributeList) => fieldNameOf(al)),                            // field name
      new ColText((wl: WinstonLutz, _: AttributeList) => {                                            //         status
        val limit = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get).passLimit_mm
        if (wl.errorXY_mm < limit) "Passed" else "Failed"
      }),
      new ColAlDbl(TagByName.PatientSupportAngle),   // table yaw
      new ColWlDbl(_.gantryAngle_deg),               // gantry angle
      new ColWlDbl(_.collimatorAngle_deg),           // coll angle
      // new ColWlDbl(_.errorX_mm),                     // X offset corrected box-ball // TODO put back
      // new ColWlDbl(_.errorY_mm),                     // Y offset corrected box-ball // TODO put back
      // new ColWlDbl(_.errorXY_mm),                    // XY offset corrected         // TODO put back
      new ColWlDbl(_ => rando()),               // X offset corrected box-ball // TODO rm
      new ColWlDbl(_ => rando()),               // Y offset corrected box-ball // TODO rm
      new ColWlDbl(_ => rando()),               // XY offset corrected         // TODO rm
      new ColWlDbl(_.boxCenterX_mm),                 // X box center corrected
      new ColWlDbl(_.boxCenterY_mm),                 // Y box center corrected
      new ColWlDbl(_ => 0.0),                        // X tongue and groove correction
      new ColWlDbl(_ => 0.0),                        // Y tongue and groove correction
      new ColWlDbl(_.ballX_mm),                      // X ball center
      new ColWlDbl(_.ballY_mm),                      // Y ball center
      new ColWlDbl(_.leftEdge_mm),                   // box left uncorrected
      new ColWlDbl(_.rightEdge_mm),                  // box right uncorrected
      new ColWlDbl(_.topEdge_mm),                    // box top uncorrected
      new ColWlDbl(_.bottomEdge_mm),                 // box bottom uncorrected
      new ColWlDbl(_.boxCenterX_mm),                 // X box center uncorrected
      new ColWlDbl(_.boxCenterY_mm),                 // Y box center uncorrected
      new ColAlAnonText(TagByName.PatientID),        // Patient ID
      new ColAlAnonText(TagByName.PatientName),      // Patient Name
      new ColAlAnonText(TagByName.SOPInstanceUID),   // Instance (slice) UID
      new ColAlAnonText(TagByName.SeriesInstanceUID) // Series UID
      // @formatter:on
    )

    list
  }

  private def updateContentRow(rowNum: Int, wl: WinstonLutz, al: AttributeList): Unit = {

    Trace.trace(s"Processing row $rowNum")
    val row = sheet.getRow(rowNum - 1) // subtracting 1 converts a row number to a 0-relative row index

    (0 until columnList.size).foreach(i => columnList(i).updateCell(row.getCell(i), wl, al))
  }

  /** First row of Data sheet content (inclusive). */
  private val firstRowNum = 3

  /** Last row of Data sheet content (inclusive). */
  private val lastRowNum = 17

  /**
   * Update all the content rows.  Sort by acquisition time, associate each data set with a row, and then process each row.
   */
  private def updateContentRowList(): Unit = {
    def acquisitionDateTime(al: AttributeList): Date = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get

    val alList = runReq.epidList.sortBy(acquisitionDateTime)

    case class WlAl(rowNum: Int, wl: WinstonLutz, al: AttributeList) {}

    /**
     * Find the WL data that is associated with the given DICOM.
     * @param al DICOM.
     * @return Paired al and wl.
     */
    def wlOfAl(al: AttributeList): Option[WlAl] = {
      val alSop = Util.sopOfAl(al)
      dbList.find(wl => wl.rtimageUID.equals(alSop)) match {
        case Some(w) => Some(WlAl(0, w, al))
        case _ => None
      }
    }

    val wlAlList = alList.flatMap(al => wlOfAl(al))

    val updateList = (firstRowNum to lastRowNum).zip(wlAlList).map(iWlAl => WlAl(iWlAl._1, iWlAl._2.wl, iWlAl._2.al))

    updateList.foreach(wlAl => updateContentRow(wlAl.rowNum, wlAl.wl, wlAl.al))
  }


  def update(): Unit = {
    // turn on auto sizing for all columns
    updateTitleRow()
    updateContentRowList()
  }
}

