package org.aqa.webrun.wl.isoCheck

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import org.apache.poi.xssf.usermodel.XSSFCell
import org.aqa.db.WinstonLutz
import org.aqa.AnonymizeUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.numericFormat

/**
  * Generalized column.  This supports putting values into spreadsheet form, both as HTML and XLSX.
  * @param name Column name.
  */
abstract case class WLColumn( //
    name: String
) {

  /**
    * If true, align text left in each HTML cell, otherwise align right.
    */
  val alignLeft: Boolean

  /**
    * Get the name of the column.
    * @return The name of the column.
    */
  def getName: String = name

  /**
    * Format the value as text.
    * @param wl Using this value from database.
    * @param al Using this value from DICOM.
    * @return Text to show user.
    */
  def toText(wl: WinstonLutz, al: AttributeList): String

  /**
    * Format the value as text.
    * @param wl Using this value from database.
    * @param al Using this value from DICOM.
    * @return Text to show user in Preprocess sheet.
    */
  def toPreprocessText(wl: WinstonLutz, al: AttributeList): String

  /**
    * Update the given spreadsheet cell's content.
    *
    * Note: There are multiple setCellValue methods for the XSSFCell object, each with different signatures.
    * This necessitates specifying them in the extended classes, as opposed to doing it in this class.
    *
    * @param cell For this cell.
    * @param wl Using this value from database.
    * @param al Using this value from DICOM.
    */
  def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle columns that have simple text.
  * @param name Column name.
  * @param toVal Convert data to text.
  */
class WLColumnText(name: String, toVal: (WinstonLutz, AttributeList) => String) extends WLColumn(name) {

  override val alignLeft: Boolean = true

  override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(wl, al)

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toText(wl, al)

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toText(wl, al))
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle columns that have a numeric value to be extracted from the DICOM (AttributeList).
  * @param name Column name.
  * @param tag Indicates what DICOM tag to extract the data from.
  */
class WLColumnAlNumeric(name: String, tag: AttributeTag) extends WLColumn(name) {

  override val alignLeft: Boolean = false

  private def toVal(al: AttributeList): Double = DicomUtil.findAllSingle(al, tag).head.getDoubleValues.head

  def toRoundedVal(al: AttributeList): Double = (toVal(al) * 100).round / 100.0

  override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(al).toString

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toVal(al).formatted(numericFormat).trim

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(al))
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle columns that have an angle value to be extracted from the DICOM (AttributeList).
  * @param name Column name.
  * @param tag Indicates what DICOM tag to extract the data from.
  */
class WLColumnAlAngle(name: String, tag: AttributeTag) extends WLColumn(name) {

  override val alignLeft: Boolean = false

  private def toVal(al: AttributeList): Double = DicomUtil.findAllSingle(al, tag).head.getDoubleValues.head

  private def toRoundedVal(al: AttributeList): Int = WLXlsxUtil.angleRounded(toVal(al))

  override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(al).toString

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toRoundedVal(al).toString

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(al))
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle columns that have an angle value to be extracted from the DICOM (AttributeList) and then negated.
  * @param name Column name.
  * @param tag Indicates what DICOM tag to extract the data from.
  */
class WLColumnAlNegAngle(name: String, tag: AttributeTag) extends WLColumn(name) {

  override val alignLeft: Boolean = false

  private def toVal(al: AttributeList): Double = DicomUtil.findAllSingle(al, tag).head.getDoubleValues.head

  private def toRoundedVal(al: AttributeList): Int = (360 - WLXlsxUtil.angleRounded(toVal(al))) % 360

  override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(al).toString

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toRoundedVal(al).toString

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(al))
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle column that have a numeric value extracted from the WinstonLutz database object.
  * @param name Column name.
  * @param toVal Extract value from database object.
  */
class WlColumnWlNumeric(name: String, toVal: WinstonLutz => Double) extends WLColumn(name) {

  override val alignLeft: Boolean = false

  override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(wl).toString

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toVal(wl).formatted(numericFormat).trim

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = cell.setCellValue(toVal(wl))
}

// ---------------------------------------------------------------------------------------------------

/**
  * Handle column that have an anonymized text value to be extracted from the DICOM (AttributeList).
  * @param name Column name.
  * @param tag Indicates what DICOM tag to extract the data from.
  */
class WLColumnAlAnonText(name: String, tag: AttributeTag, institutionPK: Long) extends WLColumn(name) {

  override val alignLeft: Boolean = true

  override def toText(wl: WinstonLutz, al: AttributeList): String = {
    val text = AnonymizeUtil.deAnonymizeAttribute(institutionPK, al.get(tag)) match {
      case Some(attr) => attr.getSingleStringValueOrEmptyString
      case _          => "NA"
    }
    text
  }

  override def toPreprocessText(wl: WinstonLutz, al: AttributeList): String = toText(wl, al)

  override def updateCell(cell: XSSFCell, wl: WinstonLutz, al: AttributeList): Unit = {
    cell.setCellValue(toText(wl, al))
  }
}
