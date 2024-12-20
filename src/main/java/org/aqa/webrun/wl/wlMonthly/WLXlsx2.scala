package org.aqa.webrun.wl.wlMonthly

import org.aqa.Logging

object WLXlsx2 extends Logging {

  /*
  def update(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz]): File = {

    val templateFile = new File("""D:\tmp\wl\2024_WinstonLutz_TB5_2024-10-15.xlsx""") // TODO
    val workbook = new XSSFWorkbook(templateFile)

    val wlData = WLData2(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook)

    wlData.update()

    val baseFileName = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd'_'HH-mm")
      val dateText = Util.formatDate(dateFormat, extendedData.output.dataDate.get)
      s"WinstonLutz_$dateText"
    }

    val xlsxFile = new File(extendedData.output.dir, s"$baseFileName.xlsx")

    workbook.write(new FileOutputStream(xlsxFile))
    logger.info(s"Wrote spreadsheet ${xlsxFile.getAbsolutePath}")

    xlsxFile
  }
  */

}
