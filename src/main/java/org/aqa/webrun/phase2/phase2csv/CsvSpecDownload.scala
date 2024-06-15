package org.aqa.webrun.phase2.phase2csv

import edu.umro.ScalaUtil.Trace
import org.apache.logging.log4j.core.util.datetime.FixedDateFormat
import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.HasOutput
import org.aqa.db.Machine
import org.aqa.web.WebUtil
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.util.Date

object CsvSpecDownload extends Logging {

  def download(csvSpec: CsvSpec, response: Response): Unit = {
    val start = System.currentTimeMillis()
    logger.info("Starting custom CSV generation with spec: " + csvSpec)
    // val metadataCache = MetadataCache.metadataCache

    // ---------------------------------------------------------------------------------------------------

    (new PopulateDicomCsv).populateAll() // Get the DICOM column data up to date.

    // @formatter:off
    val list ={
      csvSpec.dataType.machineToCustomCsv(csvSpec)
        .filter(_.text.nonEmpty)
        .filter(l => csvSpec.timeComparator.ok(l.data.asInstanceOf[HasOutput].getOutput.dataDate.get))
    }
    // @formatter:on
    val text = csvSpec.slice(list).map(_.text).mkString("\n")

    response.setStatus(Status.SUCCESS_OK)
    response.setEntity(text, MediaType.TEXT_CSV)

    // set the file name that the client sees
    val fileName = "AQA" + Util.currentTimeAsFileName + ".csv"
    WebUtil.setDownloadName(response, fileName)

    // ---------------------------------------------------------------------------------------------------

    val elapsedText = Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start)
    logger.info("Done with CSV generation.  Elapsed time: " + elapsedText)
  }

  def main(args: Array[String]): Unit = {
    Trace.trace(Config.validated)
    DbSetup.init
    Trace.trace()
    Trace.trace(FixedDateFormat.FixedFormat.ISO8601_BASIC_PERIOD.toString)
    Trace.trace(FixedDateFormat.FixedFormat.ISO8601_BASIC_PERIOD.getPattern)
    Trace.trace()

    val machine = Machine.get(22).get
    val dataType = new WinstonLutzCsv
    val csvSpec = CsvSpec(machine, dataType, beam = ".*", header = false,
      //timeComparator = CsvSpec.defaultTimeComparator,
      timeComparator = CsvSpec.TimeComparator(CsvSpec.TimeComparatorEnum.timeLE, new Date),
      //count = CsvCount(2,0)
      count = CsvSpec.CsvCount())

    val response: Response = null

    download(csvSpec, response)

    MetadataCache.metadataCache.machineMap.size
    Trace.trace()
    Trace.trace()
  }
}
