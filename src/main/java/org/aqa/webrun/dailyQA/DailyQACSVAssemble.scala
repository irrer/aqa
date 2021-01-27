package org.aqa.webrun.dailyQA

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.BBbyEPIDComposite
import org.aqa.web.WebUtil
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.io.File
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date

abstract class DailyQACSVAssemble {

  /** Name used to distinguish this cached results from others. */
  protected def cacheDirName(): String

  /**
   * Given a date and an institution PK, get the corresponding data as text.
   *
   * @param date          Get for this date.
   * @param hostRef       URL prefix that references host. (https://somehost/)
   * @param institutionPK Indicates which institution.
   * @return Text representation of data.  If no data, then a zero length string.
   */
  protected def fetchData(date: Timestamp, hostRef: String, institutionPK: Long): String

  protected def getHeaders: String

  protected def firstDataDate(institutionPK: Long): Option[Timestamp]

  private val cacheDir = new File(Config.cacheDirFile, cacheDirName())

  private val csvSuffix = ".csv"

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  /** Convert a file to a date formatted as text.  Do this by removing the
   * suffix.  The file name format is yyyy-MM-dd.csv .
   */
  private def fileToDateText(file: File): String = file.getName.dropRight(csvSuffix.length)

  /**
   * Given the text representation of a date, construct the file.
   *
   * @param dateText date as <code>dateFormat</code>
   * @return File in cache where the data is persisted..
   */
  private def dateTextToFile(dateText: String) = new File(cacheDir, dateText + csvSuffix)

  private case class CachedResult(file: File, csv: String) {
    val dateText: String = fileToDateText(file)
  }

  /**
   * Get all of the data from the cache.
   *
   * @return Cached data.
   */
  private def retrieveAllCached(): Seq[CachedResult] = {

    def fileNameIsValidFormat(file: File): Boolean = {
      try {
        val name = file.getName
        name.endsWith(csvSuffix) &&
          (dateFormat.parse(name.dropRight(csvSuffix.length)).getTime > 0)
      }
      catch {
        case _: Throwable => false
      }
    }

    def getCache(file: File): Option[CachedResult] = {
      if (fileNameIsValidFormat(file)) {
        val cachedResult =
          Util.readTextFile(file) match {
            case Right(text) => Some(CachedResult(file, text))
            case _ => None
          }
        cachedResult
      }
      else
        None
    }

    if (cacheDir.isDirectory)
      Util.listDirFiles(cacheDir).flatMap(f => getCache(f))
    else
      Seq()
  }

  /**
   * Create a cache entry by getting the data from the database and then storing it as
   * a file in the cache directory.
   *
   * @param dateText      Date for which to get data.
   * @param hostRef       Host reference for generating URLs.
   * @param institutionPK Indicates which institution this is for.
   * @return A new cache entry that has been written to disk.
   */
  private def instantiateCache(dateText: String, hostRef: String, institutionPK: Long): CachedResult = {
    val timestamp = new Timestamp(dateFormat.parse(dateText).getTime)
    val csvText = fetchData(timestamp, hostRef = hostRef, institutionPK)
    val file = dateTextToFile(dateText)
    Util.writeBinaryFile(file, csvText.getBytes)
    CachedResult(file, csvText)
  }

  /**
   * Assemble the CSV results for the user's institution and put it in the response.  Use previously
   * calculated values as much as possible because they are faster to get.  For any values that are
   * not cached get them from the database and add them to the cache.  Always get today's data form
   * the database because that data is still in a state of flux.
   *
   * The response is where the data is put, but is also used to get the request, which
   * indicates the user, which is used to get the institution.
   *
   * @param response Put the CSV here.
   */
  def assemble(response: Response): Unit = {

    /**
     * Process the data by formatting it into a CSV.
     *
     * @param firstTime Time stamp of first (earliest acquired) data.
     */
    def processCsv(firstTime: Timestamp, institutionPK: Long): Unit = {
      /** One day in milliseconds. */
      val dayInMs = 24 * 60 * 60 * 1000.toLong

      val hostRef = response.getRequest.getHostRef.toString

      // add 1/2 day to avoid daylight savings time issues
      val firstDate = edu.umro.ScalaUtil.Util.roundToDate(firstTime).getTime + (dayInMs / 2)

      // list of required dates as text
      val requiredDayList = (firstDate to System.currentTimeMillis() by dayInMs).map(d => dateFormat.format(d))

      val allCached = retrieveAllCached()

      val todayText = dateFormat.format(new Date)
      val todayTimestamp = new Timestamp(dateFormat.parse(todayText).getTime)

      val nonCached = requiredDayList.diff(allCached.map(_.dateText)).diff(Seq(todayText))

      cacheDir.mkdirs()

      val newlyInstantiated = nonCached.map(nc => instantiateCache(nc, hostRef, institutionPK))

      val todayData = CachedResult(dateTextToFile(todayText), fetchData(todayTimestamp, hostRef, institutionPK))

      val all: Seq[CachedResult] = allCached ++ newlyInstantiated ++ Seq(todayData)

      val text: String =
        getHeaders +
          all. // all content
            map(_.csv).
            mkString("\n"). // Separate each day's content with a newline.
            replaceAll("\n\n\n*", "\n"). // Remove multiple sequential newlines.
            split("\n"). // break into list of lines
            sorted. // sort
            mkString("\n") // make into a single CSV

      response.setEntity(text, MediaType.TEXT_CSV)
      response.setStatus(Status.SUCCESS_OK)
    }


    val institutionPK = WebUtil.getUser(response.getRequest).get.institutionPK

    // if there is any data, then process it
    BBbyEPIDComposite.getEarliestDate(institutionPK) match {
      case Some(firstTime) => processCsv(firstTime, institutionPK)
      case _ =>
    }

  }
}
