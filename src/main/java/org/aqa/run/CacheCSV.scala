package org.aqa.run

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Institution
import org.aqa.web.WebUtil
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.io.File
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date

/**
 * Support for caching CSV content.  The point is to save content so that the
 * entire data set does not have to be regenerated every time a user asks for
 * the CSV.  The granularity is institution + data type + day.  When cache is
 * invalidated, the granularity is institution + day.
 */
object CacheCSV extends Logging {
  // Map of Institution.institutionPK --> Institution.name (name is anonymized) to be used as a cache
  private val institutionMap = scala.collection.mutable.Map[Long, String]()

  /**
   * Given an institutionPK, get its name.
   *
   * @param institutionPK Primary key of institution.
   * @return Name of institution.
   */
  private def getInstitutionName(institutionPK: Long) = {
    if (institutionMap.contains(institutionPK))
      institutionMap(institutionPK)
    else {
      val name = Institution.get(institutionPK).get.name
      institutionMap.put(institutionPK, name)
      institutionMap(institutionPK)
    }
  }

  private val csvSuffix = ".csv"

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  private def dateToFileName(date: Date) = dateFormat.format(date) + csvSuffix

  /**
   * Remove cached entries for the given institution on the given date.  This is
   * done when old data is analysed at a later date or new analysis of old data
   * makes the cached value invalid.
   *
   * To simplify the interface, a slightly heavy-handed approach is taken where
   * all cached data for that date for that institution is invalidated.  Thi
   * is also a more robust approach, and it is more important to invalidate cache
   * rather than keep some to be ever so slightly more efficient.
   *
   * @param date          Date of cached entry.
   * @param institutionPK For the cache of this institution.
   */
  def invalidateCacheEntries(date: Date, institutionPK: Long): Unit = {
    try {
      val institutionDir = new File(Config.cacheDirFile, getInstitutionName(institutionPK))

      val fileName = dateToFileName(date)

      def invalidateCache(cacheDir: File): Unit = {
        val file = new File(cacheDir, fileName)
        file.delete()
      }

      FileUtil.listFiles(institutionDir).foreach(cacheDir => invalidateCache(cacheDir))
    }
    catch {
      case t: Throwable =>
        logger.error("Unexpected exception while invalidating cache entry for date " + date + " institutionPK: " + institutionPK + " : " + fmtEx(t))
    }
  }

}

abstract class CacheCSV extends Logging {

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

  protected def constructHeaders: String

  protected def firstDataDate(institutionPK: Long): Option[Timestamp]

  /**
   * Allow the custom modification of the CSV content after all the pieces have been assembled.
   *
   * Default is to do nothing.
   *
   * @param assembledCsvText Sorted list of CSV lines without the headers.
   * @return List of CSV lines.
   */
  protected def postProcessing(assembledCsvText: Seq[String]): Seq[String] = {
    assembledCsvText
  }

  protected def getInstitutionPK: Long


  val cacheDir: File = {
    val institutionDir = new File(Config.cacheDirFile, CacheCSV.getInstitutionName(getInstitutionPK))
    new File(institutionDir, cacheDirName())
  }

  /** Convert a file to a date formatted as text.  Do this by removing the
   * suffix.  The file name format is yyyy-MM-dd.csv .
   */
  private def fileToDateText(file: File): String = file.getName.dropRight(CacheCSV.csvSuffix.length)


  /**
   * Given the text representation of a date, construct the file.
   *
   * @param dateText date as <code>dateFormat</code>
   * @return File in cache where the data is persisted..
   */
  private def dateTextToFile(dateText: String) = new File(cacheDir, dateText + CacheCSV.csvSuffix)


  /**
   * Represent the CSV content entry for a given day.
   *
   * @param file File containing content.
   * @param csv  CSV content.
   */
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
        name.endsWith(CacheCSV.csvSuffix) &&
          (CacheCSV.dateFormat.parse(name.dropRight(CacheCSV.csvSuffix.length)).getTime > 0)
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
    val timestamp = new Timestamp(CacheCSV.dateFormat.parse(dateText).getTime)
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

      logger.info("First data date: " + firstTime)
      /** One day in milliseconds. */
      val dayInMs = 24 * 60 * 60 * 1000.toLong

      val hostRef = response.getRequest.getHostRef.toString

      val firstDate = edu.umro.ScalaUtil.Util.roundToDate(firstTime).getTime - (24 * 60 * 60 * 1000)

      // list of required dates as text
      val requiredDayList = (firstDate to System.currentTimeMillis() by dayInMs).map(d => CacheCSV.dateFormat.format(d))

      val allCached = retrieveAllCached()

      val todayText = CacheCSV.dateFormat.format(new Date)
      val todayTimestamp = new Timestamp(CacheCSV.dateFormat.parse(todayText).getTime)

      val nonCached = requiredDayList.diff(allCached.map(_.dateText)).diff(Seq(todayText))

      cacheDir.mkdirs()

      val newlyInstantiated = nonCached.map(nc => instantiateCache(nc, hostRef, institutionPK))

      val todayData = CachedResult(dateTextToFile(todayText), fetchData(todayTimestamp, hostRef, institutionPK))

      val all: Seq[CachedResult] = allCached ++ newlyInstantiated ++ Seq(todayData)

      val assembledAndSorted = all. // all content
        map(_.csv).
        mkString("\n"). // Separate each day's content with a newline.
        replaceAll("\n\n\n*", "\n"). // Remove multiple sequential newlines.
        split("\n"). // break into list of lines
        sorted

      // Perform post-processing and make into a single CSV
      val text: String = (constructHeaders +: postProcessing(assembledAndSorted)).mkString("\n")

      response.setEntity(text, MediaType.TEXT_CSV)
      response.setStatus(Status.SUCCESS_OK)
    }


    val institutionPK = WebUtil.getUser(response.getRequest).get.institutionPK

    // if there is any data, then process it
    firstDataDate(institutionPK) match {
      case Some(firstTime) => processCsv(firstTime, institutionPK)
      case _ =>
    }

  }

}
