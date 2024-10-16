/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DateTimeAttribute
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.Utility
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVParser

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Polygon
import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.image.RenderedImage
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Properties
import java.util.TimeZone
import javax.imageio.ImageIO
import javax.vecmath.Matrix4d
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Vector4d
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter
import scala.xml.XML

object Util extends Logging {

  val aqaDomain = "automatedqualityassurance.org"
  val aqaUrl: String = "https://www." + aqaDomain + "/"
  val machineConfigDirEnvName = "machine_configDir"
  val machineIdEnvName = "machine_id"
  val institutionIdEnvName = "institution_id"
  val dicomFileNameSuffix = ".dcm"

  private val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

  def timeAsFileName(date: Date): String = timeAsFileNameFormat.format(date)

  def currentTimeAsFileName: String = timeAsFileName(new Date)

  /** Standard date format */
  val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  val spreadsheetDateFormat = new SimpleDateFormat("yyyy-MM-dd' 'HH:mm:ss")

  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date): String = timeHumanFriendlyFormat.format(date)

  def currentTimeHumanFriendly: String = timeHumanFriendlyFormat.format(new Date)

  private val elapsedFormatHours = new SimpleDateFormat("HH:mm:ss")
  private val elapsedFormatMinutes = new SimpleDateFormat("m:ss.SSS")
  private val elapsedFormatSeconds = new SimpleDateFormat("s.SSS")

  def formatDate(format: SimpleDateFormat, date: Date): String = standardDateFormat.synchronized(format.format(date))

  def parseDate(format: SimpleDateFormat, text: String): Date = standardDateFormat.synchronized(format.parse(text))

  def dicomGetTimeAndDate(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
    standardDateFormat.synchronized(DicomUtil.getTimeAndDate(al, dateTag, timeTag))
  }

  def dateTimeToDate(date: Date): Date = {
    val stdText = Util.formatDate(Util.standardDateFormat, date)
    val day = Util.parseDate(Util.standardDateFormat, stdText.replaceAll("T.*", "T00:00:00"))
    day
  }

  def dateTimeToDate(ts: Timestamp): Date = {
    dateTimeToDate(new Date(ts.getTime))
  }

  def elapsedTimeHumanFriendly(time: Long): String = {
    val elapsedMs = time.abs
    val fmt =
      if (elapsedMs >= (60 * 60 * 1000))
        elapsedFormatHours
      else if (elapsedMs >= (60 * 1000))
        elapsedFormatMinutes
      else
        elapsedFormatSeconds
    fmt.format(new Date(elapsedMs))
  }

  /**
   * Global lock for synchronizing all file writes so that only one write is being done at
   * a time (as opposed to being done in parallel).
   */
  private val fileSystemWriteSync = "sync"

  def writeBinaryFile(file: File, data: Array[Byte]): Unit =
    fileSystemWriteSync.synchronized({
      file.delete
      val fos = new FileOutputStream(file)
      fos.write(data)
      fos.flush()
      fos.close()
    })

  def writeFile(file: File, text: String): Unit = writeBinaryFile(file, text.getBytes)

  /**
   * Write the given attribute list to the given file, wrapping it in a synchronized to
   * prevent file write operations being doing in parallel from colliding.
   *
   * @param attributeList DICOM to be written.
   * @param file          Destination file.  If the file already exists it will first be deleted.
   */
  def writeAttributeListToFile(attributeList: AttributeList, file: File, sourceApplication: String = "AQA"): Unit = {
    fileSystemWriteSync.synchronized({
      file.delete
      DicomUtil.writeAttributeListToFile(attributeList, file, sourceApplication)
    })
  }

  /**
   * Wrapper for <code>File.mkdirs</code> (which makes a directory and all of
   * the necessary parent directories) that is thread safe.
   *
   * @param dir Make this directory if it does not already exist.
   * @return True if the directory was created.
   */
  def mkdirs(dir: File): Boolean = fileSystemWriteSync.synchronized(dir.mkdirs())

  def readBinaryFile(file: File): Either[Throwable, Array[Byte]] = {
    try {
      val fis = new FileInputStream(file)
      val buf = new Array[Byte](file.length.toInt)
      fis.read(buf)
      fis.close()
      Right(buf)
    } catch {
      case t: Throwable => Left(t)
    }
  }

  def readTextFile(file: File): Either[Throwable, String] = {
    val result = readBinaryFile(file)
    if (result.isLeft) Left(result.left.get)
    else Right(new String(result.right.get))
  }

  /**
   * Read a DICOM file.
   */
  def readDicomFile(file: File): Either[Throwable, AttributeList] = {
    try {
      val al = new AttributeList
      al.read(file)
      Right(al)
    } catch {
      case t: Throwable => Left(t)
    }
  }

  def getAttrValue(al: AttributeList, tag: AttributeTag): Option[String] = {
    val a = al.get(tag)
    if (a == null)
      None
    else
      Option(a.getSingleStringValueOrNull)
  }

  def isRtplan(al: AttributeList): Boolean = {
    val a = al.get(TagByName.Modality)
    (a != null) && a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("RTPLAN")
  }

  def isRtimage(al: AttributeList): Boolean = {
    val a = al.get(TagByName.Modality)
    (a != null) && a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("RTIMAGE")
  }

  def isCt(al: AttributeList): Boolean = {
    val a = al.get(TagByName.Modality)
    (a != null) && a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("CT")
  }

  def isReg(al: AttributeList): Boolean = {
    val a = al.get(TagByName.Modality)
    (a != null) && a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("REG")
  }

  /**
   * Get the SOPInstanceUID of an attribute list.
   */
  def sopOfAl(al: AttributeList): String = {
    val at = al.get(TagByName.SOPInstanceUID)
    if (at == null) "" else al.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the SeriesInstanceUID of an attribute list.
   */
  def serInstOfAl(al: AttributeList): String = {
    val at = al.get(TagByName.SeriesInstanceUID)
    if (at == null) "" else al.get(TagByName.SeriesInstanceUID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the StudyInstanceUID of an attribute list.
   */
  def studyInstOfAl(al: AttributeList): String = {
    val at = al.get(TagByName.StudyInstanceUID)
    if (at == null) "" else al.get(TagByName.StudyInstanceUID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the PatientID of an attribute list.
   */
  def patientIdOfAl(al: AttributeList): String = {
    val at = al.get(TagByName.PatientID)
    if (at == null) "" else al.get(TagByName.PatientID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the Modality of an attribute list.
   */
  def modalityOfAl(al: AttributeList): String = {
    val at = al.get(TagByName.Modality)
    if (at == null) "" else al.get(TagByName.Modality).getSingleStringValueOrEmptyString
  }

  def collimatorAngle(al: AttributeList): Double = {
    val at = DicomUtil.findAllSingle(al, TagByName.BeamLimitingDeviceAngle).headOption
    if (at.isDefined) at.get.getDoubleValues.head else 0
  }

  def gantryAngle(al: AttributeList): Double = {
    try {
      DicomUtil.findAllSingle(al, TagByName.GantryAngle).head.getDoubleValues.head
    }
    catch {
      case _: Throwable => 0.0
    }
  }

  def gantryAngle(df: DicomFile): Double = gantryAngle(df.attributeList.get)

  case class DateTimeAndPatientId(dateTime: Option[Long], PatientID: Option[String]) {
    override def toString: String = {
      val dt = if (dateTime.isDefined) new Date(dateTime.get) else "none"
      val p = if (PatientID.isDefined) PatientID.get else "none"
      "dateTime: " + dt + "    PatientID: " + p
    }
  }

  private def getTimeAndDate(al: AttributeList): Option[Date] = {
    try {
      Some(DateTimeAttribute.getDateFromFormattedString(al.get(TagByName.AcquisitionDateTime).getSingleStringValueOrNull))
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Get dates and patient ID from attribute list.
   */
  def extractDateTimeAndPatientIdFromDicomAl(attributeList: AttributeList): (Seq[Date], Option[String]) = {

    val PatientID = getAttrValue(attributeList, TagByName.PatientID)

    val dateTimeTagPairList = List(
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.CreationDate, TagByName.CreationTime),
      (TagByName.SeriesDate, TagByName.SeriesTime),
      (TagByName.RTPlanDate, TagByName.RTPlanTime)
    )

    val AcquisitionDateTime = getTimeAndDate(attributeList)

    val dateTimePairs = dateTimeTagPairList.map(dt => DicomUtil.getTimeAndDate(attributeList, dt._1, dt._2))

    val dateList = (AcquisitionDateTime +: dateTimePairs).flatten //.distinct.map(dt => adjustDicomDateByLocalTimeZone(dt))

    (dateList, PatientID)
  }

  /**
   * Sort a list attribute lists in ascending order by date-time.
   */
  def sortByDateTime(alList: Seq[AttributeList]): Seq[AttributeList] = {

    def compareTo(a: AttributeList, b: AttributeList): Boolean = {
      def dt(al: AttributeList) = extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime

      dt(a) < dt(b)
    }

    alList.sortWith(compareTo)
  }

  /**
   * Get the date and time for building an Output.
   */
  def getOutputDateTime(alList: Seq[AttributeList]): Option[Long] = {
    val dateTimeTags = Seq(
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.CreationDate, TagByName.CreationTime),
      (TagByName.SeriesDate, TagByName.SeriesTime)
    )

    def earliest(dateTag: AttributeTag, timeTag: AttributeTag): Option[Long] = {
      val seq = alList.flatMap(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).map(d => d.getTime)
      if (seq.isEmpty) None else Some(seq.min)
    }

    val list = dateTimeTags.flatMap(dt => earliest(dt._1, dt._2))
    list.headOption
  }

  /**
   * Get the patient ID for building an Output.
   */
  def getOutputPatientId(al: AttributeList): Option[String] = {
    val at = al.get(TagByName.PatientID)
    if ((at != null) && (at.getSingleStringValueOrNull != null))
      Some(at.getSingleStringValueOrNull)
    else
      None
  }

  /**
   * Given a DICOM date/time, adjust it by the local time zone amount.
   */
  def adjustDicomDateByLocalTimeZone(date: Date): Date = {
    new Date(date.getTime - TimeZone.getDefault.getRawOffset)
  }

  /**
   * Given a DICOM date/time, adjust it by the local time zone amount.
   */
  def adjustDicomDateByLocalTimeZone(ms: Long): Long = {
    ms - TimeZone.getDefault.getRawOffset
  }

  /**
   * Get the jar file that contains this class.
   */
  lazy val thisJarFile: File = {
    val clss = this.getClass // Pick current jar.  For a different jar pick a class from that jar.
    new File(clss.getProtectionDomain.getCodeSource.getLocation.toURI)
  }

  /**
   * Get a Long from a string if possible
   */
  def stringToLong(text: String): Option[Long] = {
    try {
      val l: Long = text.toLong
      Some(l)
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Get a Long from a string if possible
   */
  def stringToLong(text: Option[String]): Option[Long] = {
    text match {
      case Some(t) => stringToLong(t)
      case _ => None
    }
  }

  val buildProperties: Properties = {

    def wrapperByJar: Seq[String] = {
      try {
        val aqaInstallDir = new File(System.getenv("AQAJAR")).getParentFile
        val yajswDir = aqaInstallDir.listFiles.toSeq.filter(f => f.getName.toLowerCase.startsWith("yajsw")).head
        val confDir = new File(yajswDir, "conf")
        val wrapperFile = new File(confDir, "wrapper.conf")
        val wrapperFileName = wrapperFile.getAbsolutePath
        logger.info("Found wrapper file via AQAJAR environment variable at: " + wrapperFileName)
        Seq(wrapperFileName)
      } catch {
        case _: Throwable => Seq[String]()
      }
    }

    val prop = new Properties
    val fileNameList = Seq("""wrapper.conf""", """yajsw-stable-12.12\conf\wrapper.conf""", """src\main\resources\wrapper.conf""", """..\..\main\resources\wrapper.conf""") ++ wrapperByJar

    val fileList = fileNameList.map(name => new File(name))

    val propFile = {
      try {
        val file = fileList.find(f => f.exists)
        if (file.isEmpty) println("Problem finding properties file that exists from list:" + fileList.mkString("\n", "\n", "\n"))
        file
      } catch {
        // Do not use logging to print an error message because when getting
        // properties it is quite possible that logging has not yet been set up.
        case e: Exception =>
          println("Problem loading properties file List:" + fileList.mkString("\n", "\n", "\n") + " : " + fmtEx(e))
          None
      }

    }

    try {
      if (propFile.isDefined) {
        prop.load(new FileInputStream(propFile.get))
        val propText = prop.keySet().toArray().map(key => s"$key: ${prop.get(key)}").mkString("\n    ")
        logger.info(s"Loaded property file ${propFile.get.getAbsolutePath}\n    $propText")
      }
    } catch {
      // Do not use logging to print an error message because when getting
      // properties it is quite possible that logging has not yet been set up.
      case e: Exception => println("Problem reading properties file " + propFile + " : " + fmtEx(e))
    }
    prop
  }

  /**
   * Safely get the list of files in a directory.
   */
  def listDirFiles(dir: File): Seq[File] = {
    val list =
      try {
        fileSystemWriteSync.synchronized {
          dir.listFiles.toSeq
        }
      } catch {
        case _: Throwable => null
      }

    if (list != null) list.toIndexedSeq else Seq[File]()
  }

  def deleteFileTreeSafely(dir: File): Unit = {
    try {
      fileSystemWriteSync.synchronized {
        Utility.deleteFileTree(dir)
      }
    } catch {
      case t: Throwable => logger.warn("Unable to delete directory " + dir.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
   * Get the file name without the extension, as in:   foo.bar ==> foo
   */
  def fileBaseName(file: File): String = {
    file.getName match {
      case name if name.contains('.') => name.substring(0, name.lastIndexOf('.'))
      case name => name
    }
  }

  /**
   * Given the text for a single CVS cell, return the properly formatted text for CSV.
   */
  def textToCsv(text: String): String = {
    if (text.contains('"') || text.contains(',')) {
      '"' + text.replaceAll("\"", "\"\"") + '"'
    } else text
  }

  /**
   * Given CSV content, convert it to a single dimensional array of text strings.
   *
   * If the CSV is incorrectly formatted, then this will throw an exception.
   *
   * @param csv CSV content.  May have multiple lines.
   * @return Array o text strings ordered as they were in the input.
   */
  def csvToText(csv: String): Seq[String] =
    // Do this synchronized because it is not certain that the library is thread safe.
    CSVFormat.DEFAULT.synchronized {
      val parsed = CSVParser.parse(csv, CSVFormat.DEFAULT)
      val textSeq = scala.collection.mutable.ArrayBuffer[String]()
      parsed.forEach(line => {
        line.forEach(col => textSeq.append(new String(col.getBytes)))
      })
      textSeq.toIndexedSeq
    }

  /**
   * Make a new file reference, putting the given file in the given directory.
   */
  def reDir(file: File, dir: File): File = {
    new File(dir, file.getName)
  }

  /**
   * Remove the suffix from the given file name.  Examples:
   *
   * foo.TXT -> foo
   * bar.    -> bar
   * bar..   -> bar.
   * goo.foo.roo -> goo.foo
   */
  //noinspection RegExpRedundantEscape
  def removeFileNameSuffix(fileName: String): String = fileName.replaceAll("\\.[^\\.]*$", "")

  //  def changeFileNameSuffix(file: File, suffix: String): String = {
  //    removeFileNameSuffix(file.getName) + suffix
  //  }

  /**
   * Specified by 300A,00B8  RTBeamLimitingDeviceType.
   */
  val xOrientation: Seq[String] = Seq("X", "ASYMX", "MLCX", "MLCX1", "MLCX2")

  /**
   * Specified by 300A,00B8  RTBeamLimitingDeviceType.
   */
  val yOrientation: Seq[String] = Seq("Y", "ASYMY", "MLCY")

  def specifiesX(devType: String): Boolean = xOrientation.contains(devType.toUpperCase)

  def specifiesY(devType: String): Boolean = yOrientation.contains(devType.toUpperCase)

  /**
   * Write a PNG file in a thread safe way.
   */
  def writePngX(im: RenderedImage, pngFile: File): Unit =
    fileSystemWriteSync.synchronized({
      pngFile.delete
      val fos = new FileOutputStream(pngFile)
      ImageIO.write(im, "png", fos)
      try {
        fos.flush()
        fos.close()
      } catch {
        case t: Throwable => logger.warn("problem closing file output stream for PNG file " + pngFile.getAbsolutePath + " : " + t)
      }
    })

  /**
   * Write a PNG file in a thread safe way.
   *
   * First render the image into a byte array.  This can take time and is thread safe, so it does not
   * need to be done in a synchronized way.  Then write the byte array to a file, which does need to
   * be done in a synchronized way.
   */
  def writePng(im: RenderedImage, pngFile: File): Unit = {
    try {
      val baos = new ByteArrayOutputStream
      ImageIO.write(im, "png", baos)
      baos.flush()
      baos.close()
      writeBinaryFile(pngFile, baos.toByteArray)
    } catch {
      case t: Throwable => logger.warn("problem writing file for PNG file " + pngFile.getAbsolutePath + " : " + t)
    }
  }

  /**
   * Write a JPG / JPEG file in a thread safe way.
   */
  def writeJpg(im: RenderedImage, jpegFile: File): Unit =
    fileSystemWriteSync.synchronized({
      jpegFile.delete
      ImageIO.write(im, "jpg", new FileOutputStream(jpegFile))
    })

  /**
   * Round the angle to the closest 90 degree angle.
   *
   * @param angleInDegrees Angle to round off.
   */
  def angleRoundedTo90(angleInDegrees: Double): Int = {
    ((((angleInDegrees % 360.0) + 360.0) / 90.0).round.toInt % 4) * 90
  }

  /**
   * Convert arbitrary angle in degrees to a number 360 < degrees >= 0
   */
  def modulo360(degrees: Double): Double = {
    ((degrees % 360.0) + 360.0) % 360.0
  }

  /**
   * Add graticules to the given image.
   */
  def addGraticules(image: BufferedImage, x2Pix: Double => Double, y2Pix: Double => Double, pix2X: Double => Double, pix2Y: Double => Double, graticuleColor: Color): Unit = {

    val graphics = ImageUtil.getGraphics(image)

    val lightlyDashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(0.5f, 4), 0)

    val xMin = 0
    val yMin = 0
    val xMax = image.getWidth - 1
    val yMax = image.getHeight - 1
    val xMiddle = xMax / 2
    val yMiddle = yMax / 2

    //    val min = translator.pix2Iso(xMin, yMin)
    //    val max = translator.pix2Iso(xMax, yMax)

    val xGrat = ImageUtil.graticule(pix2X(xMin), pix2X(xMax), 8)
    val yGrat = ImageUtil.graticule(pix2Y(yMin), pix2Y(yMax), 8)

    val gratMajorLength = 10
    val gratMinorLength = 5

    graphics.setColor(graticuleColor)
    ImageUtil.setSolidLine(graphics)

    // draw borders and center lines
    graphics.drawLine(xMin, yMin, xMin, yMax)
    graphics.drawLine(xMax, yMin, xMax, yMax)
    graphics.drawLine(xMin, yMin, xMax, yMin)
    graphics.drawLine(xMin, yMax, xMax, yMax)
    graphics.drawLine(xMin, yMiddle, xMax, yMiddle)
    graphics.drawLine(xMiddle, yMin, xMiddle, yMax)

    //    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt
    //    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt

    val offset = 1 // Offset of tic mark from number in pixels.

    def closeTogether(aPix: Double, bPix: Double) = (aPix - bPix).abs < 2

    val numMinorTics = 4 // number of minor tics between major tics
    val xMinorInc = (x2Pix(xGrat(1)) - x2Pix(xGrat.head)) / (numMinorTics + 1)
    val yMinorInc = (y2Pix(xGrat(1)) - y2Pix(yGrat.head)) / (numMinorTics + 1)

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = graphics.drawLine(x1.round.toInt, y1.round.toInt, x2.round.toInt, y2.round.toInt)

    // draw top, bottom, and center graticules
    for (xIso <- xGrat) {
      val x = x2Pix(xIso)
      val minorStart = if (xIso == xGrat.head) -numMinorTics else 1
      val text = xIso.round.toInt.toString + " "

      if (!closeTogether(x, xMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        drawLine(x, yMin, x, yMax)
        ImageUtil.setSolidLine(graphics)
      }

      // top
      drawLine(x, yMin, x, yMin + gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMin, x + (mt * xMinorInc), yMin + gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMin + gratMajorLength + offset, text, 270) // draw number corresponding to major graticule
      // bottom
      drawLine(x, yMax, x, yMax - gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMax, x + (mt * xMinorInc), yMax - gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMax - gratMajorLength - offset, text, 90) // draw number corresponding to major graticule
      // center horizontal
      drawLine(x, yMiddle - gratMajorLength, x, yMiddle + gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMiddle - gratMinorLength, x + (mt * xMinorInc), yMiddle + gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMiddle - gratMajorLength - offset, text, 90) // draw number corresponding to major graticule
    }

    // draw left and right graticules
    for (yIso <- yGrat) {
      val y = y2Pix(yIso).round.toInt
      val minorStart = if (yIso == yGrat.head) -numMinorTics else 1
      val text = yIso.round.toInt.toString + " "

      if (!closeTogether(y, yMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        drawLine(xMin, y, xMax, y)
        ImageUtil.setSolidLine(graphics)
      }

      val textWidth = ImageText.getTextDimensions(graphics, text).getWidth.round.toInt
      // left
      drawLine(xMin, y, xMin + gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMin, y + (mt * yMinorInc), xMin + gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMin + gratMajorLength + offset, y, text, 0) // draw number corresponding to major graticule
      // right
      drawLine(xMax, y, xMax - gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMax, y + (mt * yMinorInc), xMax - gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMax - (gratMajorLength + offset + textWidth), y, text, 0) // draw number corresponding to major graticule
      // center vertical
      drawLine(xMiddle - gratMajorLength, y, xMiddle + gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMiddle - gratMinorLength, y + (mt * yMinorInc), xMiddle + gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMiddle + gratMajorLength + offset, y, text, 0) // draw number corresponding to major graticule off to the right
    }

  }

  /**
   * Add graticules to the given image.
   */
  def addGraticules(image: BufferedImage, translator: IsoImagePlaneTranslator, color: Color): Unit = {

    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt

    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt

    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt

    def pix2Y(yPix: Double) = translator.pix2Iso(0, yPix).getY.round.toInt

    addGraticules(image, x2Pix, y2Pix, pix2X, pix2Y, color)
  }

  /**
   * Add graticules to the given image, reversing the Y-axis.
   */
  def addGraticulesNegY(image: BufferedImage, translator: IsoImagePlaneTranslator, color: Color): Unit = {

    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt

    def y2Pix(yIso: Double) = translator.iso2Pix(0, -yIso).getY.round.toInt

    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt

    def pix2Y(yPix: Double) = -translator.pix2Iso(0, yPix).getY.round.toInt

    addGraticules(image, x2Pix, y2Pix, pix2X, pix2Y, color)
  }

  /**
   * Number f pixels from edge to put axis arrows.
   */
  private val axisOffsetFromEdge = 40

  private def addAxisLabels(image: BufferedImage, horzLabel: String, vertLabel: String, color: Color, top: Boolean = true, bottom: Boolean = true, left: Boolean = true, right: Boolean = true)
  : Unit = {

    val lineThickness: Float = 3
    val arrowLength = 5
    val arw = 3
    val textPointSize = 16
    val offset = 3

    val graphics = ImageUtil.getGraphics(image)
    graphics.setColor(color)
    ImageUtil.setSolidLine(graphics)

    graphics.setStroke(new BasicStroke(lineThickness))
    ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)

    def trans(): Unit = {
      val leftX = image.getWidth / 4
      val rightX = leftX * 3
      val y = image.getHeight - axisOffsetFromEdge

      graphics.drawLine(leftX, y, rightX, y) // main line
      // arrow heads
      if (left) {
        graphics.drawLine(leftX, y, leftX + arrowLength, y - arw)
        graphics.drawLine(leftX, y, leftX + arrowLength, y + arw)
      }
      if (right) {
        graphics.drawLine(rightX, y, rightX - arrowLength, y - arw)
        graphics.drawLine(rightX, y, rightX - arrowLength, y + arw)
      }

      val yText = {
        val textDim = ImageText.getTextDimensions(graphics, horzLabel)
        image.getHeight - axisOffsetFromEdge - (textDim.getHeight / 2 + 3)
      }
      ImageText.drawTextCenteredAt(graphics, image.getWidth / 3, yText, horzLabel)
    }

    def axial(): Unit = {
      val topY = image.getHeight / 4
      val bottomY = topY * 3
      val x = axisOffsetFromEdge

      graphics.drawLine(x, topY, x, bottomY) // main line
      // arrow heads
      if (top) {
        graphics.drawLine(x, topY, x + arw, topY + arrowLength)
        graphics.drawLine(x, topY, x - arw, topY + arrowLength)
      }

      if (bottom) {
        graphics.drawLine(x, bottomY, x - arw, bottomY - arrowLength)
        graphics.drawLine(x, bottomY, x + arw, bottomY - arrowLength)
      }

      val xText = axisOffsetFromEdge + offset

      val yText = image.getHeight / 3
      graphics.drawString(vertLabel, xText, yText)
    }

    trans()
    axial()
  }

  def addAxialAndTransverse(image: BufferedImage): Unit = {
    addAxisLabels(image, "Transverse", "Axial", Color.gray)
  }

  /**
   * Convert the given DICOM to a byte array (aka: serialize) and return the byte array.  If there is an error return a string describing it.
   */
  def dicomToBytes(attributeList: AttributeList): Either[String, Array[Byte]] = {
    try {
      // val j = Util.DEFAULT_TRANSFER_SYNTAX
      val transferSyntax: String = {
        val deflt = TransferSyntax.ImplicitVRLittleEndian
        val a = attributeList.get(TagByName.TransferSyntaxUID)
        if (a != null) {
          val ts = a.getStringValues
          if (ts.nonEmpty) ts.head else deflt
        } else deflt
      }

      val baos = new ByteArrayOutputStream
      attributeList.write(baos, transferSyntax, true, true)
      baos.flush()
      Right(baos.toByteArray)
    } catch {
      case t: Throwable =>
        val msg = "Error converting DICOM to bytes: " + fmtEx(t)
        logger.warn(msg)
        Left(msg)
    }

  }

  /**
   * Given two colors and a palette size, return a list of colors of the
   * given size that steps between the given colors.
   */
  def colorPallette(colorA: Color, colorB: Color, size: Int): IndexedSeq[Color] = {
    val step = {
      if (size == 1) 1.0 else (size - 1).toDouble
    }

    val stepR = (colorB.getRed - colorA.getRed) / step
    val stepG = (colorB.getGreen - colorA.getGreen) / step
    val stepB = (colorB.getBlue - colorA.getBlue) / step

    def toColor(c: Double): Int = {
      val ci = c.round.toInt
      ci match {
        case _ if ci < 0 => 0
        case _ if ci > 0xff => 0xff
        case _ => ci
      }
    }

    (0 until size).map(i => {
      val r = toColor(colorA.getRed + i * stepR)
      val g = toColor(colorA.getGreen + i * stepG)
      val b = toColor(colorA.getBlue + i * stepB)
      new Color(r, g, b)
    })
  }

  /**
   * Format a double with enough precision for most needs and not using the exponential format.  This works for numbers in
   * of the magnitude range of e+8 to e-8.  Beyond that and they start to be annoyingly long strings.
   */
  def fmtDbl(dbl: Double): String = {
    val text = dbl.formatted("%6.3e").toDouble.formatted("%32.16f").replaceAll("0*$", "").trim
    if (text.endsWith(".")) text + "0" else text
  }

  def fmtDbl(dbl: Option[Double]): String = {
    if (dbl.isDefined) fmtDbl(dbl.get) else "None"
  }

  //  /**
  //   * Calculate the standard deviation of a list of values.
  //   */
  //  def stdDev(list: Seq[Double]): Double = {
  //    val mean = list.sum / list.size
  //    val sumSq = list.map(d => (d - mean) * (d - mean)).sum
  //    //val variance = sumSq / mean
  //    val variance = sumSq / list.size
  //    val sd = Math.sqrt(variance)
  //    sd
  //  }

  /**
   * Remove a possible artificial beam prefix.
   */
  def normalizeBeamName(name: String): String = {
    val norm =
      if (name.matches("^[0-9][0-9]:.*"))
        name.substring(3)
      else {
        if (name.matches("^[0-9][0-9][abAB]:.*")) {
          name.substring(4)
        } else
          name
      }
    norm.trim
  }

  /**
   * Get the normalized beam name from the given attribute list.  Beam names are sometimes prefixed with "NN:", where NN is
   * a pair of digits that cause the beams to be sorted by the Eclipse planning system in the preferred delivery order.
   */
  def normalizedBeamName(al: AttributeList): String = {
    normalizeBeamName(al.get(TagByName.BeamName).getSingleStringValueOrEmptyString)
  }

  /**
   * Determine if two beam names are equal.  Both do normalization and consider that one or both were truncated.  Some
   * treatment planning systems truncates them from 16 characters to 13.
   */
  def beamNamesEqual(a: String, b: String): Boolean = {
    val aa = normalizeBeamName(a)
    val bb = normalizeBeamName(b)
    val len = Math.min(aa.length, bb.length)

    def subStr(text: String) = text.substring(0, Math.min(text.length, len))

    subStr(aa).equalsIgnoreCase(subStr(bb))
  }

  /**
   * Get the number of the beam referenced by the given RTIMAGE.
   *
   * If there is no referenced beam, then this will throw an exception.
   *
   * @param rtImage DICOM RTIMAGE.
   * @return Beam number.
   */
  def beamNumber(rtImage: AttributeList): Int =
    DicomUtil.findAllSingle(rtImage, TagByName.ReferencedBeamNumber).head.getIntegerValues.head

  /**
   * Show which jar file is being used to ensure that we have the right version of the software.
   */
  def showJarFile(any: Any): Unit = {

    def description(file: File): String = {
      val fullPath = file.getAbsolutePath
      val exists = file.exists
      val isNormal = file.isFile
      val len = if (file.exists) file.length.toString else "not available"
      val modified = if (exists) new Date(file.lastModified).toString else "not available"
      val desc = {
        "jar file " +
          "\n    path: " + fullPath +
          "\n    exists: " + exists +
          "\n    normal file: " + isNormal +
          "\n    file modification date: " + modified +
          "\n    length: " + len
      }
      desc
    }

    val msg =
      try {
        description(edu.umro.ScalaUtil.Util.getJarFile(any).get)
      } catch {
        case _: Throwable => "Could not determine jar file being used."
      }

    println(msg)
    logger.info(msg)
  }

  /**
   * Load XML from a string.
   */
  def loadXml(xmlText: String): Either[String, Elem] = {
    try {
      val doc = XML.loadString(xmlText)
      Right(doc)
    } catch {
      case t: Throwable => Left("XML error: " + t.toString)
    }
  }

  /**
   * Escape all the special characters in XML so that they will be interpreted as their original form.
   *
   * @param xmlText Text formatted as XML.
   * @return Escaped XML.
   */
  def escapeXml(xmlText: String): String = {
    // @formatter:off
    xmlText.
      replace("&", "&#38;").
      replace(">", "&#62;").
      replace("<", "&#60;").
      replace("'", "&#39;").
      replace("\"", "&#34;")
    // @formatter:on
  }

  /** Get the Z position of a slice. */
  def slicePosition(attributeList: AttributeList): Double = attributeList.get(TagByName.ImagePositionPatient).getDoubleValues()(2)

  /**
   * Find the slice spacing by looking at the distance between consecutive slices.  Use a few of
   * the smaller ones just in case there is a spacial discontinuity.
   */
  private def getSliceSpacing(sorted: Seq[AttributeList]) = {
    if (sorted.size < 2) throw new IllegalArgumentException("Volume must contain at least 2 slices but actually has " + sorted.size)
    val sampleSize = 5
    val smallest = sorted.indices.tail.map(i => (slicePosition(sorted(i)) - slicePosition(sorted(i - 1))).abs).sorted.take(sampleSize)
    val size = smallest.sum / smallest.size
    size
  }

  /**
   * Sort series by Z position ascending.
   */
  def sortByZ(attrListList: Seq[AttributeList]): Seq[AttributeList] = {
    attrListList.sortBy(al => slicePosition(al))
  }

  /**
   * Get the size of a voxel in mm.  Requires that multiple slices be given.
   */
  def getVoxSize_mm(attrListList: Seq[AttributeList]): Point3d = {
    val sorted = sortByZ(attrListList)
    val xSize = sorted.head.get(TagByName.PixelSpacing).getDoubleValues()(0)
    val ySize = sorted.head.get(TagByName.PixelSpacing).getDoubleValues()(1)
    val zSize = getSliceSpacing(sorted)
    new Point3d(xSize, ySize, zSize)
  }

  case class RegInfo(attrList: AttributeList) {
    val frameOfRefUID: String = attrList.get(TagByName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
    val otherFrameOfRefUID: Seq[String] = {
      val regSeq = DicomUtil.seqToAttr(attrList, TagByName.RegistrationSequence)
      val frmUid = regSeq.map(rs => rs.get(TagByName.FrameOfReferenceUID).getSingleStringValueOrEmptyString).filterNot(fu => fu.equalsIgnoreCase(frameOfRefUID))
      frmUid.toIndexedSeq
    }
  }

  /**
   * Get the list of isocenters in the given RTPLAN.
   */
  def getPlanIsocenterList(rtplan: AttributeList): Seq[Point3d] = {

    def controlPointSeqToIsocenter(cps: AttributeList): Option[Point3d] = {
      try {
        val IsocenterPosition = cps.get(TagByName.IsocenterPosition)
        Some(new Point3d(IsocenterPosition.getDoubleValues))
      } catch {
        case _: Throwable => None
      }
    }

    def beamSeqToIsocenter(beamSeq: AttributeList) = {
      val ControlPointSequence = DicomUtil.seqToAttr(beamSeq, TagByName.ControlPointSequence)
      ControlPointSequence.map(cps => controlPointSeqToIsocenter(cps))
    }

    val BeamSequence = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val list = BeamSequence.flatMap(bs => beamSeqToIsocenter(bs)).flatten
    list.toIndexedSeq
  }

  /**
   * Get the SOP of the RTPLAN that this RTIMAGE references.
   *
   * @param rtimage RTIMAGE
   * @return SOP of RTPLAN, or None if not available.
   */
  def getRtplanSop(rtimage: AttributeList): Option[String] = {
    try {
      val rtplanRef = DicomUtil.seqToAttr(rtimage, TagByName.ReferencedRTPlanSequence)
      val sop = rtplanRef.map(al => al.get(TagByName.ReferencedSOPInstanceUID)).head.getSingleStringValueOrNull
      Some(sop)
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Given arbitrary text, replace all special characters with underscore so it can be used as a JavaScript or XML identifier.
   */
  //noinspection RegExpSimplifiable
  def textToId(text: String): String = text.replaceAll("[^0-9a-zA-Z]", "_").replaceAll("__*", "_")


  /**
   * Given arbitrary text, replace all special characters with underscore so it can be used as a HTML5 identifier.
   *
   * @param text Original text.
   * @return Text valid to be used as an HTML5 id tag.
   */
  def textToHtmlId(text: String): String = text.replaceAll("[^A-Za-z0-9_\\-:. ]", "_")

  def attributeListToDeviceSerialNumber(al: AttributeList): Option[String] = {
    val at = al.get(TagByName.DeviceSerialNumber)
    if (at == null) None
    else {
      val ser = at.getSingleStringValueOrNull
      if ((ser != null) && ser.trim.nonEmpty) Some(ser.trim) else None
    }
  }

  /**
   * Convert a hex string to an AWT color.
   */
  def hexToColor(hex: String): Color = {
    new Color(Integer.parseInt(hex, 16))
  }

  /**
   * Get an attribute of a node as text.
   */
  def getAttr(node: Node, name: String): String = (node \ ("@" + name)).text

  def badPixelRadius(attributeList: AttributeList): Int = {
    val pixSize = attributeList.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.sum / 2
    val diam = Config.BadPixelRadius_mm / pixSize
    val r = (diam.ceil.toInt - 1) / 2
    r
  }

  def badPixelList(al: AttributeList): Seq[DicomImage.PixelRating] = {
    val dicomImage = new DicomImage(al)
    val numPixels = dicomImage.width * dicomImage.height
    val million = 1000.0 * 1000
    val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / million) * numPixels).round.toInt
    val badPixels = dicomImage.identifyBadPixels(maxBadPixels, Config.BadPixelStdDev, Config.BadPixelMaximumPercentChange, Util.badPixelRadius(al), Config.BadPixelMinimumDeviation_CU)
    badPixels.filter(bp => bp.rating > 100).toIndexedSeq
  }

  /**
   * Attempt to free memory by using gc.
   */
  def garbageCollect(): Unit = {
    val start = System.currentTimeMillis()
    val runtime = Runtime.getRuntime

    val totalWait_ms = 410.toLong
    val wait_ms = 100.toLong

    def fmt(x: Long) = {
      (x / (1000.0 * 1000 * 1000)).formatted("%6.3f") + " GB"
    }

    val timeout = System.currentTimeMillis + totalWait_ms
    val before = runtime.freeMemory
    logger.info("Free memory before garbage collection hint : " + fmt(before))
    while (timeout > System.currentTimeMillis) {
      runtime.gc()
      Thread.sleep(wait_ms)
    }
    val after = runtime.freeMemory
    val elapsed = System.currentTimeMillis() - start
    logger.info("Freed memory in " + elapsed + " ms garbage collection.  Available before: " + fmt(before) + "    after: " + fmt(after) + "    amount freed: " + fmt(after - before))
  }

  def getFrameOfRef(al: AttributeList): String = al.get(TagByName.FrameOfReferenceUID).getSingleStringValueOrEmptyString

  def getFrameOfRef(dicomFile: DicomFile): String = getFrameOfRef(dicomFile.attributeList.get)

  /**
   * Transform a point by this matrix and create a new point.
   */
  def transform(matrix: Matrix4d, point: Point3d): Point3d = {
    val x = point.clone.asInstanceOf[Point3d]
    matrix.transform(x)
    x
  }

  /**
   * Transform a point by the inverse of the given matrix and create a new point.
   */
  def invTransform(matrix: Matrix4d, point: Point3d): Point3d = {
    val invMatrix = matrix.clone.asInstanceOf[Matrix4d]
    invMatrix.invert()
    val x = point.clone.asInstanceOf[Point3d]
    invMatrix.transform(x)
    x
  }

  /**
   * Multiply a matrix by another matrix.  The original matrixes are unchanged.  Note that the parameter
   * order is important; they are not interchangeable.
   *
   * @param a Matrix to multiply.
   * @param b Matrix to multiply.
   * @return the transposition of the original matrix.
   */
  def multiplyMatrix(a: Matrix4d, b: Matrix4d): Matrix4d = {
    val t = a.clone().asInstanceOf[Matrix4d]
    t.mul(b)
    t
  }

  /**
   * Calculate the transpose a matrix.  The original matrix is unchanged.
   *
   * @param matrix Matrix to transpose.
   * @return the transposition of the original matrix.
   */
  def transposeMatrix(matrix: Matrix4d): Matrix4d = {
    val trans = matrix.clone().asInstanceOf[Matrix4d]
    trans.transpose()
    trans
  }

  /**
   * Format the matrix as a Matlab consumable string, aligning decimal points vertically and
   * minimizing the overall width.
   *
   * @param matrix Matrix to format.
   * @return A text fragment that can be embedded in a Matlab program and executed as code.
   */
  def formatMatrix(matrix: Matrix4d): String = {
    def fmt(d: Double) = {
      //noinspection RegExpSimplifiable
      d.toString.replaceAll("00* ", "0 ").trim.replaceAll("\\.$", ".0")
    }

    /** Representation of one column of the matrix. */
    case class Specs(textList: Seq[String]) {

      /** Position of decimal point in text version of number. */
      private def dPos(t: String): Int = t.indexOf('.')

      /** Largest index of position of decimal point. */
      val decimalPosition: Int = textList.map(dPos).max

      /** Maximum number of characters needed to represent a number. */
      val maxLength: Int = textList.map(t => t.length - dPos(t)).max + decimalPosition

      /**
       * Add spaces before and after as needed.
       *
       * @param row Row index.
       * @return Number formatted so it will line up visually.
       */
      def show(row: Int): String = (Seq.fill(decimalPosition - dPos(textList(row)))(" ").mkString + textList(row) + "                                                 ").take(maxLength)
    }

    def specs(column: Int): Specs = {
      val vector = new Vector4d()
      matrix.getColumn(column, vector)
      val values = Array.fill(4)(0.0)
      vector.get(values)

      val textList = values.map(fmt)
      Specs(textList.toIndexedSeq)
    }

    val range = 0 to 3
    val specList = range.map(c => specs(c))

    val matrixText = "[\n" + range.map(row => range.map(column => specList(column).show(row)).mkString("  ")).mkString("  ;\n") + " ];\n"

    matrixText
  }

  /**
   * Draw two green concentric circles around the RTPLAN center.  This imitates the way it is shown in Eclipse.
   */
  def drawPlanCenter(image: BufferedImage, planCenter_pix: Point2d, scale: Double = 1.0): Unit = {
    def d2i(d: Double) = d.round.toInt

    val graphics = ImageUtil.getGraphics(image)
    graphics.setColor(Color.green)

    val smallRadius = 4 * scale
    val largeRadius = smallRadius * 2

    graphics.drawOval(d2i(planCenter_pix.getX - smallRadius), d2i(planCenter_pix.getY - smallRadius), d2i(smallRadius * 2), d2i(smallRadius * 2))
    graphics.drawOval(d2i(planCenter_pix.getX - largeRadius), d2i(planCenter_pix.getY - largeRadius), d2i(largeRadius * 2), d2i(largeRadius * 2))
  }

  /**
   * Scale a pixel coordinate by the given scale to produce a scaled coordinate in pixel space.
   */
  def scalePixel(scale: Int, pix: Double): Double = {
    scale * (pix + 0.5) - 0.5
  }

  def d2i(d: Double): Int = d.round.toInt

  /**
   * Draw a + at the center showing where the plan isocenter is.
   *
   * @param center Center of + in pixels, not scaled.
   */
  def drawTarget(center: Point2d, bufImage: BufferedImage, scale: Int, color: Color = Color.red): Unit = {

    val planCenterOffsetAndScaled_pix = new Point2D.Double(center.getX, center.getY)

    val graphics = ImageUtil.getGraphics(bufImage)
    graphics.setColor(color)

    val radius = scale * 2
    val skip = 2
    val diam = radius * 2
    val xi = d2i(planCenterOffsetAndScaled_pix.getX)
    val yi = d2i(planCenterOffsetAndScaled_pix.getY)

    graphics.drawLine(xi - radius, yi, xi - skip, yi)
    graphics.drawLine(xi + skip, yi, xi + radius, yi)

    graphics.drawLine(xi, yi - radius, xi, yi - skip)
    graphics.drawLine(xi, yi + skip, xi, yi + radius)

    graphics.setStroke(new BasicStroke(3))
    graphics.drawOval(xi - radius, yi - radius, diam, diam)

    val thickness = d2i(scale * 0.25)
    val len = thickness * 3

    def top(): Unit = {
      val poly = new Polygon
      poly.addPoint(xi, yi - radius)
      poly.addPoint(xi - thickness, yi - radius)
      poly.addPoint(xi, yi - radius + len)
      poly.addPoint(xi + 1, yi - radius + len)
      poly.addPoint(xi + thickness, yi - radius)
      graphics.fill(poly)
    }

    def bottom(): Unit = {
      val poly = new Polygon
      poly.addPoint(xi, yi + radius)
      poly.addPoint(xi - thickness, yi + radius)
      poly.addPoint(xi, yi + radius - len)
      poly.addPoint(xi + 1, yi + radius - len)
      poly.addPoint(xi + thickness, yi + radius)
      graphics.fill(poly)
    }

    def left(): Unit = {
      val poly = new Polygon
      poly.addPoint(xi - radius, yi)
      poly.addPoint(xi - radius, yi - thickness + 1)
      poly.addPoint(xi - radius + len, yi)
      poly.addPoint(xi - radius + len, yi + 1)
      poly.addPoint(xi - radius, yi + thickness)
      graphics.fill(poly)
    }

    def right(): Unit = {
      val poly = new Polygon
      poly.addPoint(xi + radius, yi)
      poly.addPoint(xi + radius, yi - thickness + 1)
      poly.addPoint(xi + radius - len, yi)
      poly.addPoint(xi + radius - len, yi + 1)
      poly.addPoint(xi + radius, yi + thickness)
      graphics.fill(poly)
    }

    top()
    bottom()
    left()
    right()
  }

  /**
   * Get values from an attribute list, scaling by the amount given if they are
   * numeric.  Discrete (Int, Short, Long) values will be rounded.  Search the
   * entire depth of the attribute list for values, using the first one.  If
   * there is a problems (such as none found), then return a Seq of "NA".
   *
   * @param al    Attribute list containing values.
   * @param tag   Attribute tag of value.
   * @param scale Amount to scale value.  Ignored if string.
   * @return String representation of scaled values.
   */
  def attr2Csv(al: AttributeList, tag: AttributeTag, scale: Double = 1.0): Seq[String] = {
    try {
      val at = DicomUtil.findAllSingle(al, tag).head

      def asLong() = at.getLongValues.map(l => (l * scale).round)

      val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
      val numList = vr match {
        case _ if ValueRepresentation.isIntegerStringVR(vr) => at.getIntegerValues.map(i => (i * scale).round.toInt)
        case _ if ValueRepresentation.isLongStringVR(vr) => asLong()

        case _ if ValueRepresentation.isSignedLongVR(vr) => asLong()
        case _ if ValueRepresentation.isSignedShortVR(vr) => asLong()

        case _ if ValueRepresentation.isUnsignedLongVR(vr) => asLong()
        case _ if ValueRepresentation.isUnsignedShortVR(vr) => at.getIntegerValues.map(i => (i * scale).round.toShort)

        case _ if ValueRepresentation.isFloatDoubleVR(vr) => at.getDoubleValues.map(n => n * scale)
        case _ if ValueRepresentation.isFloatSingleVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ if ValueRepresentation.isDecimalStringVR(vr) => at.getFloatValues.map(n => n * scale)

        case _ if ValueRepresentation.isOtherByteOrWordVR(vr) => throw new RuntimeException("binary values not supported")
        case _ => at.getStringValues // Handle all of the various string VRs.
      }
      numList.map(n => n.toString).toIndexedSeq
    } catch {
      case _: Throwable => Seq("NA", "NA", "NA")
    }
  }

  /** The mathematical standard Golden Ratio that is pleasing to view : https://en.wikipedia.org/wiki/Golden_ratio */
  val goldenRatio = 1.61803398875

  /**
   * Make an awt Rectangle from double precision values.  This is mostly a convenience function.
   *
   * @param x      X
   * @param y      Y
   * @param width  Width
   * @param height Height
   * @return a Rectangle
   */
  def rectD(x: Double, y: Double, width: Double, height: Double): Rectangle = {
    new Rectangle(d2i(x), d2i(y), d2i(width), d2i(height))
  }

  /** General purpose XML formatter */
  def prettyPrint(xml: Node): String = {
    new PrettyPrinter(1024, 2).format(xml)
  }

  /**
   * Given XML from a machine log, get the machine serial number.  If it does not exist, then return nothing.
   * This also somewhat serves as a validation for the XML.
   *
   * @param elem Machine log representation.
   * @return Machine serial number if it is present.
   */
  def machineLogSerialNumber(elem: Elem): Option[String] = {
    (elem \ "Environment" \ "MachineSerialNumber").headOption match {
      case Some(e) => Some(e.text)
      case _ => None
    }
  }

  /**
   * Determine if a given beam is always are open for the given size centered.
   * This is used to determine which beams are suitable for tests that require a minimally
   * sized rectangular field, such as symmetry+flatness or center dose.
   *
   * @param beam       Beam meta data.
   * @param minSize_mm Minimum field size.
   * @return True if criteria is met.
   */
  def minCenteredFieldBeam(beam: AttributeList, minSize_mm: Double): Boolean = {
    val distance = minSize_mm / 2
    val positionList = DicomUtil.findAllSingle(beam, TagByName.LeafJawPositions).flatMap(_.getDoubleValues)
    val notOpen = positionList.exists(pos => pos.abs < distance)
    val wedgeList = DicomUtil.findAllSingle(beam, TagByName.NumberOfWedges).flatMap(_.getIntegerValues).distinct.filter(_ != 0)
    (!notOpen) && wedgeList.isEmpty
  }

  /**
   * Get the list of beam names whose fields always are open for the given size centered.
   * This is used to determine which beams are suitable for tests that require a minimally
   * sized rectangular field, such as symmetry+flatness or center dose.
   *
   * @param rtplan     Plan under scrutiny.
   * @param minSize_mm Minimum vertical and horizontal field size in mm.
   * @return List of qualifying beam names.
   */
  def minCenteredFieldBeamList(rtplan: AttributeList, minSize_mm: Double): Seq[String] = {

    val BeamSequence = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val list = BeamSequence.filter(b => minCenteredFieldBeam(b, minSize_mm)).map(normalizedBeamName).distinct
    list.toIndexedSeq
  }

  /**
   * Determine which beams can be used for center dose analysis
   *
   * @param rtplan Based on this RTPLAN.
   * @return List of normalized beam names.
   */
  def makeCenterDoseBeamNameList(rtplan: AttributeList): Seq[String] = {
    // list of beam names
    val list = Util.minCenteredFieldBeamList(rtplan, Config.CenterDoseRadius_mm * 2)
    list
  }

  /**
   * Determine which beams can be used for symmetry+flatness analysis
   *
   * @param rtplan Based on this RTPLAN.
   * @return List of normalized beam names.
   */
  def makeSymFlatConstBeamNameList(rtplan: AttributeList): Seq[String] = {
    val margin = Config.SymmetryAndFlatnessDiameter_mm / 2

    // calculate the farthest possible points
    val max = Config.SymmetryAndFlatnessPointList.flatMap(p => Seq(p.x_mm, p.y_mm)).map(c => c.abs + margin).max

    // list of beam names
    val list = Util.minCenteredFieldBeamList(rtplan, max * 2)

    list
  }

  /**
   * Given an RTPLAN and an RTIMAGE, get the beam's attribute list in the RTPLAN.
   *
   * @param plan       RTPLAN
   * @param BeamNumber beam number
   * @return Beam parameters.
   */
  def getBeamOfRtimage(plan: AttributeList, BeamNumber: Int): Option[AttributeList] = {
    try {
      val beam = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).find(bs => bs.get(TagByName.BeamNumber).getIntegerValues.head == BeamNumber)
      beam
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Given an RTPLAN and an RTIMAGE, get the beam's attribute list in the RTPLAN.
   *
   * @param plan    RTPLAN
   * @param rtimage RTIMAGE
   * @return Beam parameters.
   */
  def getBeamOfRtimage(plan: AttributeList, rtimage: AttributeList): Option[AttributeList] = {
    try {
      val BeamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
      val beam = getBeamOfRtimage(plan, BeamNumber)
      beam
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Given an RTPLAN and an RTIMAGE, get the name of the beam that the RTIMAGE is referencing in the plan.
   *
   * @param plan    RTPLAN
   * @param rtimage RTIMAGE
   * @return Beam name.
   */
  def getBeamNameOfRtimage(plan: AttributeList, rtimage: AttributeList): Option[String] = {
    try {
      getBeamOfRtimage(plan, rtimage).map(Util.normalizedBeamName)
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Determine if the beam is an FFF beam.
   * Throw an exception if more than one beam (e.g. an entire rtplan) is specified.
   *
   * @param beam Attribute list for just one beam.
   * @return True if it is an FFF beam.
   */
  def isFFF(beam: AttributeList): Boolean = {
    val fluenceList = DicomUtil.findAllSingle(beam, TagByName.FluenceModeID)
    fluenceList.size match {
      case 0 => false
      case 1 => fluenceList.head.getSingleStringValueOrEmptyString.trim.toUpperCase().equals("FFF")
      case _ => throw new RuntimeException("More than one beam specified in call to isFFF.")
    }
  }

  def main(args: Array[String]): Unit = {
    Config.validate

    println("Starting --------------------------------------------------------------------------------------------------------------------------")
    val dir = new File("""D:\pf\IntelliJ\ws\aqa\src\main\resources\static\rtplan""")
    val fileList = listDirFiles(dir).filter(f => f.getName.toLowerCase().endsWith(".dcm"))

    def showRtplan(file: File): Unit = {
      val al = DicomFile(file).attributeList.get
      val names = makeCenterDoseBeamNameList(al) ++ Seq(" :: ") ++ makeSymFlatConstBeamNameList(al)
      println(s"""${file.getName} :: ${names.mkString("  ")}""")
    }

    fileList.foreach(showRtplan)
  }
}
