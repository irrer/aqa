package org.aqa

import java.text.SimpleDateFormat
import java.util.Date
import java.io.FileOutputStream
import java.io.File
import java.io.FileInputStream
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DateTimeAttribute
import java.util.Properties
import edu.umro.util.Utility
import com.pixelmed.dicom.SequenceAttribute
import java.awt.image.RenderedImage
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color
import edu.umro.ImageUtil.ImageUtil
import java.awt.BasicStroke
import edu.umro.ImageUtil.ImageText
import com.pixelmed.dicom.TransferSyntax
import java.awt.Point
import java.security.InvalidParameterException
import com.pixelmed.dicom.DicomDictionary
import scala.xml.Elem
import scala.xml.XML
import java.io.ByteArrayOutputStream
import edu.umro.ScalaUtil.DicomUtil
import javax.vecmath.Point3d
import edu.umro.ScalaUtil.Trace
import java.util.TimeZone
import scala.xml.Node
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator

object Util extends Logging {

  val aqaDomain = "automatedqualityassurance.org"
  val aqaUrl = "https://www." + aqaDomain + "/"
  val machineConfigDirEnvName = "machine_configDir"
  val machineIdEnvName = "machine_id"
  val institutionIdEnvName = "institution_id"
  val dicomFileNameSuffix = ".dcm"

  private val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

  def timeAsFileName(date: Date) = timeAsFileNameFormat.format(date)

  def currentTimeAsFileName = timeAsFileName(new Date)

  /** Standard date format */
  val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date) = timeHumanFriendlyFormat.format(date)

  def currentTimeHumanFriendly = timeHumanFriendlyFormat.format(new Date)

  private val elapsedFormatHours = new SimpleDateFormat("HH:mm:ss")
  private val elapsedFormatMinutes = new SimpleDateFormat("m:ss.SSS")
  private val elapsedFormatSeconds = new SimpleDateFormat("s.SSS")

  def elapsedTimeHumanFriendly(time: Long): String = {
    val elapsedMs = time.abs
    val fmt = if (elapsedMs >= (60 * 60 * 1000))
      elapsedFormatHours
    else if (elapsedMs >= (60 * 1000))
      elapsedFormatMinutes
    else
      elapsedFormatSeconds
    fmt.format(new Date(elapsedMs))
  }

  /**
   * Global lock for synchronizing all file writes so that only one write is being done at
   *  a time (as opposed to being done in parallel).
   */
  private val fileSystemWriteSync = "sync"

  def writeBinaryFile(file: File, data: Array[Byte]): Unit = fileSystemWriteSync.synchronized({
    file.delete
    val fos = new FileOutputStream(file)
    fos.write(data)
    fos.flush
    fos.close
  })

  def writeFile(file: File, text: String): Unit = writeBinaryFile(file, text.getBytes)

  def readBinaryFile(file: File): Either[Throwable, Array[Byte]] = {
    try {
      val fis = new FileInputStream(file)
      val buf = new Array[Byte](file.length.toInt)
      fis.read(buf)
      fis.close
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
    if (a == null) None
    else {
      val v = a.getSingleStringValueOrNull
      if (v == null) None
      else Some(v)
    }
  }

  def isRtplan(al: AttributeList) = {
    val a = al.get(TagFromName.Modality)
    (a != null) && (a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("RTPLAN"))
  }

  def isRtimage(al: AttributeList) = {
    val a = al.get(TagFromName.Modality)
    (a != null) && (a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("RTIMAGE"))
  }

  def isCt(al: AttributeList) = {
    val a = al.get(TagFromName.Modality)
    (a != null) && (a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("CT"))
  }

  def isReg(al: AttributeList) = {
    val a = al.get(TagFromName.Modality)
    (a != null) && (a.getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("REG"))
  }

  /**
   * Get the SOPInstanceUID of an attribute list.
   */
  def sopOfAl(al: AttributeList): String = {
    val at = al.get(TagFromName.SOPInstanceUID)
    if (at == null) "" else al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the SeriesInstanceUID of an attribute list.
   */
  def serInstOfAl(al: AttributeList): String = {
    val at = al.get(TagFromName.SeriesInstanceUID)
    if (at == null) "" else al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the PatientID of an attribute list.
   */
  def patientIdOfAl(al: AttributeList): String = {
    val at = al.get(TagFromName.PatientID)
    if (at == null) "" else al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
  }

  /**
   * Get the Modality of an attribute list.
   */
  def modalityOfAl(al: AttributeList): String = {
    val at = al.get(TagFromName.Modality)
    if (at == null) "" else al.get(TagFromName.Modality).getSingleStringValueOrEmptyString
  }

  def collimatorAngle(al: AttributeList): Double = {
    val at = al.get(TagFromName.BeamLimitingDeviceAngle)
    if (at == null) 0 else at.getDoubleValues.head
  }

  def gantryAngle(al: AttributeList): Double = {
    val at = al.get(TagFromName.GantryAngle)
    if (at == null) 0 else at.getDoubleValues.head
  }

  def gantryAngle(df: DicomFile): Double = gantryAngle(df.attributeList.get)

  case class DateTimeAndPatientId(val dateTime: Option[Long], val PatientID: Option[String]) {
    override def toString = {
      val dt = if (dateTime.isDefined) new Date(dateTime.get) else "none"
      val p = if (PatientID.isDefined) PatientID.get else "none"
      "dateTime: " + dt + "    PatientID: " + p
    }
  }

  private val dicomDateTimeFormat = new SimpleDateFormat("yyyyMMddHHmmss.SSS")
  private val zeroDate = new Date(0)

  /**
   * Convert date and time pair into java.util.Date.  Note that time will only be accurate to the ms.  Note that
   * the date and time must be done together so as to get the time zone and daylight savings time right.
   */
  private def getTimeAndDate(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
    try {
      val dateText = al.get(dateTag).getSingleStringValueOrNull
      val timeText = {
        val t = al.get(timeTag).getSingleStringValueOrNull
        val t2 = if (t.contains('.')) t + "000"
        else t + ".000"
        t2.take(10)
      }
      Some(dicomDateTimeFormat.parse(dateText + timeText))
    } catch {
      case e: Throwable => None
    }
  }

  private def getTimeAndDate(al: AttributeList, dateTimeTag: AttributeTag): Option[Date] = {
    try {
      Some(DateTimeAttribute.getDateFromFormattedString(al.get(dateTimeTag).getSingleStringValueOrNull))
    } catch {
      case e: Throwable => None
    }
  }

  /**
   * Get dates and patient ID from attribute list.
   */
  def extractDateTimeAndPatientIdFromDicomAl(attributeList: AttributeList): (Seq[Date], Option[String]) = {

    val PatientID = getAttrValue(attributeList, TagFromName.PatientID)

    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.CreationDate, TagFromName.CreationTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime))

    val AcquisitionDateTime = getTimeAndDate(attributeList, TagFromName.AcquisitionDateTime)

    val dateTimePairs = dateTimeTagPairList.map(dt => getTimeAndDate(attributeList, dt._1, dt._2))

    val dateList = (AcquisitionDateTime +: dateTimePairs).flatten //.distinct.map(dt => adjustDicomDateByLocalTimeZone(dt))

    (dateList, PatientID)
  }

  /**
   * Sort a list attribute lists in ascending order by date-time.
   */
  def sortByDateTime(alList: Seq[AttributeList]): Seq[AttributeList] = {

    def compar(a: AttributeList, b: AttributeList): Boolean = {
      def dt(al: AttributeList) = extractDateTimeAndPatientIdFromDicomAl(al)._1.head.getTime
      dt(a) < dt(b)
    }

    alList.sortWith(compar _)
  }

  /**
   * Get dates and patient ID from DICOM file.
   */
  def extractDateTimeAndPatientIdFromDicom(file: File): (Seq[Date], Option[String]) = {
    val al = new AttributeList
    al.read(file)
    extractDateTimeAndPatientIdFromDicomAl(al)
  }

  private def dateTimeAndPatientIdFromDicom(file: File, dateList: Seq[Date], patientIdList: Seq[String]): (Seq[Date], Seq[String]) = {
    if (file.isDirectory) {
      val listPair = file.listFiles.map(f => dateTimeAndPatientIdFromDicom(f, List[Date](), List[String]())).toList.unzip
      (dateList ++ listPair._1.flatten, patientIdList ++ listPair._2.flatten)
    } else if (file.canRead && DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
      val dtp = extractDateTimeAndPatientIdFromDicom(file)
      val p = if (dtp._2.isDefined) patientIdList :+ dtp._2.get else patientIdList
      (dtp._1, p)
    } else (dateList, patientIdList)
  }

  def dateTimeAndPatientIdFromDicom(file: File): DateTimeAndPatientId = {
    val pdt = dateTimeAndPatientIdFromDicom(file, List[Date](), List[String]())

    def bestPatId(p1: String, p2: String): Boolean = {
      if (p1.size == p2.size) p1.toLowerCase.compareTo(p2.toLowerCase()) < 0
      else (p1.size > p2.size)
    }

    val dateTime = if (pdt._1.isEmpty) None else Some(pdt._1.min.getTime)

    val patientId = {
      pdt._2.distinct.sortWith(bestPatId)
      if (pdt._2.isEmpty) None
      else Some(pdt._2.distinct.sortWith(bestPatId).head)
    }
    new DateTimeAndPatientId(dateTime, patientId)
  }

  /**
   * Get the date and time for building an Output.
   */
  def getOutputDateTime(alList: Seq[AttributeList]): Option[Long] = {
    val dateTimeTags = Seq(
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    def earliest(dateTag: AttributeTag, timeTag: AttributeTag): Option[Long] = {
      val seq = alList.map(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).flatten.map(d => d.getTime)
      if (seq.isEmpty) None else Some(seq.min)
    }

    val list = dateTimeTags.map(dt => earliest(dt._1, dt._2)).flatten
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get the patient ID for building an Output.
   */
  def getOutputPatientId(al: AttributeList): Option[String] = {
    val at = al.get(TagFromName.PatientID)
    if ((at != null) && (at.getSingleStringValueOrNull != null))
      Some(at.getSingleStringValueOrNull)
    else
      None
  }

  /**
   * Given a DICOM date/time, adjust it by the local time zone amount.
   */
  def adjustDicomDateByLocalTimeZone(date: Date) = {
    new Date(date.getTime - TimeZone.getDefault.getRawOffset)
  }

  /**
   * Given a DICOM date/time, adjust it by the local time zone amount.
   */
  def adjustDicomDateByLocalTimeZone(ms: Long) = {
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
      case t: Throwable => None
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
        val aqaInstallDir = (new File(System.getenv("AQAJAR"))).getParentFile
        val yajswDir = aqaInstallDir.listFiles.toSeq.filter(f => f.getName.toLowerCase.startsWith("yajsw")).head
        val confDir = new File(yajswDir, "conf")
        val wrapperFile = new File(confDir, "wrapper.conf")
        val wrapperFileName = wrapperFile.getAbsolutePath
        logger.info("Found wrapper file via AQAJAR environment variable at: " + wrapperFileName)
        Seq(wrapperFileName)
      } catch {
        case t: Throwable => Seq[String]()
      }
    }

    val prop = new Properties
    val fileNameList = Seq(
      """wrapper.conf""",
      """yajsw-stable-12.12\conf\wrapper.conf""",
      """src\main\resources\wrapper.conf""",
      """..\..\main\resources\wrapper.conf""") ++ wrapperByJar

    val fileList = fileNameList.map(name => new File(name))

    val propFile = {
      try {
        val file = fileList.filter(f => f.exists).headOption
        if (file.isEmpty) println("Problem finding properties file that exists from list:" + fileList.mkString("\n", "\n", "\n"))
        file
      } catch {
        // Do not use logging to print an error message because when getting
        // properties it is quite possible that logging has not yet been set up.
        case e: Exception => {
          println("Problem loading properties file List:" + fileList.mkString("\n", "\n", "\n") + " : " + fmtEx(e))
          None
        }
      }

    }

    try {
      if (propFile.isDefined) {
        prop.load(new FileInputStream(propFile.get))
        println("Loaded property file " + propFile.get)
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
    val list = try {
      dir.listFiles.toSeq
    } catch {
      case t: Throwable => null
    }

    if (list != null) list else Seq[File]()
  }

  def deleteFileTreeSafely(dir: File) = {
    try {
      Utility.deleteFileTree(dir)
    } catch {
      case t: Throwable => logger.warn("Unable to delete directory " + dir.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
   * Get the file name without the extension, as in:   foo.bar ==> foo
   */
  def fileBaseName(file: File): String = {
    file.getName match {
      case name if (name.contains('.')) => name.substring(0, name.lastIndexOf('.'))
      case name => name
    }
  }

  /**
   * Given the text for a single CVS cell, return the properly formatted text for CSV.
   */
  def textToCsv(text: String): String = {
    if (text.contains('"') || text.contains(',')) { '"' + text.replaceAll("\"", "\"\"") + '"' }
    else text
  }

  /**
   * Make a new file reference, putting the given file in the given directory.
   */
  def reDir(file: File, dir: File) = {
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
  def removeFileNameSuffix(fileName: String): String = fileName.replaceAll("\\.[^\\.]*$", "")

  //  def changeFileNameSuffix(file: File, suffix: String): String = {
  //    removeFileNameSuffix(file.getName) + suffix
  //  }

  /**
   * Specified by 300A,00B8  RTBeamLimitingDeviceType.
   */
  val xOrientation = Seq("X", "ASYMX", "MLCX", "MLCX1", "MLCX2")

  /**
   * Specified by 300A,00B8  RTBeamLimitingDeviceType.
   */
  val yOrientation = Seq("Y", "ASYMY", "MLCY")

  def specifiesX(devType: String): Boolean = xOrientation.contains(devType.toUpperCase)
  def specifiesY(devType: String): Boolean = yOrientation.contains(devType.toUpperCase)

  /**
   * Write a PNG file in a thread safe way.
   */
  def writePng(im: RenderedImage, pngFile: File): Unit = fileSystemWriteSync.synchronized({
    pngFile.delete
    ImageIO.write(im, "png", new FileOutputStream(pngFile))
  })

  /**
   * Write a JPG / JPEG file in a thread safe way.
   */
  def writeJpg(im: RenderedImage, jpegFile: File): Unit = fileSystemWriteSync.synchronized({
    jpegFile.delete
    ImageIO.write(im, "jpg", new FileOutputStream(jpegFile))
  })

  /**
   * Round the angle to the closest 90 degree angle.
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

  def main(args: Array[String]): Unit = {

    if (true) {
      val fileList = (new File("""D:\tmp\aqa\CBCT\MQATX1OBIQA2019Q3\ri_20190620""")).listFiles

      def doit(file: File) = {
        val al = new AttributeList
        al.read(file)

        val dtp = extractDateTimeAndPatientIdFromDicom(file)

        println(dtp)

      }

      fileList.map(al => doit(al))

      System.exit(99)
    }

    if (false) {
      for (i <- (-800 until 800)) println(i.formatted("%5d") + " --> " + angleRoundedTo90(i).formatted("%5d"))
      System.exit(0)
    }

    if (false) {
      import scala.util.Try
      var inc = 0

      def bad = {
        inc = inc + 1
        println("*bad " + inc)
        throw new RuntimeException("being bad")
        inc
      }

      def good = {
        inc = inc + 1
        println("*good " + inc)
        inc
      }

      println("bad good: " + { Try(bad).flatMap(g => Try(good)) })
      println("bad bad: " + { Try(bad).flatMap(g => Try(bad)) })
      println("good good: " + { Try(good).flatMap(g => Try(good)) })
      println("good bad: " + { Try(good).flatMap(g => Try(bad)) })

      val goodBad = { Try(good).flatMap(g => Try(bad)) }
      println("goodBad.getClass: " + goodBad.getClass)
      System.exit(0)
    }

    if (false) {
      val file = new File("""D:\tmp\aqa\ritter_bad_loc_upload\copy_me\jj.txt""")
      val fis = new FileInputStream(file)
      val data = fis.read()
      println("Sleeping ....")
      Thread.sleep(60 * 1000)
      println("exiting")
      System.exit(0)
    }

    if (false) {
      val j = this

      def doit(j: Any) = {
        val j1 = j.getClass
        val j2 = j1.getName
        val j3 = org.aqa.web.WebUtil.cleanClassName(j2)
        val j4 = org.aqa.web.WebUtil.pathOf(org.aqa.web.WebUtil.SubUrl.admin, this)
        println("ey")
      }
      doit(j)
    }

    val list: Seq[Double] = Seq(234.29847234, 0.00000023424, 2398472742.12341234)
    list.map(d => println("    " + d.toString + " --> " + d.formatted("%7.5f") + " ==> " + d.formatted("%7.5e")))

    println("thisJarFile: " + thisJarFile.getAbsolutePath)

    val fileNameList = List(
      """D:\pf\Conquest\dicomserver1417\data\ISOCAL""",
      """D:\pf\Conquest\dicomserver1417\data\Phantom""",
      """D:\pf\Conquest\dicomserver1417\data\txdlite_mr4""",
      """D:\pf\Conquest\dicomserver1417\data\28291735""",
      """D:\pf\Conquest\dicomserver1417\data\junk""")

    fileNameList.map(fn => {
      val f = new File(fn)
      val dtp = dateTimeAndPatientIdFromDicom(f)
      println("file: " + f.getName.formatted("%-20s") + "    " + dtp.dateTime + "    " + dtp.PatientID)
    })
  }

  /**
   * Add graticules to the given image.
   */
  def addGraticules(
    image: BufferedImage,
    x2Pix: (Double) => Double, y2Pix: (Double) => Double,
    pix2X: (Double) => Double, pix2Y: (Double) => Double,
    color: Color): Unit = {

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

    graphics.setColor(Color.gray)
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
    val xMinorInc = (x2Pix(xGrat(1)) - x2Pix(xGrat(0))) / (numMinorTics + 1)
    val yMinorInc = (y2Pix(xGrat(1)) - y2Pix(yGrat(0))) / (numMinorTics + 1)

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double) = graphics.drawLine(x1.round.toInt, y1.round.toInt, x2.round.toInt, y2.round.toInt)

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
  def addGraticules(
    image: BufferedImage,
    translator: IsoImagePlaneTranslator,
    color: Color): Unit = {

    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt
    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt
    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt
    def pix2Y(yPix: Double) = translator.pix2Iso(0, yPix).getY.round.toInt

    Util.addGraticules(image, x2Pix _, y2Pix _, pix2X _, pix2Y _, Color.gray)
  }

  /**
   * Number f pixels from edge to put axis arrows.
   */
  val axisOffsetFromEdge = 40

  def addAxisLabels(image: BufferedImage, horzLabel: String, vertLabel: String, color: Color, top: Boolean = true, bottom: Boolean = true, left: Boolean = true, right: Boolean = true) = {

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

    def trans = {
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

    def axial = {
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

      val textDim = ImageText.getTextDimensions(graphics, vertLabel)
      val xText = axisOffsetFromEdge + offset

      val yText = image.getHeight / 3
      graphics.drawString(vertLabel, xText, yText)
    }

    trans
    axial
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
        val a = attributeList.get(TagFromName.TransferSyntaxUID)
        if (a != null) {
          val ts = a.getStringValues
          if (ts.nonEmpty) ts.head else deflt
        } else deflt
      }

      val baos = new ByteArrayOutputStream
      attributeList.write(baos, transferSyntax, true, true)
      baos.flush
      Right(baos.toByteArray)
    } catch {
      case t: Throwable => {
        val msg = "Error converting DICOM to bytes: " + fmtEx(t)
        logger.warn(msg)
        Left(msg)
      }
    }

  }

  /**
   * Given two colors and a palette size, return a list of colors of the
   * given size that steps between the given colors.
   */
  def colorPallette(colorA: Color, colorB: Color, size: Int): IndexedSeq[Color] = {
    val step = { if (size == 1) 1.0 else (size - 1).toDouble }

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
    val norm = if (name.matches("^[0-9][0-9]:.*"))
      name.substring(3)
    else
      name
    norm.trim
  }

  /**
   * Get the normalized beam name from the given attribute list.  Beam names are sometimes prefixed with "NN:", where NN is
   * a pair of digits that cause the beams to be sorted by the Eclipse planning system in the preferred delivery order.
   */
  def normalizedBeamName(al: AttributeList): String = {
    normalizeBeamName(al.get(TagFromName.BeamName).getSingleStringValueOrEmptyString)
  }

  /**
   * Determine if two beam names are equal.  Both do normalization and consider that one or both were truncated.  Some
   * treatment planning systems truncates them from 16 characters to 13.
   */
  def beamNamesEqual(a: String, b: String): Boolean = {
    val aa = normalizeBeamName(a)
    val bb = normalizeBeamName(b)
    val len = Math.min(aa.size, bb.size)
    def subStr(text: String) = text.substring(0, Math.min(text.size, len))

    subStr(aa).equalsIgnoreCase(subStr(bb))
  }

  /**
   * Show which jar file is being used to ensure that we have the right version of the software.
   */
  def showJarFile(any: Any) = {

    def description(file: File): String = {
      val fullPath = file.getAbsolutePath
      val exists = file.exists
      val isNormal = file.isFile
      val len = if (file.exists) file.length.toString else "not available"
      val modified = if (exists) (new Date(file.lastModified)).toString else "not available";
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

    val msg = try {
      description(edu.umro.ScalaUtil.Util.getJarFile(any).get)
    } catch {
      case t: Throwable => "Could not determine jar file being used."
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

  /** Get the Z position of a slice. */
  def slicePosition(attributeList: AttributeList): Double = attributeList.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

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
  def getVoxSize_mm(attrListList: Seq[AttributeList]) = {
    val sorted = sortByZ(attrListList)
    val xSize = sorted.head.get(TagFromName.PixelSpacing).getDoubleValues()(0)
    val ySize = sorted.head.get(TagFromName.PixelSpacing).getDoubleValues()(1)
    val zSize = getSliceSpacing(sorted)
    Seq(xSize, ySize, zSize)
  }

  case class RegInfo(attrList: AttributeList) {
    val frameOfRefUID = attrList.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
    val otherFrameOfRefUID = {
      val regSeq = DicomUtil.seqToAttr(attrList, TagFromName.RegistrationSequence)
      val frmUid = regSeq.map(rs => rs.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString).filterNot(fu => fu.equalsIgnoreCase(frameOfRefUID))
      frmUid
    }
  }

  /**
   * Get the list of isocenters in the given RTPLAN.
   */
  def getPlanIsocenterList(rtplan: AttributeList): Seq[Point3d] = {

    def controlPointSeqToIsocenter(cps: AttributeList): Option[Point3d] = {
      try {
        val IsocenterPosition = cps.get(TagFromName.IsocenterPosition)
        Some(new Point3d(IsocenterPosition.getDoubleValues))
      } catch {
        case t: Throwable => None
      }
    }

    def beamSeqToIsocenter(beamSeq: AttributeList) = {
      val ControlPointSequence = DicomUtil.seqToAttr(beamSeq, TagFromName.ControlPointSequence)
      ControlPointSequence.map(cps => controlPointSeqToIsocenter(cps))
    }

    val BeamSequence = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val list = BeamSequence.map(bs => beamSeqToIsocenter(bs)).flatten.flatten
    list
  }

  /**
   * Given arbitrary text, replace all special characters with underscore so it can be used as a JavaScript or XML identifier.
   */
  def textToId(text: String) = text.replaceAll("[^0-9a-zA-Z]", "_").replaceAll("__*", "_")

  def attributeListToDeviceSerialNumber(al: AttributeList): Option[String] = {
    val at = al.get(TagFromName.DeviceSerialNumber)
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
  def getAttr(node: Node, name: String) = (node \ ("@" + name)).text.toString

  def badPixelRadius(attributeList: AttributeList): Int = {
    val pixSize = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues.sum / 2
    val diam = Config.BadPixelRadius_mm / pixSize
    val r = (diam.ceil.toInt - 1) / 2
    r
  }

  def badPixelList(al: AttributeList): Seq[DicomImage.PixelRating] = {
    val dicomImage = new DicomImage(al)
    val numPixels = dicomImage.width * dicomImage.height
    val million = 1000.0 * 1000
    val sampleSize = ((Config.BadPixelSamplePerMillion / million) * numPixels).round.toInt
    val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / million) * numPixels).round.toInt
    val badPixels = dicomImage.identifyBadPixels(
      maxBadPixels,
      Config.BadPixelStdDev, Config.BadPixelMaximumPercentChange, Util.badPixelRadius(al), Config.BadPixelMinimumDeviation_CU)
    badPixels.filter(bp => bp.rating > 100) // TODO filter?
  }

  /**
   * Attempt to free memory by using gc.
   */
  def garbageCollect = {
    val runtime = Runtime.getRuntime

    val totalWait_ms = 410.toLong
    val wait_ms = 100.toLong

    val timeout = System.currentTimeMillis + totalWait_ms
    val before = runtime.freeMemory
    logger.info("Free memory before garbage collection hint : " + before)
    while (timeout > System.currentTimeMillis) {
      runtime.gc
      Thread.sleep(wait_ms)
    }
    val after = runtime.freeMemory
    logger.info("Free memory after garbage collection hint  : " + after + "    amount freed: " + (after - before))
  }

  /**
   * Convert a gantry coordinate to a patient coordinate.  This does not compensate for table yaw, pitch, or roll.
   */
  def gantryCoordinateToPatientCoordinate(point: Point3d, gantryAngle: Double): Point3d = {

    ???
  }

  def getFrameOfRef(al: AttributeList): String = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  def getFrameOfRef(dicomFile: DicomFile): String = getFrameOfRef(dicomFile.attributeList.get)

}