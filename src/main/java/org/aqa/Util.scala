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
import gnu.crypto.hash.HashFactory
import scala.util.Random
import java.util.Properties
import edu.umro.util.Utility
import com.pixelmed.dicom.SequenceAttribute
import java.awt.image.RenderedImage
import javax.imageio.ImageIO
import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream
import java.awt.image.BufferedImage
import java.awt.Color
import edu.umro.ImageUtil.ImageUtil
import java.awt.BasicStroke
import edu.umro.ImageUtil.ImageText
import com.pixelmed.dicom.TransferSyntax

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

  def writeBinaryFile(file: File, data: Array[Byte]): Unit = (writeBinaryFile _).synchronized({
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

  def isModality(al: AttributeList, sopClassUID: String): Boolean = {
    try {
      al.get(TagFromName.MediaStorageSOPClassUID).getSingleStringValueOrEmptyString.equals(sopClassUID)
    } catch {
      case t: Throwable => false
    }
  }

  /**
   * Get the SOPInstanceUID of an attribute list.
   */
  def sopOfAl(al: AttributeList): String = {
    val at = al.get(TagFromName.SOPInstanceUID)
    if (at == null) "" else al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
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

  case class DateTimeAndPatientId(val dateTime: Option[Long], val PatientID: Option[String]);

  private def getTimeAndDate(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
    try {
      Some(DateTimeAttribute.getDateFromFormattedString(al, dateTag, timeTag))
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
  def extractDateTimeAndPatientIdFromDicom(attributeList: AttributeList): (Seq[Date], Option[String]) = {

    val PatientID = getAttrValue(attributeList, TagFromName.PatientID)

    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.CreationDate, TagFromName.CreationTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime))

    val AcquisitionDateTime = getTimeAndDate(attributeList, TagFromName.AcquisitionDateTime)

    val dateTimePairs = dateTimeTagPairList.map(dt => getTimeAndDate(attributeList, dt._1, dt._2))

    val dateList = (AcquisitionDateTime +: dateTimePairs).filter(dt => dt.isDefined).map(dtd => dtd.get).distinct

    (dateList, PatientID)
  }

  /**
   * Get dates and patient ID from DICOM file.
   */
  def extractDateTimeAndPatientIdFromDicom(file: File): (Seq[Date], Option[String]) = {
    val al = new AttributeList
    al.read(file)
    extractDateTimeAndPatientIdFromDicom(al)
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

  /** Convert byte array to printable text */
  def byteToString(bytes: Array[Byte]): String = bytes.foldLeft("")((t, b) => t + b.formatted("%02x"))

  private val DIGEST_NAME = "sha-512"

  def secureHash(data: Array[Byte]): Array[Byte] = {
    val md = HashFactory.getInstance(DIGEST_NAME)
    md.update(data, 0, data.size)
    md.digest
  }

  /**
   * Calculate a secure hash of the given text.
   */
  def secureHash(text: String): String = byteToString(secureHash(text.getBytes))

  /**
   * Generate a random cryptographically secure hash value.
   */
  def randomSecureHash: String = {
    val rand = new Random
    val words = (0 until 100).map(i => rand.nextLong.toString)
    val text = words.foldLeft(System.currentTimeMillis.toString)((t, l) => t + l)
    secureHash(text)
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
    val prop = new Properties
    val fileNameList = Seq(
      """wrapper.conf""",
      """yajsw-stable-12.12\conf\wrapper.conf""",
      """src\main\resources\wrapper.conf""",
      """..\..\main\resources\wrapper.conf""")

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
   * Get the attribute lists of a sequence attribute.
   */
  def seq2Attr(al: AttributeList, tag: AttributeTag): Seq[AttributeList] = {
    val seq = (al.get(tag)).asInstanceOf[SequenceAttribute]
    (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList)
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
  val xOrientation = Seq("X", "ASYMX", "MLCX")

  /**
   * Specified by 300A,00B8  RTBeamLimitingDeviceType.
   */
  val yOrientation = Seq("Y", "ASYMY", "MLCY")

  def specifiesX(devType: String): Boolean = xOrientation.contains(devType.toUpperCase)
  def specifiesY(devType: String): Boolean = yOrientation.contains(devType.toUpperCase)

  /**
   * Write a PNG file in a thread safe way.
   */
  def writePng(im: RenderedImage, pngFile: File): Unit = {
    val stream = new ByteOutputStream
    ImageIO.write(im, "png", stream)
    writeBinaryFile(pngFile, stream.getBytes)
  }

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

  def addGraticules(
    image: BufferedImage,
    x2Pix: (Double) => Double, y2Pix: (Double) => Double,
    pix2X: (Double) => Double, pix2Y: (Double) => Double,
    color: Color) = {

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

      val bos = new ByteOutputStream
      attributeList.write(bos, transferSyntax, true, true)
      bos.flush
      Right(bos.getBytes)
    } catch {
      case t: Throwable => {
        val msg = "Error converting DICOM to bytes: " + fmtEx(t)
        logger.warn(msg)
        Left(msg)
      }
    }

  }

  /**
   * Given two colors and a pallette size, return a list of colors of the
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

}