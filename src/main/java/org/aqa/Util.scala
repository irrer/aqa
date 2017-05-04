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
import java.text.ParseException
import com.pixelmed.dicom.DateTimeAttribute
import gnu.crypto.hash.IMessageDigest
import gnu.crypto.hash.HashFactory
import gnu.crypto.prng.BasePRNG
import gnu.crypto.prng.PRNGFactory
import scala.util.Random
import java.math.BigDecimal
import java.math.RoundingMode
import java.util.Properties

object Util {

    val aqaDomain = "automatedqualityassurance.org"
    val aqaUrl = "https://www." + aqaDomain + "/"
    val machineConfigDirEnvName = "machine_configDir"
    val machineIdEnvName = "machine_id"

    private val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

    def timeAsFileName(date: Date) = timeAsFileNameFormat.format(date)

    def currentTimeAsFileName = timeAsFileName(new Date)

    /** Standard date format */
    val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

    private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss")

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

    def writeBinaryFile(file: File, data: Array[Byte]): Unit = {
        val fos = new FileOutputStream(file)
        fos.write(data)
        fos.flush
        fos.close
    }

    def writeFile(file: File, text: String): Unit = writeBinaryFile(file, text.getBytes)

    def readBinaryFile(file: File): Either[Throwable, Array[Byte]] = {
        try {
            val fis = new FileInputStream(file)
            val buf = new Array[Byte](file.length.toInt)
            fis.read(buf)
            fis.close
            Right(buf)
        }
        catch {
            case t: Throwable => Left(t)
        }
    }

    def readTextFile(file: File): Either[Throwable, String] = {
        val result = readBinaryFile(file)
        if (result.isLeft) Left(result.left.get)
        else Right(new String(result.right.get))
    }

    def readDicomFile(file: File): Either[Throwable, AttributeList] = {
        try {
            val al = new AttributeList
            al.read(file)
            Right(al)
        }
        catch {
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

    case class DateTimeAndPatientId(val dateTime: Option[Long], val PatientID: Option[String]);

    private def getTimeAndDate(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
        try {
            Some(DateTimeAttribute.getDateFromFormattedString(al, dateTag, timeTag))
        }
        catch {
            case e: Throwable => None
        }
    }

    private def getTimeAndDate(al: AttributeList, dateTimeTag: AttributeTag): Option[Date] = {
        try {
            Some(DateTimeAttribute.getDateFromFormattedString(al.get(dateTimeTag).getSingleStringValueOrNull))
        }
        catch {
            case e: Throwable => None
        }
    }

    private def extractDateTimeAndPatientIdFromDicom(file: File): (Seq[Date], Option[String]) = { // TODO make this safer

        val attrList = new AttributeList
        attrList.read(file)

        val PatientID = getAttrValue(attrList, TagFromName.PatientID)

        val dateTimeTagPairList = List(
            (TagFromName.ContentDate, TagFromName.ContentTime),
            (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
            (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
            (TagFromName.CreationDate, TagFromName.CreationTime),
            (TagFromName.SeriesDate, TagFromName.SeriesTime))

        val AcquisitionDateTime = getTimeAndDate(attrList, TagFromName.AcquisitionDateTime)

        val dateTimePairs = dateTimeTagPairList.map(dt => getTimeAndDate(attrList, dt._1, dt._2))

        val dateList = (AcquisitionDateTime +: dateTimePairs).filter(dt => dt.isDefined).map(dtd => dtd.get).distinct

        (dateList, PatientID)
    }

    private def dateTimeAndPatientIdFromDicom(file: File, dateList: Seq[Date], patientIdList: Seq[String]): (Seq[Date], Seq[String]) = {
        if (file.isDirectory) {
            val listPair = file.listFiles.map(f => dateTimeAndPatientIdFromDicom(f, List[Date](), List[String]())).toList.unzip
            (dateList ++ listPair._1.flatten, patientIdList ++ listPair._2.flatten)
        }
        else if (file.canRead && DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
            val dtp = extractDateTimeAndPatientIdFromDicom(file)
            val p = if (dtp._2.isDefined) patientIdList :+ dtp._2.get else patientIdList
            (dtp._1, p)
        }
        else (dateList, patientIdList)
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
        }
        catch {
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
        val propFile = {
            val std = new File("yajsw-stable-11.03\\conf\\wrapper.conf")
            if (std.exists) std
            else new File("src\\main\\resources\\wrapper.conf")
        }
        try {
            prop.load(new FileInputStream(propFile))
        }
        catch {
            // Do not use logging to print an error message because when getting
            // properties it is quite possible that logging has not yet been set up.
            case e: Exception => println("Problem reading properties file " + propFile.getAbsolutePath + " : " + Logging.fmtEx(e))
        }
        prop
    }

    /**
     * Safely get the list of files in a directory.
     */
    def listDirFiles(dir: File): Seq[File] = {
        val list = try {
            dir.listFiles.toSeq
        }
        catch {
            case t: Throwable => null
        }

        if (list != null) list else Seq[File]()
    }

    def main(args: Array[String]): Unit = {

        if (true) {
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
}