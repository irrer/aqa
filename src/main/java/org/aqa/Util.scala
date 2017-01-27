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

object Util {

    private val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

    def timeAsFileName(date: Date) = timeAsFileNameFormat.format(date)

    def currentTimeAsFileName = timeAsFileName(new Date)

    private val timeHumanFriendlyFormat = new SimpleDateFormat("MMM dd yyyy HH:mm:ss")

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

    def writeFile(file: File, text: String): Unit = {
        val fos = new FileOutputStream(file)
        fos.write(text.getBytes)
        fos.flush
        fos.close
    }

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

    private def extractDateTimeAndPatientIdFromDicom(file: File): (Seq[Date], Option[String]) = {

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
            println("patIds: " + pdt._2.distinct) // TODO remove
            println("patIds sorted: " + pdt._2.distinct.sortWith(bestPatId)) // TODO remove
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

    def main(args: Array[String]): Unit = {

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