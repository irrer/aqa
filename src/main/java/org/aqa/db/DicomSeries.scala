package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.security.InvalidParameterException
import java.sql.Timestamp
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.SOPClass

/**
 * Store the contents of a DICOM series.
 *
 */
case class DicomSeries(
  dicomSeriesPK: Option[Long], // primary key
  userPK: Long, // user that uploaded or created this series
  inputPK: Option[Long], // referenced input, if available
  machinePK: Option[Long], // referenced machine, if available
  sopInstanceUIDList: String, // DICOM SOPInstanceUID of all files in series.
  seriesInstanceUID: String, // DICOM SeriesInstanceUID
  frameOfReferenceUID: Option[String], // DICOM FrameOfReferenceUID.  For registration files, this is the FrameOfReferenceUID that they convert from (take as input).
  modality: String, // DICOM Modality
  sopClassUID: String, // DICOM SOPClassUID of first file.  A more rigorous definition of modality.
  deviceSerialNumber: Option[String], // DICOM DeviceSerialNumber found in top-level attribute list, if present.
  date: Timestamp, // when data was acquired at the treatment machine.  Value from first file found by checking for (in this order): ContentDate, InstanceCreationDate, AcquisitionDate, CreationDate, SeriesDate
  patientID: Option[String], // Patient ID, if available
  size: Int, // Number of files in series
  content: Array[Byte]) // files in zip form
  {

  /**
   * Insert, returning the row that was inserted.  Note that dicomSeriesPK in the return value is defined.
   */
  def insert: DicomSeries = {
    val insertQuery = DicomSeries.query returning DicomSeries.query.map(_.dicomSeriesPK) into ((dicomSeries, dicomSeriesPK) => dicomSeries.copy(dicomSeriesPK = Some(dicomSeriesPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  lazy val attributeListList = DicomUtil.zippedByteArrayToDicom(content)

  override def toString = {
    "\n    dicomSeriesPK: " + dicomSeriesPK +
      "\n    userPK: " + userPK +
      "\n    inputPK: " + inputPK +
      "\n    machinePK: " + machinePK +
      "\n    sopInstanceUIDList: " + sopInstanceUIDList +
      "\n    seriesInstanceUID: " + seriesInstanceUID +
      "\n    frameOfReferenceUID: " + frameOfReferenceUID +
      "\n    modality: " + modality +
      "\n    sopClassUID: " + sopClassUID +
      "\n    deviceSerialNumber: " + deviceSerialNumber +
      "\n    date: " + date +
      "\n    patientID: " + patientID +
      "\n    size: " + size +
      "\n    content size: " + content.size
  }
}

object DicomSeries extends Logging {
  class DicomSeriesTable(tag: Tag) extends Table[DicomSeries](tag, "dicomSeries") {

    def dicomSeriesPK = column[Long]("dicomSeriesPK", O.PrimaryKey, O.AutoInc)
    def userPK = column[Long]("userPK")
    def inputPK = column[Option[Long]]("inputPK")
    def machinePK = column[Option[Long]]("machinePK")
    def sopInstanceUIDList = column[String]("sopInstanceUIDList")
    def seriesInstanceUID = column[String]("seriesInstanceUID")
    def frameOfReferenceUID = column[Option[String]]("frameOfReferenceUID")
    def modality = column[String]("modality")
    def sopClassUID = column[String]("sopClassUID")
    def deviceSerialNumber = column[Option[String]]("deviceSerialNumber")
    def date = column[Timestamp]("date")
    def patientID = column[Option[String]]("patientID")
    def size = column[Int]("size")
    def content = column[Array[Byte]]("content")

    def * = (
      dicomSeriesPK.?,
      userPK,
      inputPK,
      machinePK,
      sopInstanceUIDList,
      seriesInstanceUID,
      frameOfReferenceUID,
      modality,
      sopClassUID,
      deviceSerialNumber,
      date,
      patientID,
      size,
      content) <> ((DicomSeries.apply _)tupled, DicomSeries.unapply _)

    /* Note that accidental data deletion is protected by attempts to remove a machine.  If the
       user does confirm that they want a machine deleted, then the associated DicomSeries will be deleted automatically. */
    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomSeriesTable]

  def get(dicomSeriesPK: Long): Option[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.dicomSeriesPK === dicomSeriesPK
    } yield (dicomSeries)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByFrameUIDAndSOPClass(frameUIDSet: Set[String], sopClassUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if (dicomSeries.frameOfReferenceUID.inSet(frameUIDSet) && (dicomSeries.sopClassUID === sopClassUID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def getBySopInstanceUID(sopInstUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.sopInstanceUIDList === sopInstUID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def getBySeriesInstanceUID(seriesInstanceUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.seriesInstanceUID === seriesInstanceUID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def getByPatientID(patientID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.patientID === patientID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def delete(dicomSeriesPK: Long): Int = {
    val q = query.filter(_.dicomSeriesPK === dicomSeriesPK)
    val action = q.delete
    Db.run(action)
  }

  def makeDicomSeries(usrPK: Long, inpPK: Option[Long], machPK: Option[Long], alList: Seq[AttributeList]): DicomSeries = {

    val datePatID = alList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al))

    case class DPAl(al: AttributeList) {
      private val dp = Util.extractDateTimeAndPatientIdFromDicomAl(al)
      val date = dp._1.head
      val patId = dp._2
    }

    val sorted = alList.map(al => new DPAl(al)).sortBy(_.date.getTime)

    def byTag(tag: AttributeTag): Option[String] = {
      val s = sorted.head.al.get(tag).getSingleStringValueOrEmptyString
      if (s == null) None else Some(s)
    }

    val derivedMachinePK = {
      (machPK.isDefined, inpPK.isDefined) match {
        case (true, _) => machPK
        case (_, true) => Input.get(inpPK.get).get.machinePK
        case _ => None
      }
    }

    if (inpPK.isDefined) {
      val input = Input.get(inpPK.get).get
      if (input.userPK.get != usrPK) throw new IllegalArgumentException("User PK " + usrPK + " passed as parameter is different from that referenced by inputPK " + inpPK.get + " --> " + input.userPK.get)
    }

    def getSopInstanceUIDlist = alList.map(al => Util.sopOfAl(al)).mkString(" ")
    def getSeriesInstanceUID = byTag(TagFromName.SeriesInstanceUID).get
    def getFrameOfReferenceUID = byTag(TagFromName.FrameOfReferenceUID)
    def getModality = byTag(TagFromName.Modality).get
    def getSopClassUID = byTag(TagFromName.SOPClassUID).get
    def deviceSerialNumber = byTag(TagFromName.DeviceSerialNumber)
    def getDate = new Timestamp(sorted.head.date.getTime)
    def getPatientID = sorted.map(_.patId).flatten.headOption
    def getSize = sorted.size
    def getContent: Array[Byte] = DicomUtil.dicomToZippedByteArray(alList)

    new DicomSeries(
      None,
      usrPK,
      inpPK,
      derivedMachinePK,
      getSopInstanceUIDlist,
      getSeriesInstanceUID,
      getFrameOfReferenceUID,
      getModality,
      getSopClassUID,
      deviceSerialNumber,
      getDate,
      getPatientID,
      getSize,
      getContent)
  }

  /**
   * Get a list of all known RTPLAN device serial numbers
   */
  def planDeviceSerialNumberList = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.sopClassUID === SOPClass.RTPlanStorage))
    } yield (dicomSeries.deviceSerialNumber)
    val list = Db.run(action.result)
    list.flatten.distinct
  }
}
