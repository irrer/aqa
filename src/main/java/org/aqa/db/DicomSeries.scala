package org.aqa.db

import Db.driver.api._

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
import edu.umro.ScalaUtil.Trace

/**
 * Store the contents of a DICOM series.
 *
 */
case class DicomSeries(
  dicomSeriesPK: Option[Long], // primary key
  userPK: Long, // user that uploaded or created this series
  inputPK: Option[Long], // referenced input, if available
  machinePK: Option[Long], // referenced machine, if available
  sopInstanceUIDList: String, // DICOM SOPInstanceUID of all files in series, separated by a single blank, prefixed and appended with a single blank.
  seriesInstanceUID: String, // DICOM SeriesInstanceUID
  frameOfReferenceUID: Option[String], // DICOM FrameOfReferenceUID.  For registration files, this is the FrameOfReferenceUID that they convert from (take as input).
  mappedFrameOfReferenceUID: Option[String], // For REG modality only.  DICOM FrameOfReferenceUID that can be mapped to.
  modality: String, // DICOM Modality
  sopClassUID: String, // DICOM SOPClassUID of first file.  A more rigorous definition of modality.
  deviceSerialNumber: Option[String], // DICOM DeviceSerialNumber found in top-level attribute list, if present.
  date: Timestamp, // when data was acquired at the treatment machine.  Value from first file found by checking for (in this order): ContentDate, InstanceCreationDate, AcquisitionDate, CreationDate, SeriesDate
  patientID: Option[String], // Patient ID, if available
  size: Int, // Number of files in series
  referencedRtplanUID: Option[String], // referenced RTPLAN UID if one is referenced
  content: Array[Byte]) // files in zip form
  {

  /**
   * Insert, returning the row that was inserted.  Note that dicomSeriesPK in the return value is defined.
   *
   * If this is an RTPLAN, then it should not be 'owned' by any particular input, so set the inputPK to None.
   */
  def insert: DicomSeries = {
    val ds = {
      if (modality.trim.equalsIgnoreCase("RTPLAN"))
        this.copy(inputPK = None)
      else
        this
    }
    val insertQuery = DicomSeries.query returning DicomSeries.query.map(_.dicomSeriesPK) into ((dicomSeries, dicomSeriesPK) => dicomSeries.copy(dicomSeriesPK = Some(dicomSeriesPK)))

    val action = insertQuery += ds
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
      "\n    mappedFrameOfReferenceUID: " + mappedFrameOfReferenceUID +
      "\n    modality: " + modality +
      "\n    sopClassUID: " + sopClassUID +
      "\n    deviceSerialNumber: " + deviceSerialNumber +
      "\n    date: " + date +
      "\n    patientID: " + patientID +
      "\n    size: " + size +
      "\n    referencedRtplanUID: " + referencedRtplanUID +
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
    def mappedFrameOfReferenceUID = column[Option[String]]("mappedFrameOfReferenceUID")
    def modality = column[String]("modality")
    def sopClassUID = column[String]("sopClassUID")
    def deviceSerialNumber = column[Option[String]]("deviceSerialNumber")
    def date = column[Timestamp]("date")
    def patientID = column[Option[String]]("patientID")
    def size = column[Int]("size")
    def referencedRtplanUID = column[Option[String]]("referencedRtplanUID")
    def content = column[Array[Byte]]("content")

    def * = (
      dicomSeriesPK.?,
      userPK,
      inputPK,
      machinePK,
      sopInstanceUIDList,
      seriesInstanceUID,
      frameOfReferenceUID,
      mappedFrameOfReferenceUID,
      modality,
      sopClassUID,
      deviceSerialNumber,
      date,
      patientID,
      size,
      referencedRtplanUID,
      content) <> ((DicomSeries.apply _)tupled, DicomSeries.unapply _)

    /* Note that accidental data deletion is protected by attempts to remove a machine.  If the
       user does confirm that they want a machine deleted, then the associated DicomSeries will be deleted automatically. */
    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
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

  case class DicomSeriesWithoutContent(
    dicomSeriesPK: Long,
    userPK: Long,
    inputPK: Option[Long],
    machinePK: Option[Long],
    sopInstanceUIDList: String,
    seriesInstanceUID: String,
    frameOfReferenceUID: Option[String],
    mappedFrameOfReferenceUID: Option[String],
    modality: String,
    sopClassUID: String,
    deviceSerialNumber: Option[String],
    date: Timestamp,
    patientID: Option[String],
    size: Int,
    referencedRtplanUID: Option[String]) {}

  def getByPatientID(patientID: String): Seq[DicomSeriesWithoutContent] = {
    val action = for {
      ds <- query if ((ds.patientID === patientID))
    } yield (
      ds.dicomSeriesPK,
      ds.userPK,
      ds.inputPK,
      ds.machinePK,
      ds.sopInstanceUIDList,
      ds.seriesInstanceUID,
      ds.frameOfReferenceUID,
      ds.mappedFrameOfReferenceUID,
      ds.modality,
      ds.sopClassUID,
      ds.deviceSerialNumber,
      ds.date,
      ds.patientID,
      ds.size,
      ds.referencedRtplanUID)
    val list = Db.run(action.result)
    val dsLite = list.map(ds => new DicomSeriesWithoutContent(
      ds._1,
      ds._2,
      ds._3,
      ds._4,
      ds._5,
      ds._6,
      ds._7,
      ds._8,
      ds._9,
      ds._10,
      ds._11,
      ds._12,
      ds._13,
      ds._14,
      ds._15))
    dsLite
  }

  def delete(dicomSeriesPK: Long): Int = {
    val q = query.filter(_.dicomSeriesPK === dicomSeriesPK)
    val action = q.delete
    Db.run(action)
  }

  /**
   * Construct a DicomSeries from an attribute list and some other required fields.
   */
  def makeDicomSeries(usrPK: Long, inpPK: Option[Long], machPK: Option[Long], alList: Seq[AttributeList]): Option[DicomSeries] = {
    // Doing a lot of things with undependable data so wrap this in a try-catch to cover all the bases
    try {
      val datePatID = alList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al))

      case class DPAl(al: AttributeList) {
        private val dp = Util.extractDateTimeAndPatientIdFromDicomAl(al)
        val date = dp._1.head
        val patId = dp._2
      }

      val sorted = alList.map(al => new DPAl(al)).sortBy(_.date.getTime)

      def byTag(tag: AttributeTag): Option[String] = {
        val s = if (sorted.nonEmpty && (sorted.head.al.get(tag) != null))
          Some(sorted.head.al.get(tag).getSingleStringValueOrEmptyString)
        else
          None
        s
      }

      def getReferencedRtplanUID: Option[String] = {
        val seqList = alList.filter(al => al.get(TagFromName.ReferencedRTPlanSequence) != null)
        val refList = seqList.map(seq => DicomUtil.seqToAttr(seq, TagFromName.ReferencedRTPlanSequence)).flatten
        val uidList = refList.filter(ref => ref.get(TagFromName.ReferencedSOPInstanceUID) != null).map(ref => ref.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull)
        val rtplanUid = uidList.filter(uid => uid != null).headOption
        rtplanUid
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

      def getSopInstanceUIDlist = alList.map(al => Util.sopOfAl(al)).mkString(" ", " ", " ")
      def getSeriesInstanceUID = byTag(TagFromName.SeriesInstanceUID).get
      def getFrameOfReferenceUID = byTag(TagFromName.FrameOfReferenceUID)
      def getModality = byTag(TagFromName.Modality).get
      def getSopClassUID = byTag(TagFromName.SOPClassUID).get
      def deviceSerialNumber = byTag(TagFromName.DeviceSerialNumber)
      def getDate = new Timestamp(sorted.head.date.getTime)
      def getPatientID = sorted.map(_.patId).flatten.headOption
      def getSize = sorted.size
      def getContent: Array[Byte] = DicomUtil.dicomToZippedByteArray(alList)

      def getMappedFrameOfReferenceUID: Option[String] = {
        if (getFrameOfReferenceUID.isDefined) {
          val mainFrmOfRef = getFrameOfReferenceUID.get
          val allFrmOfRef = alList.map(al => DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID)).flatten.map(a => a.getSingleStringValueOrNull).filterNot(uid => uid == null).distinct
          val mapped = allFrmOfRef.filterNot(frmOfRef => frmOfRef.equals(mainFrmOfRef)).headOption
          mapped
        } else
          None
      }

      val ds = new DicomSeries(
        None,
        usrPK,
        inpPK,
        derivedMachinePK,
        getSopInstanceUIDlist,
        getSeriesInstanceUID,
        getFrameOfReferenceUID,
        getMappedFrameOfReferenceUID,
        getModality,
        getSopClassUID,
        deviceSerialNumber,
        getDate,
        getPatientID,
        getSize,
        getReferencedRtplanUID,
        getContent)
      Some(ds)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while creating DicomSeries: " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Add this series to the database if it is not already in.  Use the SeriesInstanceUID to determine if it is already in the database.
   */
  def insertIfNew(userPK: Long, inputPK: Option[Long], machinePK: Option[Long], alList: Seq[AttributeList]): Unit = {
    val current = DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(alList.head))
    if (current.isEmpty) {
      val ds = DicomSeries.makeDicomSeries(userPK, inputPK, machinePK, alList)
      if (ds.isDefined) {
        ds.get.insert
        logger.info("inserted DicomSeries in to database: " + ds)
      } else
        logger.warn("Failed to insert new DicomSeries.  userPK: " + userPK + "    inputPK: " + inputPK + "    machinePK: " + machinePK)
    }
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
