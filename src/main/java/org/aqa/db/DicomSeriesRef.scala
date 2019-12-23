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
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile

/**
 * Reference the contents of a DICOM series to allow lookup by DICOM features.
 */
case class DicomSeriesRef(
  dicomSeriesRefPK: Option[Long], // primary key
  inputFilesPK: Long, //  InputFiles that contains the content
  seriesInstanceUID: String, // DICOM SeriesInstanceUID
  sopInstanceUIDList: String, // DICOM SOPInstanceUID of all files in series.
  frameOfReferenceUID: Option[String], // DICOM FrameOfReferenceUID.  For registration files, this is the FrameOfReferenceUID that they convert from (take as input).
  mappedFrameOfReferenceUID: Option[String], // For REG modality only.  DICOM FrameOfReferenceUID that can be mapped to.
  modality: String, // DICOM Modality
  sopClassUID: String, // DICOM SOPClassUID of first file.  A more rigorous definition of modality.
  deviceSerialNumber: Option[String], // DICOM DeviceSerialNumber found in top-level attribute list, if present.
  date: Timestamp, // when data was acquired at the treatment machine.  Value from first file found by checking for (in this order): ContentDate, InstanceCreationDate, AcquisitionDate, CreationDate, SeriesDate
  patientID: Option[String], // Patient ID, if available
  size: Int, // Number of files in series
  referencedRtplanUID: Option[String],
  IsocenterPosition: Option[String]) // blank separated X Y Z list of isocenter position.  Only present if attribute is present in series. 
  {

  /**
   * Insert, returning the row that was inserted.
   *
   */
  def insert: DicomSeriesRef = {
    val insertQuery = DicomSeriesRef.query returning DicomSeriesRef.query.map(_.dicomSeriesRefPK) into ((dicomSeriesRef, dicomSeriesRefPK) => dicomSeriesRef.copy(dicomSeriesRefPK = Some(dicomSeriesRefPK)))

    val result = Db.run(insertQuery += this)
    result
  }

  /**
   * Get the seq of attribute lists that belong to this series.  Fetch them from the file system, reinstantiating them from disk if necessary.
   */
  lazy val attributeListList: Seq[AttributeList] = {
    val inputFiles = InputFiles.get(inputFilesPK).get
    val input = Input.get(inputFiles.inputPK).get

    if (!input.dir.isDirectory) Input.getFilesFromDatabase(inputFiles.inputPK, input.dir.getParentFile)
    // read files from directory and only get the ones that are in this series
    val alList = input.dir.listFiles.map(f => new DicomFile(f)).map(df => df.attributeList).flatten.filter(al => Util.serInstOfAl(al).equals(seriesInstanceUID))
    alList.toSeq
  }

  override def toString = {
    "\n    dicomSeriesRefPK: " + dicomSeriesRefPK +
      "\n    inputFilesPK: " + inputFilesPK +
      "\n    seriesInstanceUID: " + seriesInstanceUID +
      "\n    sopInstanceUIDList: " + sopInstanceUIDList.take(100) +
      "\n    frameOfReferenceUID: " + frameOfReferenceUID +
      "\n    mappedFrameOfReferenceUID: " + mappedFrameOfReferenceUID +
      "\n    modality: " + modality +
      "\n    sopClassUID: " + sopClassUID +
      "\n    deviceSerialNumber: " + deviceSerialNumber +
      "\n    date: " + date +
      "\n    patientID: " + patientID +
      "\n    size (slices): " + size +
      "\n    referencedRtplanUID: " + referencedRtplanUID +
      "\n    IsocenterPosition: " + IsocenterPosition
  }
}

object DicomSeriesRef extends Logging {
  class DicomSeriesRefTable(tag: Tag) extends Table[DicomSeriesRef](tag, "dicomSeriesRef") {

    def dicomSeriesRefPK = column[Long]("dicomSeriesRefPK", O.PrimaryKey, O.AutoInc)
    def inputFilesPK = column[Long]("inputFilesPK")
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
    def IsocenterPosition = column[Option[String]]("IsocenterPosition")

    def * = (
      dicomSeriesRefPK.?,
      inputFilesPK,
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
      IsocenterPosition) <> ((DicomSeriesRef.apply _)tupled, DicomSeriesRef.unapply _)

    def inputFilesFK = foreignKey("inputFilesPK", inputFilesPK, InputFiles.query)(_.inputFilesPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomSeriesRefTable]

  def get(dicomSeriesRefPK: Long): Option[DicomSeriesRef] = {
    val action = for {
      dicomSeriesRef <- query if dicomSeriesRef.dicomSeriesRefPK === dicomSeriesRefPK
    } yield (dicomSeriesRef)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByFrameUIDAndSOPClass(frameUIDSet: Set[String], sopClassUID: String): Seq[DicomSeriesRef] = {
    val action = for {
      dicomSeriesRef <- query if (dicomSeriesRef.frameOfReferenceUID.inSet(frameUIDSet) && (dicomSeriesRef.sopClassUID === sopClassUID))
    } yield (dicomSeriesRef)
    val list = Db.run(action.result)
    list
  }

  def getBySopInstanceUID(sopInstUID: String): Seq[DicomSeriesRef] = {
    val action = for {
      dicomSeriesRef <- query if ((dicomSeriesRef.sopInstanceUIDList === sopInstUID))
    } yield (dicomSeriesRef)
    val list = Db.run(action.result)
    list
  }

  def getBySeriesInstanceUID(seriesInstanceUID: String): Seq[DicomSeriesRef] = {
    val action = for {
      dicomSeriesRef <- query if ((dicomSeriesRef.seriesInstanceUID === seriesInstanceUID))
    } yield (dicomSeriesRef)
    val list = Db.run(action.result)
    list
  }

  def getByPatientID(patientID: String): Seq[DicomSeriesRef] = {
    val action = for {
      dicomSeriesRef <- query if ((dicomSeriesRef.patientID === patientID))
    } yield (dicomSeriesRef)
    val list = Db.run(action.result)
    list
  }

  def delete(dicomSeriesRefPK: Long): Int = {
    val q = query.filter(_.dicomSeriesRefPK === dicomSeriesRefPK)
    val action = q.delete
    Db.run(action)
  }

  /**
   * Construct a DicomSeriesRef from an attribute list and some other required fields.
   */
  def makeDicomSeriesRef(inpFilesPK: Long, alList: Seq[AttributeList]): Option[DicomSeriesRef] = {
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

      def getSopInstanceUIDlist = alList.map(al => Util.sopOfAl(al)).mkString(" ")
      def getSeriesInstanceUID = byTag(TagFromName.SeriesInstanceUID).get
      def getFrameOfReferenceUID = byTag(TagFromName.FrameOfReferenceUID)
      def getModality = byTag(TagFromName.Modality).get
      def getSopClassUID = byTag(TagFromName.SOPClassUID).get
      def deviceSerialNumber = byTag(TagFromName.DeviceSerialNumber)
      def getDate = new Timestamp(sorted.head.date.getTime)
      def getPatientID = sorted.map(_.patId).flatten.headOption
      def getSize = sorted.size

      def getMappedFrameOfReferenceUID: Option[String] = {
        if (getFrameOfReferenceUID.isDefined) {
          val mainFrmOfRef = getFrameOfReferenceUID.get
          val allFrmOfRef = alList.map(al => DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID)).flatten.map(a => a.getSingleStringValueOrNull).filterNot(uid => uid == null).distinct
          val mapped = allFrmOfRef.filterNot(frmOfRef => frmOfRef.equals(mainFrmOfRef)).headOption
          mapped
        } else
          None
      }

      def getIsocenterPosition: Option[String] = {
        DicomUtil.findAllSingle(alList.head, TagFromName.IsocenterPosition).headOption match {
          case Some(ip) => Some(ip.getDoubleValues.mkString(" "))
          case _ => None
        }
      }

      val ds = new DicomSeriesRef(
        None,
        inpFilesPK,
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
        getIsocenterPosition)
      Some(ds)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while creating DicomSeriesRef: " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Get a list of all known RTPLAN device serial numbers
   */
  def planDeviceSerialNumberList = {
    val action = for {
      dicomSeriesRef <- query if ((dicomSeriesRef.sopClassUID === SOPClass.RTPlanStorage))
    } yield (dicomSeriesRef.deviceSerialNumber)
    val list = Db.run(action.result)
    list.flatten.distinct
  }
}
