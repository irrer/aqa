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

/**
 * Store the contents of a DICOM series.
 *
 */
case class DicomSeries(
  dicomSeriesPK: Option[Long], // primary key
  institutionPK: Long, // institution to which this file belongs
  inputPK: Option[Long], // referenced input, if available
  machinePK: Option[Long], // referenced machine, if available
  sopInstanceUID: String, // DICOM SOPInstanceUID of first file in series.  First is defined by the one with the earliest <code>date</code>.
  seriesInstanceUID: String, // DICOM SeriesInstanceUID
  frameOfReferenceUID: Option[String], // DICOM FrameOfReferenceUID
  modality: String, // DICOM Modality
  sopClassUID: String, // DICOM SOPClassUID of first file.  A more rigorous definition of modality.
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
}

object DicomSeries extends Logging {
  class DicomSeriesTable(tag: Tag) extends Table[DicomSeries](tag, "dicomSeries") {

    def dicomSeriesPK = column[Long]("dicomSeriesPK", O.PrimaryKey, O.AutoInc)
    def institutionPK = column[Long]("institutionPK")
    def inputPK = column[Option[Long]]("inputPK")
    def machinePK = column[Option[Long]]("machinePK")
    def sopInstanceUID = column[String]("sopInstanceUID")
    def seriesInstanceUID = column[String]("seriesInstanceUID")
    def frameOfReferenceUID = column[Option[String]]("frameOfReferenceUID")
    def modality = column[String]("modality")
    def sopClassUID = column[String]("sopClassUID")
    def date = column[Timestamp]("date")
    def patientID = column[Option[String]]("patientID")
    def size = column[Int]("size")
    def content = column[Array[Byte]]("content")

    def * = (
      dicomSeriesPK.?,
      institutionPK,
      inputPK,
      machinePK,
      sopInstanceUID,
      seriesInstanceUID,
      frameOfReferenceUID,
      modality,
      sopClassUID,
      date,
      patientID,
      size,
      content) <> ((DicomSeries.apply _)tupled, DicomSeries.unapply _)

    def dicomSeriesFK = foreignKey("dicomSeriesPK", dicomSeriesPK, DicomSeries.query)(_.dicomSeriesPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def institutionFK = foreignKey("institutionPK", institutionPK, DicomSeries.query)(_.institutionPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machineFK = foreignKey("machinePK", machinePK, DicomSeries.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomSeriesTable]

  def get(dicomSeriesPK: Long): Option[DicomSeries] = {
    val action = for {
      outputFiles <- query if outputFiles.dicomSeriesPK === dicomSeriesPK
    } yield (outputFiles)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByOutput(dicomSeriesPK: Long): Option[DicomSeries] = {
    val action = for {
      outputFiles <- query if outputFiles.dicomSeriesPK === dicomSeriesPK
    } yield (outputFiles)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(dicomSeriesPK: Long): Int = {
    val q = query.filter(_.dicomSeriesPK === dicomSeriesPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(dicomSeriesPK: Long): Int = {
    val q = query.filter(_.dicomSeriesPK === dicomSeriesPK)
    val action = q.delete
    Db.run(action)
  }

  def makeDicomSeries(instPK: Option[Long], inpPK: Option[Long], machPK: Option[Long], alList: Seq[AttributeList]): DicomSeries = {

    val datePatID = alList.map(al => Util.extractDateTimeAndPatientIdFromDicom(al))

    case class DPAl(al: AttributeList) {
      private val dp = Util.extractDateTimeAndPatientIdFromDicom(al)
      val date = dp._1.head
      val patId = dp._2
    }

    val sorted = alList.map(al => new DPAl(al)).sortBy(_.date.getTime)

    def byTag(tag: AttributeTag): Option[String] = {
      val s = sorted.head.al.get(tag).getSingleStringValueOrEmptyString
      if (s == null) None else Some(s)
    }

    def getInstitutionPK: Long = {
      (instPK.isDefined, machPK.isDefined, inpPK.isDefined) match {
        case (true, _, _) => instPK.get // if given, use it
        case (_, true, _) => Some(Machine.get(machPK.get).get.institutionPK).get // if machine is given, then get institution from it
        case (_, _, true) => { // if input is given, then get institution from it via user
          val userPK = Input.get(inpPK.get).get.userPK.get
          Some(User.get(userPK).get.institutionPK).get
        }
        case _ => throw new IllegalArgumentException("Could not find institution for construction of DicomSeries")
      }
    }

    def getInputPK = inpPK

    def getMachinePK = {
      (machPK.isDefined, inpPK.isDefined) match {
        case (true, _) => machPK
        case (_, true) => Input.get(inpPK.get).get.machinePK
        case _ => None
      }
    }

    def getSopInstanceUID = byTag(TagFromName.SOPInstanceUID).get

    def getSeriesInstanceUID = byTag(TagFromName.SeriesInstanceUID).get

    def getFrameOfReferenceUID = byTag(TagFromName.FrameOfReferenceUID)

    def getModality = byTag(TagFromName.Modality).get

    def getSopClassUID = byTag(TagFromName.SOPClassUID).get

    def getDate = {
      new Timestamp(sorted.head.date.getTime)
    }

    def getPatientID = sorted.map(_.patId).flatten.headOption

    def getSize = sorted.size

    def getContent: Array[Byte] = {
      ???
    }

    new DicomSeries(
      None,
      getInstitutionPK,
      getInputPK,
      getMachinePK,
      getSopInstanceUID,
      getSeriesInstanceUID,
      getFrameOfReferenceUID,
      getModality,
      getSopClassUID,
      getDate,
      getPatientID,
      getSize,
      getContent)
  }

}
