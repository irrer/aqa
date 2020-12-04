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
import org.aqa.web.WebServer
import scala.util.Try
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import java.io.File
import java.io.ByteArrayInputStream
import com.pixelmed.dicom.DicomInputStream

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
  content: Option[Array[Byte]]) // DICOM in zip form.  If empty, then DICOM must be retrieved from Input
  extends Logging {

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
    logger.info("Inserted DicomSeries: " + this.toString)
    result
  }

  def insertOrUpdate = Db.run(DicomSeries.query.insertOrUpdate(this))

  /**
   * Get the content as a list of <code>AttributeList</code>.
   */
  def attributeListList: Seq[AttributeList] = {

    if (content.nonEmpty) {
      val contentList = FileUtil.writeZipToNamedByteArrays(new ByteArrayInputStream(content.get)).map(_._2)
      val alList = DicomUtil.zippedByteArrayToDicom(content.get)
      alList
    } else {
      val inputContent = InputFiles.getByInputPK(inputPK.get).head.zippedContent
      val alList = DicomUtil.zippedByteArrayToDicom(inputContent)
      val list = alList.filter(al => Util.serInstOfAl(al).equals(seriesInstanceUID))
      list
    }
  }

  override def toString = {

    val contentText = if ((content != null) && (content.isDefined)) {
      "\n    content size: " + content.get.size
    } else {
      "\n    content: null"
    }

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
      contentText
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
    def content = column[Option[Array[Byte]]]("content")

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
    def userFK = foreignKey("DicomSeries_userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Restrict)
    def inputFK = foreignKey("DicomSeries_inputPKConstraint", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machineFK = foreignKey("DicomSeries_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomSeriesTable]

  /**
   * Get using the primary key.
   */
  def get(dicomSeriesPK: Long): Option[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.dicomSeriesPK === dicomSeriesPK
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get using the input key.
   */
  def getByInputPK(inputPK: Long): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.inputPK === inputPK
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def getByFrameUIDAndSOPClass(frameUIDSet: Set[String], sopClassUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if (dicomSeries.frameOfReferenceUID.inSet(frameUIDSet) && (dicomSeries.sopClassUID === sopClassUID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  /**
   * Get the DICOM series based on SopInstanceUID.  Each series has a blank separated
   * list, so the list has to contain the given UID.
   */
  def getBySopInstanceUID(sopInstUID: String): Seq[DicomSeries] = {
    val withBlanks = "% " + sopInstUID + " %"
    val withBlankPrefix = "% " + sopInstUID
    val withBlankSuffix = sopInstUID + " %"
    val action = for {
      dicomSeries <- query if (
        (dicomSeries.sopInstanceUIDList === sopInstUID) ||
        (dicomSeries.sopInstanceUIDList like withBlanks) ||
        (dicomSeries.sopInstanceUIDList like withBlankPrefix) ||
        (dicomSeries.sopInstanceUIDList like withBlankSuffix))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    list
  }

  def getBySeriesInstanceUID(seriesInstanceUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.seriesInstanceUID === seriesInstanceUID))
    } yield (dicomSeries)
    val list = Db.run(action.result)
    Trace.trace("Got list for ser UID " + seriesInstanceUID + "  of size: " + list.size)
    list
  }

  def getByReferencedRtplanUID(referencedRtplanUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.referencedRtplanUID === referencedRtplanUID))
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
      def getContent: Option[Array[Byte]] = {
        if (getModality.equalsIgnoreCase("RTPLAN")) {
          Some(DicomUtil.dicomToZippedByteArray(alList))
        } else None
      }

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
   * Return true if the DICOM series with the given seriesInstanceUID is in the database.
   */
  private def seriesExists(seriesInstanceUID: String): Boolean = {
    val action = for {
      dicomSeries <- query if ((dicomSeries.seriesInstanceUID === seriesInstanceUID))
    } yield (dicomSeries.seriesInstanceUID)
    val size = Db.run(action.length.result)
    size > 0
  }

  /**
   * Add this series to the database if it is not already in.  Use the SeriesInstanceUID to determine if it is already in the database.
   */
  def insertIfNew(userPK: Long, inputPK: Option[Long], machinePK: Option[Long], alList: Seq[AttributeList]): Unit = {
    if (alList.isEmpty) throw new IllegalArgumentException("List of DICOM slices is empty")
    val uidList = alList.map(al => Util.serInstOfAl(al)).distinct
    if (uidList.size > 1) throw new IllegalArgumentException("List of DICOM slices have more than one series UID: " + uidList.mkString("    "))
    if (uidList.isEmpty) throw new IllegalArgumentException("List of DICOM slices has no SeriesInstanceUID's")

    if (seriesExists(uidList.head)) {
      logger.info("Not inserting series into the database because it is already in the database")
      // TODO there are some odd cases where RTPLAN series may be created incrementally, one 'slice' at
      // a time over months.  With this logic, the first slice would be stored, and subsequent ones would
      // be assumed redundant and not stored.  Will have to think about how to handle this.
    } else {
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

  // ------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------
  // ------------------------------------------------------------------------------------------------------------------------

  private def deleteOrphans: Unit = {
    println("deleteOrphans DicomSeries starting ...")
    val start = System.currentTimeMillis

    // get list of inputs
    val actionInput = for { inPk <- Input.query.map(i => i.inputPK) } yield (inPk)
    val inputPKset = Db.run(actionInput.result).toSet

    // get list of DicomSeries
    case class Ref(dicomSeriesPK: Long, inputPK: Option[Long], modality: String);
    val action1 = for { ref <- DicomSeries.query.map(ds => (ds.dicomSeriesPK, ds.inputPK, ds.modality)) } yield (ref)
    val listOfDicomSeries = Db.run(action1.result).map(ref => new Ref(ref._1, ref._2, ref._3))
    logger.info("Number of DicomSeries found: " + listOfDicomSeries.size)

    val listToDelete = listOfDicomSeries.
      filterNot(ref => ref.modality.equalsIgnoreCase("RTPLAN")). // not interested in references to RTPLANs
      filter(ref => ref.inputPK.isDefined).
      filterNot(ref => inputPKset.contains(ref.inputPK.get))

    println("Number of orphans to delete: " + listToDelete.size + " DicomSeries to delete:\n    " + listToDelete.mkString("    \n"))
    if (Config.DicomSeriesDeleteOrphans == Config.Fix.fix) {
      println("Deleting DicomSeries")
      listToDelete.map(ref => DicomSeries.delete(ref.dicomSeriesPK))
    } else
      println("NOT Deleting DicomSeries " + listToDelete.map(ds => ds.dicomSeriesPK).mkString("  "))
    println("deleteOrphans DicomSeries done.  Elapsed ms: " + (System.currentTimeMillis - start))

  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def populateFromInput = {
    println("populate DicomSeries starting ...")
    val start = System.currentTimeMillis
    // get list of inputs
    val inputPKseq = {
      val actionInput = for { inPk <- Input.query.map(i => i.inputPK) } yield (inPk)
      Db.run(actionInput.result)
    }
    println("populate DicomSeries number of inputs to process: " + inputPKseq.size)

    var count = 0
    def ensureDS(inPK: Long) {
      InputFiles.get(inPK) match {
        case Some(inputFiles) => {
          println("Processing files for input " + inPK)
          val seriesList = DicomUtil.zippedByteArrayToDicom(inputFiles.zippedContent).groupBy(al => Util.serInstOfAl(al)) // TODO do not use zippedByteArrayToDicom
          val missing = seriesList.filter(s => DicomSeries.getBySeriesInstanceUID(s._1).isEmpty).map(s => s._2)

          if (missing.nonEmpty) {
            println("For input " + inPK + " number of series missing: " + missing.size)
            count = count + missing.size
            val input = Input.get(inPK).get
            missing.map(alList => {
              val ds = DicomSeries.makeDicomSeries(input.userPK.get, Some(inPK), input.machinePK, alList)
              if (ds.isDefined) {
                if (Config.DicomSeriesPopulateFromInput == Config.Fix.fix) {
                  println("Creating DicomSeries " + ds.get)
                  ds.get.insert
                } else
                  println("NOT Creating DicomSeries series UID " + ds.get.seriesInstanceUID + "  " + ds.get.modality)
              }
            })

          }
        }
        case _ => println("no such input " + inPK)
      }
    }

    inputPKseq.map(inPK => ensureDS(inPK))
    println("populateFromInput done.  Total missing: " + count + "   Elapsed ms: " + (System.currentTimeMillis - start))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Find DicomSeries that have content but should not.
   */
  private def trimOld = {
    val start = System.currentTimeMillis

    val trimList = {
      val action = for {
        dicomSeries <- query.filter(ds => ((ds.modality =!= "RTPLAN") && (ds.content.isDefined))).map(ds => ds.dicomSeriesPK)
      } yield (dicomSeries)
      val list = Db.run(action.result)
      list
    }

    println("Number of DicomSeries rows that need content set to null: " + trimList.size)

    var trimCount = 0
    def nullContent(dsPK: Long) = {
      try {
        val oldDs = DicomSeries.get(dsPK).get
        if ((!oldDs.modality.equals("RTPLAN")) && (oldDs.content.isDefined)) {
          val newDs = oldDs.copy(content = null)
          trimCount = trimCount + 1
          if (Config.DicomSeriesTrim == Config.Fix.fix) {
            Trace.trace
            println("Going to trim content of " + newDs.dicomSeriesPK.get)
            Trace.trace
            newDs.insertOrUpdate
            Trace.trace
            println("Trimmed content of " + newDs.dicomSeriesPK.get)
          } else {
            println("Did NOT trim content of " + newDs.dicomSeriesPK.get)
          }
        } else {
          println("Unexpectedly found DicomSeries to trim that should not be trimmed: " + oldDs)
        }
      } catch {
        case t: Throwable => println("Failed to trimOld for DicomSeries PK " + dsPK + " : " + fmtEx(t))
      }
    }

    trimList.map(dsPK => nullContent(dsPK))
    println("trimOld done.  trimCount: " + trimCount + "  Elapsed ms: " + (System.currentTimeMillis - start))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Find DicomSeries that are linked to input but should not be
   */
  private def unlinkRtplan = {
    val start = System.currentTimeMillis
    val unlinkList = {
      val action = for {
        dicomSeries <- query.filter(ds => ((ds.modality === "RTPLAN") && (ds.inputPK.isDefined))).map(ds => ds.dicomSeriesPK)
      } yield (dicomSeries)
      val list = Db.run(action.result)
      list
    }

    println("Number of DicomSeries rows that need to be unlinked from inputPK: " + unlinkList.size)

    def unlinkInputPK(dsPK: Long) = {
      try {
        val oldDs = DicomSeries.get(dsPK).get
        if ((oldDs.modality.equals("RTPLAN")) && (oldDs.inputPK.isDefined)) {
          val newDs = oldDs.copy(inputPK = None)
          if (Config.DicomSeriesUnlinkInputPK == Config.Fix.fix) {
            newDs.insertOrUpdate
            println("Unlinked content of " + newDs)
          } else {
            println("Did NOT unlink content of " + newDs.dicomSeriesPK.get)
          }
        } else {
          println("Unexpectedly found DicomSeries to unlink that should not be unlinked: " + oldDs)
        }
      } catch {
        case t: Throwable => println("Failed to unlinkInputPK for DicomSeries PK " + dsPK + " : " + fmtEx(t))
      }
    }

    unlinkList.map(dsPK => unlinkInputPK(dsPK))
    println("unlinkRtplan done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def deleteOrphanOutputs = {
    val search = Output.query.filter(o => o.machinePK.isEmpty)
    val list = Db.run(search.result)
    println("list of output PK's that have a null Machine reference: " + list.map(o => o.outputPK.get).mkString("    "))
    println("Number of outputs that have a null Machine reference: " + list.size)

    def deleteOrphan(output: Output) = {
      if (Config.DicomSeriesOrphanOutputs == Config.Fix.fix) {
        try {
          println("removing output " + output.outputPK.get + " with input " + output.inputPK)
          val input = Input.get(output.inputPK).get
          Input.delete(output.inputPK)
          Util.deleteFileTreeSafely(input.dir)
        } catch {
          case t: Throwable => {
            println("error removing output " + output.outputPK.get + " with input " + output.inputPK + " : " + fmtEx(t))
          }
        }
      } else {
        println("NOT removing output " + output.outputPK.get + " with input " + output.inputPK)
      }
    }

    list.map(output => deleteOrphan(output))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def validateContent(ds: DicomSeries): Boolean = {
    try {
      println("getting DICOM for " + ds.seriesInstanceUID)
      val alList = ds.attributeListList
      println("got DICOM for " + ds.seriesInstanceUID)
      val count = alList.map(al => println(Util.sopOfAl(al))).size
      true
    } catch {
      case t: Throwable => {
        println("Unable to validate DICOM series for dicomSeriesPK: " + ds.dicomSeriesPK + "\n" + fmtEx(t))
        false
      }
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Verify that the series is in the database.  If not, and if 'fix' is set, then put it in the database.
   */
  private def verifySharedInDicomSeries = {
    Trace.trace
    val series = Util.listDirFiles(Config.sharedDir).map(f => (new DicomFile(f).attributeList)).flatten.groupBy(al => Util.serInstOfAl(al))
    Trace.trace("Number of series in shared: " + series.size)

    def saveSeries(serUID: String, alList: Seq[AttributeList]) = {
      val ds = DicomSeries.getBySeriesInstanceUID(serUID)
      if (ds.nonEmpty)
        println("Shared DicomSeries already in database: " + serUID + "    " + Util.modalityOfAl(alList.head))
      else {
        println("Shared DicomSeries NOT in database: " + serUID + "    " + Util.modalityOfAl(alList.head))

        val machinePK = {

          val machByRefPlan: Option[Long] = {
            val dsList = DicomSeries.getByReferencedRtplanUID(Util.sopOfAl(alList.head))
            if (dsList.nonEmpty) {
              Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    Found machine by plan ref: " + dsList.head.machinePK)
              dsList.head.machinePK
            } else {
              Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    Did not find machine by plan ref.")
              None
            }
          }

          val machByPatId: Option[Long] = {
            val patId = Util.patientIdOfAl(alList.head)

            val dsList = {
              val action = for {
                dicomSeries <- query if ((dicomSeries.patientID === patId))
              } yield (dicomSeries)
              val list = Db.run(action.result)
              list
            }
            val machPk = dsList.map(ds => ds.machinePK).flatten.headOption
            if (machPk.nonEmpty) {
              Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    Found machine by patient ref: " + machPk)
              dsList.head.machinePK
            } else {
              Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    Did not find machine by patient ref.")
              None
            }
          }

          Seq(machByRefPlan, machByPatId).flatten.headOption
        }
        Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    machinePK is : " + machinePK)

        val institutionPK: Option[Long] = {
          val instPkByMach: Option[Long] = if (machinePK.isDefined) Some(Machine.get(machinePK.get).get.institutionPK) else None

          val sopList = DicomAnonymous.getAttributeByValue(Util.sopOfAl(alList.head))
          val instPkBySop: Option[Long] = if (sopList.size == 1) Some(sopList.head.institutionPK) else None

          val all = Seq(instPkByMach, instPkBySop).flatten.distinct
          if (all.size > 1) {
            Trace.trace("for series " + series.head._1 + " ambiguous as to which institution it belongs to: " + all.toString)
            None
          } else all.headOption
        }

        Trace.trace("for SOP " + Util.sopOfAl(alList.head) + "    institutionPK: " + institutionPK + "    machinePK: " + machinePK)

        if (institutionPK.isDefined) {

          // val machine = Machine.get(machinePK.get).get
          //val user = User.getOrMakeInstitutionAdminUser(machine.institutionPK)
          val user = User.getOrMakeInstitutionAdminUser(institutionPK.get)

          def putInDb(al: AttributeList) = {
            val ds = DicomSeries.makeDicomSeries(user.userPK.get, None, machinePK, Seq(al))
            if (ds.isDefined) {
              if (Config.DicomSeriesShared == Config.Fix.fix) {
                val dsFinal = ds.get.insert
                println("Shared DicomSeries has been put in database: " + dsFinal)
              } else println("Shared DicomSeries would have been been put in database: " + ds)
            }
          }

          alList.map(al => putInDb(al))
        } else
          println("Could not determine institution for " + serUID)
      }
    }

    Trace.trace
    series.filter(_._2.nonEmpty).map(s => saveSeries(s._1, s._2))
    Trace.trace
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def putRtplansFromInputsIntoDicomSeries = {
    Trace.trace
    val list = {
      val action = for { inFiles <- InputFiles.query } yield (inFiles.inputFilesPK, inFiles.inputPK)
      Db.run(action.result)
    }
    Trace.trace("Number of InputFiles: " + list.size)

    def putIfNeeded(ser: String, rtplan: Seq[AttributeList]) = {
      val dsList = DicomSeries.getBySeriesInstanceUID(ser)
      if (dsList.isEmpty) {
        Trace.trace("Series " + ser + " is not in DicomSeries table.")
        if (Config.DicomSeriesInput == Config.Fix.fix) {
          Trace.trace("want to put " + ser + " in DicomSeries table.")
          //          val user: Long ={
          //            input.userPK match {
          //              case Some( userPK) => userPK
          //              case _ =>   User.getInstitutionAdminUser(input.institutionPK)
          //            }
          //          }
          //
          //          val newDs = DicomSeries.makeDicomSeries(input.userPK, None, ???, rtplan)
          //          if (newDs.isDefined) {
          //            Trace.trace("Did put " + ser + " in DicomSeries table with pk " + newDs.get.dicomSeriesPK)
          //          } else {
          //            Trace.trace("Could not make " + ser)
          //          }
          //        } else {
          //          Trace.trace("Not putting " + ser + " in DicomSeries table.")
        }
      } else {
        Trace.trace("Series " + ser + " is already in DicomSeries table.")
      }
    }

    def putFp(filePk: Long, inPk: Long) = {
      val content = InputFiles.get(filePk).get.zippedContent
      val rtplanSeriesList = DicomUtil.zippedByteArrayToDicom(content).filter(al => Util.modalityOfAl(al) == "RTPLAN").groupBy(al => Util.serInstOfAl(al))
      Trace.trace("Number of RTPLAN series found in " + inPk + " : " + rtplanSeriesList.size)
      rtplanSeriesList.map(serPlan => putIfNeeded(serPlan._1, serPlan._2))
    }

    list.map(fp => putFp(fp._1, fp._2))
    Trace.trace
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * List RTPLAN series that have bad/non-unzippable content.  If 'fix', then remove.
   */
  private def findBadRtplans = {
    Trace.trace

    val outDir = new File(Config.tmpDirFile, "saveDs")
    outDir.mkdirs

    val action = for {
      ds <- query if ((ds.modality === "RTPLAN"))
    } yield (
      ds.dicomSeriesPK)
    val list = Db.run(action.result)
    Trace.trace("total RTPLANs in db: " + list.size)

    def process(pk: Long) = {
      val ds = DicomSeries.get(pk).get
      if (ds.content.isDefined && (ds.content.get != null)) {
        val outFile = new File(outDir, "ds-" + pk + "-" + ds.seriesInstanceUID)
        println("writing file " + outFile.getAbsolutePath)
        Util.writeBinaryFile(outFile, ds.content.get)
        println("wrote file " + outFile.getAbsolutePath)
      }
      Trace.trace("findBadRtplans checking DicomSeries " + pk + " : " + ds.seriesInstanceUID)
      if (validateContent(ds)) {
        Trace.trace("content of DicomSeries is ok: " + ds.dicomSeriesPK.get)
      } else {
        Trace.trace("content of DicomSeries is bad: " + ds.dicomSeriesPK.get)

        //        if (Config.DicomSeriesFindBadRtplans == Config.Fix.fix) {
        //          Trace.trace("Deleting: " + ds.dicomSeriesPK.get)
        //          val count = DicomSeries.delete(pk)
        //          Trace.trace("content of DicomSeries Deleted: " + count)
        //        } else {
        //          Trace.trace("check only, not deleting: " + ds.dicomSeriesPK.get)
        //        }

      }
    }

    Trace.trace
    list.map(pk => process(pk))
    Trace.trace("Done with findBadRtplans")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def main(args: Array[String]): Unit = {
    if (Config.validate) {
      DbSetup.init
      val start = System.currentTimeMillis
      Trace.trace("DicomSeries cleanup starting ------------------------------------------------------------------------------")
      if (Config.DicomSeriesDeleteOrphans != Config.Fix.ignore) deleteOrphans
      Trace.trace
      if (Config.DicomSeriesPopulateFromInput != Config.Fix.ignore) populateFromInput
      Trace.trace
      if (Config.DicomSeriesTrim != Config.Fix.ignore) trimOld
      Trace.trace
      if (Config.DicomSeriesUnlinkInputPK != Config.Fix.ignore) unlinkRtplan
      Trace.trace
      if (Config.DicomSeriesShared != Config.Fix.ignore) verifySharedInDicomSeries
      Trace.trace
      if (Config.DicomSeriesOrphanOutputs != Config.Fix.ignore) deleteOrphanOutputs
      Trace.trace
      if (Config.DicomSeriesFindBadRtplans != Config.Fix.ignore) findBadRtplans
      Trace.trace
      if (Config.DicomSeriesInput != Config.Fix.ignore) putRtplansFromInputsIntoDicomSeries
      Trace.trace
      val elapsed = System.currentTimeMillis - start
      Trace.trace("DicomSeries cleanup finished.  Elapsed ms: " + elapsed + " ------------------------------------------------------------------------------")
      System.exit(0)
    }
  }
}
