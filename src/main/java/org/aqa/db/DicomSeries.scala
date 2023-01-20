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

package org.aqa.db

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.web.GetSeries

import java.sql.Timestamp
import java.util.Date

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
    procedurePK: Option[Long], // procedure to which this DICOM was applied
    content: Option[Array[Byte]] // DICOM in zip form.  If empty, then DICOM must be retrieved from Input
) extends Logging {

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
    GetSeries.remove(userPK, patientID)
    result
  }

  def insertOrUpdate(): Int = {
    GetSeries.remove(userPK, patientID)
    Db.run(DicomSeries.query.insertOrUpdate(this))
  }

  /**
    * Get the list of SOPInstanceUID 's that are in this series.
    *
    * @return
    */
  def sopUidSeq: Seq[String] = DicomSeries.sopUidSeq(sopInstanceUIDList)

  /**
    * Get the content as a list of <code>AttributeList</code>.
    */
  def attributeListList: Seq[AttributeList] = {

    if (content.nonEmpty) {
      val alList = DicomUtil.zippedByteArrayToDicom(content.get)
      alList
    } else {
      try {
        val inputContent: Array[Byte] = {
          val all = InputFiles.getByInputPK(inputPK.get)
          if (all.nonEmpty)
            all.head.zippedContent
          else {
            logger.warn("Could not get content for DicomSeries " + this.toString)
            Array()
          }
        }
        val alList =
          try {
            DicomUtil.zippedByteArrayToDicom(inputContent)
          } catch {
            case t: Throwable =>
              logger.warn("Unexpected exception unzipping DICOM from input table: " + fmtEx(t))
              Seq[AttributeList]()
          }
        val list = alList.filter(al => Util.serInstOfAl(al).equals(seriesInstanceUID))
        list
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception.  DicomSeries: " + this.toString + "\n" + t + " :\n" + fmtEx(t))
          Seq()
      }
    }
  }

  override def toString: String = {

    val contentText = if ((content != null) && content.isDefined) {
      "\n    content size: " + content.get.length
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
      "\n    procedurePK: " + procedurePK +
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
    def procedurePK = column[Option[Long]]("procedurePK")
    def content = column[Option[Array[Byte]]]("content")

    def * =
      (
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
        procedurePK,
        content
      ) <> (DicomSeries.apply _ tupled, DicomSeries.unapply)

    /* Note that accidental data deletion is protected by attempts to remove a machine.  If the
       user does confirm that they want a machine deleted, then the associated DicomSeries will be deleted automatically. */
    def userFK =
      foreignKey("DicomSeries_userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Restrict)

    def inputFK =
      foreignKey("DicomSeries_inputPKConstraint", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def machineFK =
      foreignKey("DicomSeries_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def procedureFK =
      foreignKey("DicomSeries_procedurePKConstraint", procedurePK, Procedure.query)(_.procedurePK)
  }

  val query = TableQuery[DicomSeriesTable]

  def sopUidSeq(sopInstanceUIDList: String): Seq[String] = sopInstanceUIDList.split(" ").filterNot(_.isEmpty).toSeq

  /**
    * Get using the primary key.
    */
  def get(dicomSeriesPK: Long): Option[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.dicomSeriesPK === dicomSeriesPK
    } yield dicomSeries
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get using the input key.
    */
  def getByInputPK(inputPK: Long): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.inputPK === inputPK
    } yield dicomSeries
    val list = Db.run(action.result)
    list
  }

  /**
    * Get using the patient ID.
    */
  def getByPatientIdAndModality(patientId: String, modality: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if (dicomSeries.patientID === patientId) && (dicomSeries.modality === modality)
    } yield dicomSeries
    val list = Db.run(action.result)
    list
  }

  def getByFrameUIDAndSOPClass(frameUIDSet: Set[String], sopClassUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.frameOfReferenceUID.inSet(frameUIDSet) && (dicomSeries.sopClassUID === sopClassUID)
    } yield dicomSeries
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
      dicomSeries <- query if (dicomSeries.sopInstanceUIDList === sopInstUID) ||
        (dicomSeries.sopInstanceUIDList like withBlanks) ||
        (dicomSeries.sopInstanceUIDList like withBlankPrefix) ||
        (dicomSeries.sopInstanceUIDList like withBlankSuffix)
    } yield dicomSeries
    val list = Db.run(action.result)
    list
  }

  def getBySeriesInstanceUID(seriesInstanceUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.seriesInstanceUID === seriesInstanceUID
    } yield dicomSeries
    val list = Db.run(action.result)
    list
  }

  def getByReferencedRtplanUID(referencedRtplanUID: String): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.referencedRtplanUID === referencedRtplanUID
    } yield dicomSeries
    val list = Db.run(action.result)
    list
  }

  def getByMachine(machinePK: Long): Seq[DicomSeries] = {
    val action = for {
      dicomSeries <- query if dicomSeries.machinePK === machinePK
    } yield dicomSeries
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
      procedurePK: Option[Long],
      size: Int,
      referencedRtplanUID: Option[String]
  ) {
    def sopUidSeq: Seq[String] = DicomSeries.sopUidSeq(sopInstanceUIDList)
  }

  def getByPatientID(patientID: String): Seq[DicomSeriesWithoutContent] = {
    val action = for {
      ds <- query if ds.patientID === patientID
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
      ds.procedurePK,
      ds.size,
      ds.referencedRtplanUID
    )
    val list = Db.run(action.result)
    val dsLite = list.map(ds => DicomSeriesWithoutContent(ds._1, ds._2, ds._3, ds._4, ds._5, ds._6, ds._7, ds._8, ds._9, ds._10, ds._11, ds._12, ds._13, ds._14, ds._15, ds._16))
    dsLite
  }

  def delete(dicomSeriesPK: Long): Int = {
    // Removing an entry potentially invalidates the GetSeries cache.  Remove the relevant entry.
    try {
      val action = for {
        dicomSeries <- query if dicomSeries.dicomSeriesPK === dicomSeriesPK
      } yield (dicomSeries.userPK, dicomSeries.patientID)
      val list = Db.run(action.result)

      val userPK = list.head._1
      val patientID = list.head._2

      GetSeries.remove(userPK, patientID)
    } catch {
      case t: Throwable => logger.warn("Unable to remove GetSeries cache entry.  dicomSeriesPK: " + dicomSeriesPK + " : " + fmtEx(t))
    }
    val q = query.filter(_.dicomSeriesPK === dicomSeriesPK)
    val action = q.delete
    Db.run(action)
  }

  /**
    * Construct a DicomSeries from an attribute list and some other required fields.
    */
  def makeDicomSeries(usrPK: Long, inpPK: Option[Long], machPK: Option[Long], alList: Seq[AttributeList], procedurePK: Option[Long]): Option[DicomSeries] = {
    // Doing a lot of things with undependable data so wrap this in a try-catch to cover all the bases
    try {
      //val datePatID = alList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al))

      case class DPAl(al: AttributeList) {
        private val dp = Util.extractDateTimeAndPatientIdFromDicomAl(al)
        val date: Date = dp._1.head
        val patId: Option[String] = dp._2
      }

      val sorted = alList.map(al => DPAl(al)).sortBy(_.date.getTime)

      def byTag(tag: AttributeTag): Option[String] = {
        val s =
          if (sorted.nonEmpty && (sorted.head.al.get(tag) != null))
            Some(sorted.head.al.get(tag).getSingleStringValueOrEmptyString)
          else
            None
        s
      }

      def getReferencedRtplanUID: Option[String] = {
        val seqList = alList.filter(al => al.get(TagByName.ReferencedRTPlanSequence) != null)
        val refList = seqList.flatMap(seq => DicomUtil.seqToAttr(seq, TagByName.ReferencedRTPlanSequence))
        val uidList = refList.filter(ref => ref.get(TagFromName.ReferencedSOPInstanceUID) != null).map(ref => ref.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull)
        val rtplanUid = uidList.find(uid => uid != null)
        rtplanUid
      }

      val derivedMachinePK = {
        (machPK.isDefined, inpPK.isDefined) match {
          case (true, _) => machPK
          case (_, true) => Input.get(inpPK.get).get.machinePK
          case _         => None
        }
      }

      if (inpPK.isDefined) {
        val input = Input.get(inpPK.get).get
        if (input.userPK.get != usrPK)
          throw new IllegalArgumentException("User PK " + usrPK + " passed as parameter is different from that referenced by inputPK " + inpPK.get + " --> " + input.userPK.get)
      }

      def getSopInstanceUIDList = alList.map(al => Util.sopOfAl(al)).mkString(" ", " ", " ")

      def getSeriesInstanceUID = byTag(TagFromName.SeriesInstanceUID).get

      def getFrameOfReferenceUID = byTag(TagFromName.FrameOfReferenceUID)

      def getModality = byTag(TagFromName.Modality).get

      def getSopClassUID = byTag(TagFromName.SOPClassUID).get

      def deviceSerialNumber = byTag(TagFromName.DeviceSerialNumber)

      def getDate = new Timestamp(sorted.head.date.getTime)

      def getPatientID = sorted.flatMap(_.patId).headOption

      def getSize = sorted.size

      def getContent: Option[Array[Byte]] = {
        if (getModality.equalsIgnoreCase("RTPLAN")) {
          Some(DicomUtil.dicomToZippedByteArray(alList))
        } else None
      }

      def getMappedFrameOfReferenceUID: Option[String] = {
        if (getFrameOfReferenceUID.isDefined) {
          val mainFrmOfRef = getFrameOfReferenceUID.get
          val allFrmOfRef = alList.flatMap(al => DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID)).map(a => a.getSingleStringValueOrNull).filterNot(uid => uid == null).distinct
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
        getSopInstanceUIDList,
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
        procedurePK,
        getContent
      )
      Some(ds)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while creating DicomSeries: " + fmtEx(t))
        None
    }
  }

  def getRandomForTesting: Option[DicomSeries] = {
    val ds = Db.run(query.take(num = 1).result)
    ds.headOption
  }

  /**
    * Add this series to the database if it is not already in.  Use the SeriesInstanceUID to determine if it is already in the database.
    *
    * If the series is already in the database, but there are new incoming slices that are not in the database, then add them.
    */
  def insertIfNew(userPK: Long, inputPK: Option[Long], machinePK: Option[Long], alList: Seq[AttributeList], procedurePK: Option[Long]): Unit = {
    if (alList.isEmpty) throw new IllegalArgumentException("List of DICOM slices is empty")
    val uidList = alList.map(al => Util.serInstOfAl(al)).distinct
    if (uidList.size > 1) throw new IllegalArgumentException("List of DICOM slices have more than one series UID: " + uidList.mkString("    "))
    if (uidList.isEmpty) throw new IllegalArgumentException("List of DICOM slices has no SeriesInstanceUIDs")
    if (alList.map(Util.serInstOfAl).distinct.size != 1) throw new IllegalArgumentException("Some DICOM slices have different SeriesInstanceUIDs")

    val existing = getBySeriesInstanceUID(Util.serInstOfAl(alList.head))
    if (existing.nonEmpty) {
      val ds = existing.head
      val incomingUidList = alList.map(al => Util.sopOfAl(al))
      val existingUidList = ds.sopUidSeq
      val newSopList = incomingUidList.diff(existingUidList)
      if (newSopList.isEmpty) {
        logger.info("Not inserting series into the database because it is already in the database")
        // a time over months.  With this logic, the first slice would be stored, and subsequent ones would
        // be assumed redundant and not stored.  Will have to think about how to handle this.
      } else {
        // add new slices to DicomSeries
        logger.info("adding " + newSopList.size + " slices to current DicomSeries " + ds.toString)
        val newAlList = alList.filter(al => newSopList.contains(Util.sopOfAl(al)))
        val bothAlList = ds.attributeListList ++ newAlList
        val bothSize = newSopList.size + ds.size
        val bothSopList = (existingUidList ++ incomingUidList).distinct.mkString(" ", " ", " ")
        val bothContent: Option[Array[Byte]] = {
          if (ds.modality.equalsIgnoreCase("RTPLAN")) {
            Some(DicomUtil.dicomToZippedByteArray(bothAlList))
          } else None
        }

        val newDs = ds.copy(size = bothSize, sopInstanceUIDList = bothSopList, content = bothContent)
        val insertCount = newDs.insertOrUpdate()
        logger.info("Updated " + insertCount + " DicomSeries " + newDs.toString)
      }
    } else {
      val ds = DicomSeries.makeDicomSeries(userPK, inputPK, machinePK, alList, procedurePK)
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
  def planDeviceSerialNumberList: Seq[String] = {
    val action = for {
      dicomSeries <- query if dicomSeries.sopClassUID === SOPClass.RTPlanStorage
    } yield dicomSeries.deviceSerialNumber
    val list = Db.run(action.result)
    list.flatten.distinct
  }

  /**
    * Container for UID + procedures.
    * @param rtplanUID SOPInstanceUID for an RTPLAN.
    * @param procedure Procedure that processes the plan.
    */
  case class RtplanProcedure(rtplanUID: String, procedure: Procedure) {
    override def toString: String = rtplanUID.formatted("%-64s") + " " + procedure.fullName
  }

  /**
    * Get the attribute list of the RTPLAN from the database referenced by the given RTIMAGE.
    *
    * @param attributeList Referencing file, usually an RTIMAGE.
    * @return RTPLAN, or None if not found.
    */
  def getRtplan(attributeList: AttributeList): Option[AttributeList] = {
    try {
      val rtplanSop = Util.getRtplanSop(attributeList)
      if (rtplanSop.isDefined) {
        val ds = DicomSeries.getBySopInstanceUID(rtplanSop.get).headOption
        val rtplanAl = ds.get.attributeListList.filter(al => Util.sopOfAl(al).equals(rtplanSop.get)).head
        Some(rtplanAl)
      } else {
        logger.error("Asking for RTPLAN referenced by image, but there is no such reference.  Image modality: " + Util.modalityOfAl(attributeList))
        None
      }
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Find all dicomSeries that are stored more than once.  If the 'delete' flag is true, then
    * delete the input that they reference.  The dicomSeries will be automatically deleting
    * because of the CASCADE constraint.
    *
    *  @param delFlag If true, delete redundant input records.
    */
  private def fixRedundant(delFlag: Boolean): Unit = {

    val procList = Procedure.list

    case class PS(pk: Long, serUid: String, inputPK: Long, modality: String, procPK: Option[Long]) {

      val procName: String = if (procPK.isDefined) procList.find(_.procedurePK.get == procPK.get).get.fullName else "NA"
      override def toString: String =
        pk.formatted("%6d") +
          " inputPK: " + inputPK.formatted("%6d") +
          " : " + serUid.formatted("%-70s") +
          " : " + modality.formatted("%-12s") +
          " : " + procName.formatted("%-16s")
    }

    val action = for {
      dicomSeries <- query if dicomSeries.modality =!= "RTPLAN"
    } yield (dicomSeries.dicomSeriesPK, dicomSeries.seriesInstanceUID, dicomSeries.inputPK, dicomSeries.modality, dicomSeries.procedurePK)
    val list = Db.run(action.result).map(r => PS(r._1, r._2, r._3.get, r._4, r._5))

    val groupList = list.groupBy(_.serUid).filter(_._2.size > 1).values

    val numToDelete = groupList.flatten.size - groupList.size
    logger.info("Number of redundant inputs to delete: " + numToDelete)

    def deleteInput(redList: Seq[PS]): Unit = {
      val sorted = redList.sortBy(_.inputPK)

      def del(ps: PS): Unit = {
        if (delFlag) {
          val st = System.currentTimeMillis()
          Input.delete(ps.inputPK)
          val el = System.currentTimeMillis() - st
          logger.info("del  " + ps + "  DELETED   elapsed ms: " + el)
        } else
          logger.info("del  " + ps)
      }

      sorted.dropRight(1).foreach(del)
      logger.info("keep " + sorted.last + "\n----------------")
    }

    groupList.foreach(deleteInput)
    logger.info("Done fixing inputs.")
  }

  def fixRedundantInBackground(delFlag: Boolean, delay_ms: Long): Unit = {
    class Fix extends Runnable {
      override def run(): Unit = fixRedundant(delFlag)
    }
    Thread.sleep(delay_ms)
    logger.info("Starting fixRedundant...")
    new Thread(new Fix).start()
  }

  /*

  -- This will list the rows that should be deleted:

  -- Postgresql
  select
    "dicomSeriesPK"
   ,"inputPK"
   ,"seriesInstanceUID"
  from "dicomSeries" ou
  where (select count(*) from "dicomSeries" inr
  where inr."seriesInstanceUID" = ou."seriesInstanceUID") > 1
  limit 100

  -- SqlServer
  SELECT TOP (100) [dicomSeriesPK]
     ,[inputPK]
     ,[seriesInstanceUID]
     ,[modality]
     ,[deviceSerialNumber]
     ,[date]
     ,[procedurePK]
   ROM [AQAmsPR].[dbo].[dicomSeries] ou
   where
  ou.modality <> 'RTPLAN'
  AND
  (select count(*) from dicomSeries inr
  where inr.seriesInstanceUID = ou.seriesInstanceUID) > 1

   */

  def main(args: Array[String]): Unit = {

    Trace.trace()
    Config.validate
    Trace.trace()
    DbSetup.init
    Trace.trace()
    Thread.sleep(1000)
    Trace.trace()
    val start = System.currentTimeMillis()

    println("-----------------------------------------------------------------------")
    fixRedundantInBackground(delFlag = true, delay_ms = 100)
    println("-----------------------------------------------------------------------")

    Thread.sleep(10 * 1000)
    println("Done. Elapsed ms: " + (System.currentTimeMillis() - start))
    System.exit(0)
  }
}
