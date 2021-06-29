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
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.AngleType
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import java.sql.Timestamp
import java.util.Date
import javax.vecmath.Point3d
import scala.xml.Elem

/**
 * Store the analysis results for one EPID image containing a BB.
 */
case class BBbyEPID(
                     bbByEPIDPK: Option[Long], // primary key
                     outputPK: Long, // output primary key
                     epidSOPInstanceUid: String, // SOP instance UID of EPID image
                     offset_mm: Double, // distance between measured EPID position and expected (plan) location (aka: positioning error)
                     gantryAngle_deg: Double, // gantry angle in degrees
                     status: String, // termination status
                     epidImageX_mm: Double, // X position in EPID image in DICOM gantry coordinates.  Origin (0,0) is in the center, positive direction is to the right.
                     epidImageY_mm: Double, // Y position in EPID image in DICOM gantry coordinates.  Origin (0,0) is in the center, positive direction is up.  Note the the direction is opposite to that given by <code>edu.umro.ImageUtil.IsoImagePlaneTranslator</code>.
                     epid3DX_mm: Double, // X position in EPID in 3D plan space
                     epid3DY_mm: Double, // Y position in EPID in 3D plan space
                     epid3DZ_mm: Double, // Z position in EPID in 3D plan space
                     tableXlateral_mm: Double, // table position in X dimension / lateral
                     tableYvertical_mm: Double, // table position in Y dimension / vertical
                     tableZlongitudinal_mm: Double, // table position in Z dimension / longitudinal
                     metadata_dcm_zip: Option[Array[Byte]]) { // DICOM without image for the slice referenced by this EPID

  def insert: BBbyEPID = {
    val insertQuery = BBbyEPID.query returning BBbyEPID.query.map(_.bbByEPIDPK) into ((bbByEPID, bbByEPIDPK) => bbByEPID.copy(bbByEPIDPK = Some(bbByEPIDPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(BBbyEPID.query.insertOrUpdate(this))

  override def toString: String =
    "bbByEPIDPK : " + bbByEPIDPK +
      "\n    outputPK : " + outputPK +
      "\n    epidSOPInstanceUid : " + epidSOPInstanceUid +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    gantryAngle_deg : " + Util.fmtDbl(gantryAngle_deg) +
      "\n    status : " + status +
      "\n    epid image X,Y : " + Util.fmtDbl(epidImageX_mm) + ", " + Util.fmtDbl(epidImageY_mm) +
      "\n    epid 3D X,Y,Z : " + Util.fmtDbl(epid3DX_mm) + ", " + Util.fmtDbl(epid3DY_mm) + ", " + Util.fmtDbl(epid3DZ_mm) +
      "\n    table Xlat,Yvert,Zlong : " + Util.fmtDbl(tableXlateral_mm) + ", " + Util.fmtDbl(tableYvertical_mm) + ", " + Util.fmtDbl(tableZlongitudinal_mm)

  val epid = new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm)

  val isVert: Boolean = AngleType.isAngleType(Util.angleRoundedTo90(gantryAngle_deg), AngleType.vertical)
  val isHorz: Boolean = !isVert

  val attributeList: AttributeList = {
    if (metadata_dcm_zip.isEmpty || metadata_dcm_zip.get.isEmpty)
      new AttributeList
    else
      DicomUtil.zippedByteArrayToDicom(metadata_dcm_zip.get).head
  }
}

object BBbyEPID extends ProcedureOutput with Logging {

  class BBbyEPIDTable(tag: Tag) extends Table[BBbyEPID](tag, "bbByEPID") {

    def bbByEPIDPK = column[Long]("bbByEPIDPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def epidSOPInstanceUid = column[String]("epidSOPInstanceUid")

    def offset_mm = column[Double]("offset_mm")

    def gantryAngle_deg = column[Double]("gantryAngle_deg")

    def status = column[String]("status")

    def epidImageX_mm = column[Double]("epidImageX_mm")

    def epidImageY_mm = column[Double]("epidImageY_mm")

    def epid3DX_mm = column[Double]("epid3DX_mm")

    def epid3DY_mm = column[Double]("epid3DY_mm")

    def epid3DZ_mm = column[Double]("epid3DZ_mm")

    def tableXlateral_mm = column[Double]("tableXlateral_mm")

    def tableYvertical_mm = column[Double]("tableYvertical_mm")

    def tableZlongitudinal_mm = column[Double]("tableZlongitudinal_mm")

    def metadata_dcm_zip = column[Option[Array[Byte]]]("metadata_dcm_zip")

    def * = (
      bbByEPIDPK.?,
      outputPK,
      epidSOPInstanceUid,
      offset_mm,
      gantryAngle_deg,
      status,
      epidImageX_mm,
      epidImageY_mm,
      epid3DX_mm,
      epid3DY_mm,
      epid3DZ_mm,
      tableXlateral_mm,
      tableYvertical_mm,
      tableZlongitudinal_mm,
      metadata_dcm_zip) <> (BBbyEPID.apply _ tupled, BBbyEPID.unapply)

    def outputFK = foreignKey("BBbyEPID_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyEPIDTable]

  override val topXmlLabel = "BBbyEPID"

  def get(bbByEPIDPK: Long): Option[BBbyEPID] = {
    val action = for {
      inst <- BBbyEPID.query if inst.bbByEPIDPK === bbByEPIDPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all BBbyEPID for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyEPID] = {
    val action = for {
      inst <- BBbyEPID.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(bbByEPIDPK: Long): Int = {
    val q = query.filter(_.bbByEPIDPK === bbByEPIDPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyEPID] = {
    if ((outputPK == -1) || (elem != null)) throw new RuntimeException("BBbyEPID.xmlToList should never be called")
    Seq()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyEPID]): Unit = {
    val ops = list.map { loc => BBbyEPID.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class BBbyEPIDHistory(date: Date, bbByEPID: BBbyEPID) {
    override def toString: String = {
      "date: " + date + "    " + bbByEPID
    }
  }

  /**
   * Get the BBbyEPID results that are nearest in time to the given date.
   *
   * @param machinePK   : For this machine
   * @param procedurePK : For this procedure
   */
  def history(machinePK: Long, procedurePK: Long): Seq[BBbyEPIDHistory] = {
    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      bbByEPID <- BBbyEPID.query.filter(c => c.outputPK === output._1)
    } yield (output._2, bbByEPID)
    //println(sorted.result.statements.mkString("\n    "))
    val result = Db.run(search.result).map(h => BBbyEPIDHistory(h._1.get, h._2)).sortBy(_.date.getTime)
    result
  }

  /** EPID data and related results. */
  case class DailyDataSetEPIDJJ(output: Output, machine: Machine, bbByEPID: BBbyEPID) {

    private val angType = AngleType.classifyAngle(bbByEPID.gantryAngle_deg)

    def isHorz: Boolean = angType.isDefined && angType.get.toString.equals(AngleType.horizontal.toString)

    def isVert: Boolean = angType.isDefined && angType.get.toString.equals(AngleType.vertical.toString)
  }

  /**
   * Get all results that were acquired on one day for one institution.
   */
  def getForOneDayX(date: Date, institutionPK: Long): Seq[DailyDataSetEPIDJJ] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate))
      bbByEPID <- BBbyEPID.query.filter(c => c.outputPK === output.outputPK)
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
    } yield (output, machine, bbByEPID)

    val seq = Db.run(search.result).map(omc => DailyDataSetEPIDJJ(omc._1, omc._2, omc._3))
    seq
  }


  /** EPID data and related results. */
  case class DailyDataSetEPID(output: Output, machine: Machine, data: Either[AttributeList, BBbyEPID]) {

    val al: AttributeList = data match {
      case Right(bbyEPID: BBbyEPID) => bbyEPID.attributeList
      case Left(attributeList: AttributeList) => attributeList
    }

    private val angType = AngleType.classifyAngle(al.get(TagByName.GantryAngle).getDoubleValues.head)

    def isHorz: Boolean = angType.isDefined && angType.get.toString.equals(AngleType.horizontal.toString)

    def isVert: Boolean = angType.isDefined && angType.get.toString.equals(AngleType.vertical.toString)
  }


  /**
   * Get all results that were acquired on one day for one institution.
   *
   * @param date          Get results for all EPID results that were created on this day.
   * @param institutionPK Get only for this institution.
   */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetEPID] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val epidProcPk = Procedure.ProcOfBBbyEPID.get.procedurePK.get

    // one day's worth of BBbyCBCT processing attempts.
    // case class OneDay(input: Input, output: Output, machine: Machine, dicomSeries: DicomSeries) {}

    val searchOutput = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate) && (o.procedurePK === epidProcPk))
      input <- Input.query.filter(i => i.inputPK === output.inputPK)
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      dicomSeries <- DicomSeries.query.filter(ds => (ds.inputPK === input.inputPK) && (ds.modality === "RTIMAGE"))
    } yield (output, machine, dicomSeries)

    /** Quickie class for rows from the database. */
    case class OMD(output: Output, machine: Machine, dicomSeries: DicomSeries) {}

    val omdList = Db.run(searchOutput.result).map(omd => OMD(omd._1, omd._2, omd._3))

    // - - - - - - - - - - - - - - - - - - - - - -

    // list all distinct outputs
    val outputSet = omdList.map(_.output.outputPK.get).distinct.toSet

    val searchEpid = for {
      bbByEPID <- BBbyEPID.query.filter(c => c.outputPK.inSet(outputSet))
    } yield bbByEPID

    val epidList = Db.run(searchEpid.result)


    def makePassed(epid: BBbyEPID): Option[DailyDataSetEPID] = {
      omdList.find(omd => omd.dicomSeries.sopUidSeq.contains(epid.epidSOPInstanceUid)) match {
        case Some(o) => Some(DailyDataSetEPID(o.output, o.machine, Right(epid)))
        case _ =>
          logger.warn("Could not find epid in DICOM series list: " + epid)
          None
      }
    }

    /**
     * Given a DICOM series (with output and machine), look for any SOP instances that
     * are not in the EPID list.  For each, make a DailyDataSetEPID.
     *
     * @param omd DICOM series with output and machine
     * @return List of DailyDataSetEPID that do not have a corresponding EPID.
     */
    def makeFailed(omd: OMD): Seq[DailyDataSetEPID] = {

      lazy val alListList = omd.dicomSeries.attributeListList // make this lazy because it might not be needed

      def getAl(dsUid: String): AttributeList = alListList.find(al => Util.sopOfAl(al).equals(dsUid)).get

      def toDaily(dsUid: String): Option[DailyDataSetEPID] = {
        try {
          epidList.find(e => e.epidSOPInstanceUid.equals(dsUid)) match {
            case Some(_) => None
            case None =>
              Some(DailyDataSetEPID(omd.output, omd.machine, Left(getAl(dsUid))))
          }
        }
        catch {
          case t: Throwable =>
            logger.warn("Unexpected error making failed EPID entry for " + omd + " Exception: " + fmtEx(t))
            None
        }
      }


      omd.dicomSeries.sopUidSeq.flatMap(toDaily)
    }

    val passedList = epidList.flatMap(makePassed)
    val failedList = omdList.flatMap(makeFailed)

    passedList ++ failedList
  }

  /**
   * Get the earliest date+time of an EPID result.
   *
   * @param institutionPK Get for this institution.
   * @return The date+time of the first composite result, if there is at least one.
   */
  def getEarliestDate(institutionPK: Long): Option[Timestamp] = {

    val search = for {
      epidOutputPK <- BBbyEPID.query.map(_.outputPK)
      md <- Output.query.filter(o => (epidOutputPK === o.outputPK) && o.dataDate.isDefined).map(o => (o.machinePK, o.dataDate))
      _ <- Machine.query.filter(m => (m.machinePK === md._1) && (m.institutionPK === institutionPK)).map(_.institutionPK)
    } yield md._2

    val list = search.sorted.take(num = 1)

    val result = Db.run(list.result).flatten

    result.headOption
  }
}
