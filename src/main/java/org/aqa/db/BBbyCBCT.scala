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
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import javax.vecmath.Point3d
import scala.xml.Elem

case class BBbyCBCT(
    bbByCBCTPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    rtplanSOPInstanceUID: String, // UID of RTPLAN
    cbctSeriesInstanceUid: String, // series instance UID of CBCT
    offset_mm: Double, // distance between measured CBCT position and expected (plan) location (aka: positioning error)
    status: String, // termination status
    rtplanX_mm: Double, // expected X position in RTPLAN
    rtplanY_mm: Double, // expected Y position in RTPLAN
    rtplanZ_mm: Double, // expected Z position in RTPLAN
    cbctX_mm: Double, // measured bb X position in CBCT
    cbctY_mm: Double, // measured bb Y position in CBCT
    cbctZ_mm: Double, // measured bb Z position in CBCT
    tableXlateral_mm: Double, // table position in X dimension / lateral
    tableYvertical_mm: Double, // table position in Y dimension / vertical
    tableZlongitudinal_mm: Double, // table position in Z dimension / longitudinal
    metadata_dcm_zip: Option[Array[Byte]]
) { // DICOM without image for one slice from the CBCT's series

  def insert: BBbyCBCT = {
    val insertQuery = BBbyCBCT.query returning BBbyCBCT.query.map(_.bbByCBCTPK) into ((bbByCBCT, bbByCBCTPK) => bbByCBCT.copy(bbByCBCTPK = Some(bbByCBCTPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(BBbyCBCT.query.insertOrUpdate(this))

  override def toString: String =
    "bbByCBCTPK : " + bbByCBCTPK +
      "\n    outputPK : " + outputPK +
      "\n    rtplanSOPInstanceUID : " + rtplanSOPInstanceUID +
      "\n    cbctSeriesInstanceUid : " + cbctSeriesInstanceUid +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    status : " + status +
      "\n    plan X,Y,Z : " + Util.fmtDbl(rtplanX_mm) + ", " + Util.fmtDbl(rtplanY_mm) + ", " + Util.fmtDbl(rtplanZ_mm) +
      "\n    cbct X,Y,Z : " + Util.fmtDbl(cbctX_mm) + ", " + Util.fmtDbl(cbctY_mm) + ", " + Util.fmtDbl(cbctZ_mm) +
      "\n    table Xlat,Yvert,Zlong : " + Util.fmtDbl(tableXlateral_mm) + ", " + Util.fmtDbl(tableYvertical_mm) + ", " + Util.fmtDbl(tableZlongitudinal_mm)

  val rtplan = new Point3d(rtplanX_mm, rtplanY_mm, rtplanZ_mm)

  val cbct = new Point3d(cbctX_mm, cbctY_mm, cbctZ_mm)

  /** CBCT - PLAN */
  val err_mm = new Point3d(cbctX_mm - rtplanX_mm, cbctY_mm - rtplanY_mm, cbctZ_mm - rtplanZ_mm)

  val attributeList: AttributeList = {
    if (metadata_dcm_zip.isEmpty || metadata_dcm_zip.get.isEmpty)
      new AttributeList
    else
      DicomUtil.zippedByteArrayToDicom(metadata_dcm_zip.get).head
  }
}

object BBbyCBCT extends ProcedureOutput {

  class BBbyCBCTTable(tag: Tag) extends Table[BBbyCBCT](tag, "bbByCBCT") {

    def bbByCBCTPK = column[Long]("bbByCBCTPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def rtplanSOPInstanceUID = column[String]("rtplanSOPInstanceUID")

    def cbctSeriesInstanceUid = column[String]("cbctSeriesInstanceUid")

    def offset_mm = column[Double]("offset_mm")

    def status = column[String]("status")

    private def planX_mm = column[Double]("planX_mm")

    private def planY_mm = column[Double]("planY_mm")

    private def planZ_mm = column[Double]("planZ_mm")

    def cbctX_mm = column[Double]("cbctX_mm")

    def cbctY_mm = column[Double]("cbctY_mm")

    def cbctZ_mm = column[Double]("cbctZ_mm")

    def tableXlateral_mm = column[Double]("tableXlateral_mm")

    def tableYvertical_mm = column[Double]("tableYvertical_mm")

    def tableZlongitudinal_mm = column[Double]("tableZlongitudinal_mm")

    def metadata_dcm_zip = column[Option[Array[Byte]]]("metadata_dcm_zip")

    def * =
      (
        bbByCBCTPK.?,
        outputPK,
        rtplanSOPInstanceUID,
        cbctSeriesInstanceUid,
        offset_mm,
        status,
        planX_mm,
        planY_mm,
        planZ_mm,
        cbctX_mm,
        cbctY_mm,
        cbctZ_mm,
        tableXlateral_mm,
        tableYvertical_mm,
        tableZlongitudinal_mm,
        metadata_dcm_zip
      ) <> (BBbyCBCT.apply _ tupled, BBbyCBCT.unapply)

    def outputFK = foreignKey("BBbyCBCT_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyCBCTTable]

  override val topXmlLabel = "BBbyCBCT"

  def get(bbByCBCTPK: Long): Option[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.bbByCBCTPK === bbByCBCTPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all BBbyCBCT for the given output
    */
  def getByOutput(outputPK: Long): Seq[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(bbByCBCTPK: Long): Int = {
    val q = query.filter(_.bbByCBCTPK === bbByCBCTPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  //noinspection ScalaUnusedSymbol
  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyCBCT] = {
    Seq()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyCBCT]): Unit = {
    val ops = list.map { loc => BBbyCBCT.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class BBbyCBCTHistory(date: Date, bbByCBCT: BBbyCBCT) {
    override def toString: String = {
      "date: " + date + "    " + bbByCBCT
    }
  }

  /**
    * Get all BBbyCBCT results that are nearest in time to the given date.
    *
    * @param machinePK   : For this machine
    * @param procedurePK : For this procedure
    *
    */
  def history(machinePK: Long, procedurePK: Long): Seq[BBbyCBCTHistory] = {
    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      bbByCBCT <- BBbyCBCT.query.filter(c => c.outputPK === output._1)
    } yield (output._2, bbByCBCT)
    //println(sorted.result.statements.mkString("\n    "))

    val result = Db.run(search.result).map(h => BBbyCBCTHistory(h._1.get, h._2)).sortBy(_.date.getTime)
    result
  }

  /** CBCT data and related results. */
  case class DailyDataSetCBCT(output: Output, machine: Machine, dicomSeries: DicomSeries, cbct: Option[BBbyCBCT] = None) {

    /**
      * An attribute list that is representative of the CBCT volume.  Try getting it
      * from the BBcyCBCT if possible because it is less resource intensive.
      */
    lazy val al: AttributeList = if (cbct.isDefined) cbct.get.attributeList else dicomSeries.attributeListList.head
  }

  /**
    * Get all results that were acquired on one day for one institution.
    */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetCBCT] = {
    val beginDate = new Timestamp(Util.dateTimeToDate(date).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val cbctProcPk = Procedure.ProcOfBBbyCBCT.get.procedurePK.get

    // one day's worth of BBbyCBCT processing attempts.
    // case class OneDay(input: Input, output: Output, machine: Machine, dicomSeries: DicomSeries) {}

    val searchOutput = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate) && (o.procedurePK === cbctProcPk))
      input <- Input.query.filter(i => i.inputPK === output.inputPK)
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      dicomSeries <- DicomSeries.query.filter(ds => (ds.inputPK === input.inputPK) && (ds.modality === "CT"))
    } yield (output, machine, dicomSeries)

    val oneDay = Db.run(searchOutput.result).map(od => DailyDataSetCBCT(od._1, od._2, od._3))

    // - - - - - - - - - - - - - - - - - - - - - -

    val seriesUidSet = oneDay.map(od => od.dicomSeries.seriesInstanceUID).toSet
    val searchCbct = for {
      bbByCBCT <- BBbyCBCT.query.filter(c => c.cbctSeriesInstanceUid.inSet(seriesUidSet))
    } yield bbByCBCT

    val cbctList = Db.run(searchCbct.result)

    def insertCbct(daily: DailyDataSetCBCT): DailyDataSetCBCT = {
      val cbct = cbctList.find(_.cbctSeriesInstanceUid.equals(daily.dicomSeries.seriesInstanceUID))
      DailyDataSetCBCT(daily.output, daily.machine, daily.dicomSeries, cbct)
    }

    val seq = oneDay.map(insertCbct)

    seq
  }

  /**
    * Get the earliest date+time of an CBCT result.
    *
    * @param institutionPK Get for this institution.
    * @return The date+time of the first composite result, if there is at least one.
    */
  def getEarliestDate(institutionPK: Long): Option[Timestamp] = {

    val search = for {
      cbctOutputPK <- BBbyCBCT.query.map(_.outputPK)
      md <- Output.query.filter(o => (cbctOutputPK === o.outputPK) && o.dataDate.isDefined).map(o => (o.machinePK, o.dataDate))
      _ <- Machine.query.filter(m => (m.machinePK === md._1) && (m.institutionPK === institutionPK)).map(_.institutionPK)
    } yield md._2

    val list = search.sorted.take(num = 1)

    val result = Db.run(list.result).flatten

    result.headOption
  }

  /**
    * For testing only
    * @param args Ignored.
    */
  def main(args: Array[String]): Unit = {
    DbSetup.init
    (0 to 5).foreach(_ => println("-----------------------------------------------------------"))
    Trace.trace()
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val date = dateFormat.parse("2019-05-14")

    getForOneDay(date, 1)
    Trace.trace()
  }

}
