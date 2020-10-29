package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.run.ProcedureStatus
import javax.vecmath.Point3d
import java.sql.Timestamp
import java.util.Date
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.AttributeList
import java.io.ByteArrayInputStream
import com.pixelmed.dicom.DicomInputStream
import edu.umro.ScalaUtil.DicomUtil

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
  metadata_dcm_zip: Option[Array[Byte]]) { // DICOM without image for one slice from the CBCT's series

  def insert: BBbyCBCT = {
    val insertQuery = BBbyCBCT.query returning BBbyCBCT.query.map(_.bbByCBCTPK) into ((bbByCBCT, bbByCBCTPK) => bbByCBCT.copy(bbByCBCTPK = Some(bbByCBCTPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BBbyCBCT.query.insertOrUpdate(this))

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

  val attributeList = {
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
    def planX_mm = column[Double]("planX_mm")
    def planY_mm = column[Double]("planY_mm")
    def planZ_mm = column[Double]("planZ_mm")
    def cbctX_mm = column[Double]("cbctX_mm")
    def cbctY_mm = column[Double]("cbctY_mm")
    def cbctZ_mm = column[Double]("cbctZ_mm")
    def tableXlateral_mm = column[Double]("tableXlateral_mm")
    def tableYvertical_mm = column[Double]("tableYvertical_mm")
    def tableZlongitudinal_mm = column[Double]("tableZlongitudinal_mm")
    def metadata_dcm_zip = column[Option[Array[Byte]]]("metadata_dcm_zip")

    def * = (
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
      metadata_dcm_zip) <> ((BBbyCBCT.apply _)tupled, BBbyCBCT.unapply _)

    def outputFK = foreignKey("BBbyCBCT_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyCBCTTable]

  override val topXmlLabel = "BBbyCBCT"

  def get(bbByCBCTPK: Long): Option[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.bbByCBCTPK === bbByCBCTPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all BBbyCBCT for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.outputPK === outputPK
    } yield (inst)
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

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyCBCT] = {
    ???
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
    override def toString = {
      "date: " + date + "    " + bbByCBCT
    }
  }

  /**
   * Get all BBbyCBCT results that are nearest in time to the given date.
   *
   * @param machinePK: For this machine
   *
   * @param procedurePK: For this procedure
   *
   */
  def history(machinePK: Long, procedurePK: Long) = {
    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      bbByCBCT <- BBbyCBCT.query.filter(c => c.outputPK === output._1)
    } yield ((output._2, bbByCBCT))
    //println(sorted.result.statements.mkString("\n    "))

    val result = Db.run(search.result).map(h => new BBbyCBCTHistory(h._1.get, h._2)).sortBy(_.date.getTime)
    result
  }

  /**
   * Get the procedure PK for the BBbyCBCT procedure.
   */
  def getProcedurePK: Option[Long] = {
    Db.run(query.result.headOption) match {
      case Some(cbct) => {
        Output.get(cbct.outputPK) match {
          case Some(output) => Some(output.procedurePK)
          case _ => None
        }
      }
      case _ => None
    }
  }

  /** CBCT data and related results. */
  case class DailyDataSetCBCT(output: Output, machine: Machine, bbByCBCT: BBbyCBCT);

  /**
   * Get all results that were acquired on one day for one institution.
   */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetCBCT] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate))
      bbByCBCT <- BBbyCBCT.query.filter(c => (c.outputPK === output.outputPK))
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      cbct <- BBbyCBCT.query.filter(c => c.bbByCBCTPK === bbByCBCT.bbByCBCTPK)
    } yield (output, machine, bbByCBCT)

    val seq = Db.run(search.result).map(omc => new DailyDataSetCBCT(omc._1, omc._2, omc._3))
    seq
  }

  def populateDicom = {
    val action = for {
      e <- BBbyCBCT.query.map(e => e.bbByCBCTPK)
    } yield (e)
    val list = Db.run(action.result)

    Trace.trace("Number of CBCTs to check: " + list.size)
    var count = 0

    def checkSer(cpk: Long) = {
      val cbct = get(cpk).get
      val dsList = DicomSeries.getBySopInstanceUID(cbct.cbctSeriesInstanceUid)
      if (dsList.nonEmpty) {
        val al = dsList.head.attributeListList.head
        val serUID = Util.serInstOfAl(al)
        val cbct2 = cbct.copy(cbctSeriesInstanceUid = serUID)
        cbct2.insertOrUpdate
        count = count + 1
        Trace.trace(count.formatted("%5d") + "  Updated serUID of CBCT " + cbct2)
      }
    }

    Trace.trace
    val start1 = System.currentTimeMillis
    list.map(cpk => checkSer(cpk))
    Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start1) + "    Count of CBCTs serUID updated: " + count)
    Trace.trace

    count = 0
    def check(cpk: Long) = {
      val cbct = get(cpk).get
      Trace.trace("metadata_dcm_zip: " + cbct.metadata_dcm_zip.toString.take(100))
      if ((!cbct.metadata_dcm_zip.isDefined) || cbct.metadata_dcm_zip.get.isEmpty || cbct.metadata_dcm_zip.get.size < 50) {
        val dsList = DicomSeries.getBySeriesInstanceUID(cbct.cbctSeriesInstanceUid)
        Trace.trace("dsList size: " + dsList.size + "    bbByCBCTPK: " + cbct.bbByCBCTPK)
        if (dsList.nonEmpty) {
          Trace.trace("Processing CBCT " + cpk)
          val ds = dsList.head.attributeListList.find(a => Util.serInstOfAl(a).equals(cbct.cbctSeriesInstanceUid))
          if (ds.nonEmpty) {
            val al = ds.get
            val serUID = Util.serInstOfAl(al)
            al.remove(com.pixelmed.dicom.TagFromName.PixelData)
            val content = DicomUtil.dicomToZippedByteArray(Seq(al))
            val cbct2 = cbct.copy(cbctSeriesInstanceUid = serUID, metadata_dcm_zip = Some(content))
            cbct2.insertOrUpdate
            count = count + 1
            Trace.trace(count.formatted("%5d") + "  Updated CBCT " + cbct2)
          } else {
            Trace.trace("Unexpected non-serUID match: " + cbct)
          }
        }
      }
    }

    Trace.trace
    val start2 = System.currentTimeMillis
    list.map(cpk => check(cpk))
    Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start2) + "    Count of CBCTs updated: " + count)
    Trace.trace
  }

}
