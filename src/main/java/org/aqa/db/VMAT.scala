package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import java.util.Date
import java.sql.Timestamp
import edu.umro.ScalaUtil.Trace
import org.aqa.run.ProcedureStatus

/**
 * Encapsulate data from a single VMAT measurement.  Each beam analyzed will have several of these.
 */
case class VMAT(
  vmatPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  status: String, // termination status.  Note that all individual members of a VMAT beam pair may succeed, but the the beam still fail.
  SOPInstanceUIDMLC: String, // UID of source image
  SOPInstanceUIDOpen: String, // UID of open beam
  beamNameMLC: String, // name of beam in plan
  beamNameOpen: String, // name of open beam in plan
  doseMLC_cu: Double, // average dose value in CU
  doseOpen_cu: Double, // average dose value of open in CU
  beamAverage_pct: Double, // average percent dose value for all VMAT readings for this beam
  // The following 4 columns are the top, bottom, left, and right positions of the
  // collimator in mm for this data as specified by the RTPLAN.
  topRtplan_mm: Double,
  bottomRtplan_mm: Double,
  leftRtplan_mm: Double,
  rightRtplan_mm: Double,
  // The following 4 columns are the the top, bottom, left, and right positions of position of
  // rectangle in mm used to take measurements.  This is established by:
  //
  // 1: Extracting the position from the RTPLAN
  // 2: Compensating for central axis shift
  // 3: Shrinking the rectangle by the amount specified in <code>Config.VMATBorderThickness</code> to reduce the effects of edge penumbras
  topAOI_mm: Double,
  bottomAOI_mm: Double,
  leftAOI_mm: Double,
  rightAOI_mm: Double) {

  def insert: VMAT = {
    val insertQuery = VMAT.query returning VMAT.query.map(_.vmatPK) into
      ((vmat, vmatPK) => vmat.copy(vmatPK = Some(vmatPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(VMAT.query.insertOrUpdate(this))

  /** Percent of DR-GS over OPEN. */
  def percent: Double = (doseMLC_cu / doseOpen_cu) * 100

  /** amount that this percentage differs from the average percent: percent - beamAverage_pct. */
  def diff_pct = percent - beamAverage_pct

  override def toString: String = {
    "    vmatPK: " + vmatPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    status: " + status + "\n" +
      "    SOPInstanceUIDMLC: " + SOPInstanceUIDMLC + "\n" +
      "    beamNameMLC: " + beamNameMLC + "\n" +
      "    left,right planned: " + leftRtplan_mm + ", " + rightRtplan_mm + "\n" +
      "    left,right measured: " + leftAOI_mm + ", " + rightAOI_mm + "\n" +
      "    doseMLC_cu: " + Util.fmtDbl(doseMLC_cu) + "\n" +
      "    doseOpen_cu: " + Util.fmtDbl(doseOpen_cu) + "\n" +
      "    percent: " + Util.fmtDbl(percent) + "\n" +
      "    diff_pct: " + Util.fmtDbl(diff_pct) + "\n"
  }
}

object VMAT extends ProcedureOutput {
  class VMATTable(tag: Tag) extends Table[VMAT](tag, "vmat") {

    def vmatPK = column[Long]("vmatPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def SOPInstanceUIDMLC = column[String]("SOPInstanceUIDMLC")
    def SOPInstanceUIDOpen = column[String]("SOPInstanceUIDOpen")
    def beamNameMLC = column[String]("beamNameMLC")
    def beamNameOpen = column[String]("beamNameOpen")
    def doseMLC_cu = column[Double]("doseMLC_cu")
    def doseOpen_cu = column[Double]("doseOpen_cu")
    def beamAverage_pct = column[Double]("beamAverage_pct")
    def topRtplan_mm = column[Double]("topRtplan_mm")
    def bottomRtplan_mm = column[Double]("bottomRtplan_mm")
    def leftRtplan_mm = column[Double]("leftRtplan_mm")
    def rightRtplan_mm = column[Double]("rightRtplan_mm")
    def topAOI_mm = column[Double]("topAOI_mm")
    def bottomAOI_mm = column[Double]("bottomAOI_mm")
    def leftAOI_mm = column[Double]("leftAOI_mm")
    def rightAOI_mm = column[Double]("rightAOI_mm")

    def * = (
      vmatPK.?,
      outputPK,
      status,
      SOPInstanceUIDMLC,
      SOPInstanceUIDOpen,
      beamNameMLC,
      beamNameOpen,
      doseMLC_cu,
      doseOpen_cu,
      beamAverage_pct,
      topRtplan_mm,
      bottomRtplan_mm,
      leftRtplan_mm,
      rightRtplan_mm,
      topAOI_mm,
      bottomAOI_mm,
      leftAOI_mm,
      rightAOI_mm) <> ((VMAT.apply _)tupled, VMAT.unapply _)

    def outputFK = foreignKey("VMAT_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[VMATTable]

  override val topXmlLabel = "VMAT"

  def get(vmatPK: Long): Option[VMAT] = {
    val action = for {
      inst <- VMAT.query if inst.vmatPK === vmatPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[VMAT] = {
    val action = for {
      inst <- VMAT.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(vmatPK: Long): Int = {
    val q = query.filter(_.vmatPK === vmatPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[VMAT]) = {
    val ops = list.map { imgId => VMAT.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[VMAT]): Unit = {
    val ops = list.map { loc => VMAT.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class VMATHistory(date: Date, vmat: VMAT) {
    override def toString = {
      "date: " + date + "    " + vmat
    }
  }

  /**
   * Get VMAT results.
   *
   * @param machinePK: For this machine
   *
   * @param procedurePK: For this procedure
   *
   */
  def history(machinePK: Long, procedurePK: Long) = {

    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      vmat <- VMAT.query.filter(c => c.outputPK === output._1)
    } yield ((output._2, vmat))

    val result = Db.run(search.result).map(h => new VMATHistory(h._1.get, h._2)).sortBy(_.date.getTime)
    result
  }

  /** True if each individual beam passed. */
  def individualBeamsAllPassed(vmatList: Seq[VMAT]): Boolean = {
    vmatList.filterNot(vmat => vmat.status.equals(ProcedureStatus.pass.toString)).size == 0
  }

  /** True if the beam as a whole passed. */
  def beamPassed(vmatList: Seq[VMAT]) = {
    val indiv = individualBeamsAllPassed(vmatList)
    val groupPassed = vmatList.map(vmat => Config.VMATAverageOfAbsoluteDeviationThreshold_pct >= ((vmat.percent - vmat.beamAverage_pct).abs)).reduce(_ && _)
    indiv && groupPassed
  }

}
