package org.aqa.db

import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import org.aqa.AngleType
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput
import org.aqa.run.ProcedureStatus

import java.sql.Timestamp
import java.util.Date
import javax.vecmath.Point3d
import scala.xml.Elem

/**
 * Store the analysis results for a set of EPID images containing a BB.  This is derived from
 * the BBbyEPID values to create an evaluation of the composite results of all images in a set.
 */
case class BBbyEPIDComposite(
                              bbByEPIDCompositePK: Option[Long], // primary key
                              outputPK: Long, // output primary key
                              rtplanSOPInstanceUID: Option[String], // UID of RTPLAN
                              epidSeriesInstanceUID: String, // SOP series instance UID of EPID image
                              offset_mm: Double, // distance between measured EPID position and expected (plan) location (aka: positioning error)
                              x_mm: Double, // X position in EPID in 3D plan space
                              y_mm: Double, // Y position in EPID in 3D plan space
                              z_mm: Double, // Z position in EPID in 3D plan space
                              bbByCBCTPK: Option[Long], // referenced CBCT measurement
                              offsetAdjusted_mm: Option[Double], // total distance in 3D plan space adjusted for corresponding CBCT location
                              xAdjusted_mm: Option[Double], // X position in 3D plan space adjusted for corresponding CBCT location
                              yAdjusted_mm: Option[Double], // Y position in 3D plan space adjusted for corresponding CBCT location
                              zAdjusted_mm: Option[Double], //  Z position in 3D plan space adjusted for corresponding CBCT location
                              tableXlateral_mm: Option[Double], // table position change (RTIMAGE - CT) in X dimension / lateral
                              tableYvertical_mm: Option[Double], // table position change (RTIMAGE - CT) in Y dimension / vertical
                              tableZlongitudinal_mm: Option[Double] // table position change (RTIMAGE - CT) in Z dimension / longitudinal
                            ) {

  def insert: BBbyEPIDComposite = {
    val insertQuery = BBbyEPIDComposite.query returning BBbyEPIDComposite.query.map(_.bbByEPIDCompositePK) into ((bbByEPIDComposite, bbByEPIDCompositePK) => bbByEPIDComposite.copy(bbByEPIDCompositePK = Some(bbByEPIDCompositePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(BBbyEPIDComposite.query.insertOrUpdate(this))

  override def toString: String = {

    val refCbct = {
      if (bbByCBCTPK.isEmpty) "CBCT : None"
      else {
        "CBCT : " + bbByCBCTPK.get + " : " + Util.fmtDbl(offsetAdjusted_mm.get) + " : " + Util.fmtDbl(xAdjusted_mm.get) + ", " + Util.fmtDbl(yAdjusted_mm.get) + ", " + Util.fmtDbl(zAdjusted_mm.get)
      }
    }

    "bbByEPIDCompositePK : " + bbByEPIDCompositePK +
      "\n    outputPK : " + outputPK +
      "\n    rtplanSOPInstanceUID : " + rtplanSOPInstanceUID +
      "\n    epidSeriesInstanceUID : " + epidSeriesInstanceUID +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    epid 3D X,Y,Z : " + Util.fmtDbl(x_mm) + ", " + Util.fmtDbl(y_mm) + ", " + Util.fmtDbl(z_mm) +
      "\n    " + refCbct +
      "\n    table Xlat,Yvert,Zlong : " + Util.fmtDbl(tableXlateral_mm) + ", " + Util.fmtDbl(tableYvertical_mm) + ", " + Util.fmtDbl(tableZlongitudinal_mm)
  }

  Util.fmtDbl(9)
  val epid = new Point3d(x_mm, y_mm, z_mm)
}

object BBbyEPIDComposite extends ProcedureOutput with Logging {

  class BBbyEPIDCompositeTable(tag: Tag) extends Table[BBbyEPIDComposite](tag, "bbByEPIDComposite") {

    def bbByEPIDCompositePK = column[Long]("bbByEPIDCompositePK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def rtplanSOPInstanceUID = column[Option[String]]("rtplanSOPInstanceUID")

    def epidSeriesInstanceUID = column[String]("epidSeriesInstanceUID")

    def offset_mm = column[Double]("offset_mm")

    def x_mm = column[Double]("x_mm")

    def y_mm = column[Double]("y_mm")

    def z_mm = column[Double]("z_mm")

    def bbByCBCTPK = column[Option[Long]]("bbByCBCTPK")

    def offsetAdjusted_mm = column[Option[Double]]("offsetAdjusted_mm")

    def xAdjusted_mm = column[Option[Double]]("xAdjusted_mm")

    def yAdjusted_mm = column[Option[Double]]("yAdjusted_mm")

    def zAdjusted_mm = column[Option[Double]]("zAdjusted_mm")

    def tableXlateral_mm = column[Option[Double]]("tableXlateral_mm")

    def tableYvertical_mm = column[Option[Double]]("tableYvertical_mm")

    def tableZlongitudinal_mm = column[Option[Double]]("tableZlongitudinal_mm")

    def * = (
      bbByEPIDCompositePK.?,
      outputPK,
      rtplanSOPInstanceUID,
      epidSeriesInstanceUID,
      offset_mm,
      x_mm,
      y_mm,
      z_mm,
      bbByCBCTPK,
      offsetAdjusted_mm,
      xAdjusted_mm,
      yAdjusted_mm,
      zAdjusted_mm,
      tableXlateral_mm,
      tableYvertical_mm,
      tableZlongitudinal_mm) <> (BBbyEPIDComposite.apply _ tupled, BBbyEPIDComposite.unapply)

    // Note that if the output row is deleted, then this row will be deleted through the reference to BBbyCBCT
    def bbByCBCTFK = foreignKey("BBbyEPIDComposite_bbByCBCTPKConstraint", bbByCBCTPK, BBbyCBCT.query)(_.bbByCBCTPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyEPIDCompositeTable]

  override val topXmlLabel = "BBbyEPIDComposite"

  def get(bbByEPIDCompositePK: Long): Option[BBbyEPIDComposite] = {
    val action = for {
      inst <- BBbyEPIDComposite.query if inst.bbByEPIDCompositePK === bbByEPIDCompositePK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all BBbyEPIDComposite for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyEPIDComposite] = {
    val action = for {
      inst <- BBbyEPIDComposite.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(bbByEPIDCompositePK: Long): Int = {
    val q = query.filter(_.bbByEPIDCompositePK === bbByEPIDCompositePK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyEPIDComposite] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[BBbyEPIDComposite]()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyEPIDComposite]): Unit = {
    val ops = list.map { loc => BBbyEPIDComposite.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class BBbyEPIDCompositeHistory(date: Date, bbByEPIDComposite: BBbyEPIDComposite) {
    override def toString: String = {
      "date: " + date + "    " + bbByEPIDComposite
    }
  }

  /**
   * Get the BBbyEPIDComposite results that are nearest in time to the given date.  The rows must
   * have valid bbByCBCTPK and associated values.
   *
   * @param machinePK   : For this machine
   * @param procedurePK : For this procedure
   */
  def history(machinePK: Long, procedurePK: Long): Seq[BBbyEPIDCompositeHistory] = {
    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      bbByEPIDComposite <- BBbyEPIDComposite.query.filter(c => (c.outputPK === output._1) && c.bbByCBCTPK.isDefined)
    } yield (output._2, bbByEPIDComposite)
    val result = Db.run(search.result).map(h => BBbyEPIDCompositeHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }

  case class DailyDataSetComposite(composite: BBbyEPIDComposite, cbct: BBbyCBCT, machine: Machine, output: Output, bbByEpid: Seq[BBbyEPID], cbctDicomSeries: DicomSeries) {

    // unique reference to this set of data
    val checksum: String = {
      val pkSeq = Seq(composite.bbByEPIDCompositePK, cbct.bbByCBCTPK, machine.machinePK, output.outputPK).flatten ++
        bbByEpid.map(_.bbByEPIDPK.get).sorted
      pkSeq.map(_.toString).mkString(" ")
    }

    private def byType(angleType: AngleType.Value) = bbByEpid.filter(b => AngleType.isAngleType(b.gantryAngle_deg, angleType))

    val vertList: Seq[BBbyEPID] = byType(AngleType.vertical)
    val horzList: Seq[BBbyEPID] = byType(AngleType.horizontal)
    val machineDailyQA: MachineDailyQA = MachineDailyQA.getMachineDailyQAOrDefault(machine.machinePK.get)

    /**
     * Determine if the values are pass, warning, or failure.  Pass means that all values are less than or equal to the
     * pass limit.  Fail means that at least one value is over the limit.  For CBCT there is a single pass/fail limit.
     * For EPID and composite values there are a pass and warning limits.  If any value exceeds the warning limit then it
     * means failure.  Exceeding the pass limit but not the warning limit means warning.  If all values are under the pass
     * limit the it means pass.  Note that the absolute value of values is considered when comparing to the pass or warning
     * limits.
     */
    val status: ProcedureStatus.ProcedureStatus = {

      def exceedsWarning(d: Option[Double]): Boolean = d.isDefined && d.get.abs > machineDailyQA.warningLimit_mm

      def exceedsPass(d: Option[Double]): Boolean = d.isDefined && d.get.abs > machineDailyQA.passLimit_mm

      val vertZ = (vertList.head.epid3DZ_mm - cbct.err_mm.getZ).abs
      val horzZ = (horzList.head.epid3DZ_mm - cbct.err_mm.getZ).abs

      val sliceThickness = {
        val at = cbct.attributeList.get(TagFromName.SliceThickness)
        if (at != null)
          Some(at.getDoubleValues.head)
        else
          None
      }

      val s = 0 match {

        case _ if cbct.err_mm.getX.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail
        case _ if cbct.err_mm.getY.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail
        case _ if cbct.err_mm.getZ.abs > Config.DailyQACBCTLimit_mm => ProcedureStatus.fail

        case _ if exceedsWarning(composite.xAdjusted_mm) => ProcedureStatus.fail
        case _ if exceedsWarning(composite.yAdjusted_mm) => ProcedureStatus.fail
        case _ if exceedsWarning(composite.zAdjusted_mm) => ProcedureStatus.fail

        case _ if vertZ > machineDailyQA.warningLimit_mm => ProcedureStatus.fail
        case _ if horzZ > machineDailyQA.warningLimit_mm => ProcedureStatus.fail

        case _ if composite.offsetAdjusted_mm.isEmpty => ProcedureStatus.fail
        case _ if composite.offsetAdjusted_mm.get.abs > machineDailyQA.warningLimit_mm => ProcedureStatus.fail

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        case _ if exceedsPass(composite.xAdjusted_mm) => ProcedureStatus.warning
        case _ if exceedsPass(composite.yAdjusted_mm) => ProcedureStatus.warning
        case _ if exceedsPass(composite.zAdjusted_mm) => ProcedureStatus.warning

        case _ if vertZ > machineDailyQA.passLimit_mm => ProcedureStatus.warning
        case _ if horzZ > machineDailyQA.passLimit_mm => ProcedureStatus.warning

        case _ if sliceThickness.isDefined && (sliceThickness.get > Config.BBbyCBCTMaximumSliceThickness_mm) => ProcedureStatus.warning

        case _ if composite.offsetAdjusted_mm.get.abs > machineDailyQA.passLimit_mm => ProcedureStatus.warning

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        case _ => ProcedureStatus.pass
      }
      //logger.info("DailyDataSetComposite machine: " + machine.id + " status: " + s)
      s
    }
  }

  /**
   * Get the earliest date+time of a composite EPID result.
   *
   * @param institutionPK Get for this institution.
   * @return The date+time of the first composite reulst, if there is at least one.
   */
  def getEarliestDate(institutionPK: Long): Option[Timestamp] = {

    val search = for {
      compositeOutputPK <- BBbyEPIDComposite.query.map(_.outputPK)
      md <- Output.query.filter(o => (compositeOutputPK === o.outputPK) && o.dataDate.isDefined).map(o => (o.machinePK, o.dataDate))
      machineInstitutionPK <- Machine.query.filter(m => (m.machinePK === md._1) && (m.institutionPK === institutionPK)).map(_.institutionPK)
    } yield (md._2)

    val list = search.sorted.take(num = 1)

    val result = Db.run(list.result).flatten

    result.headOption
  }

  /**
   * Get all results for this institution.
   */
  def getReportingDataSet(institutionPK: Long): Seq[DailyDataSetComposite] = {
    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined)
      bbByEPIDComposite <- BBbyEPIDComposite.query.filter(c => (c.outputPK === output.outputPK) && c.bbByCBCTPK.isDefined)
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      cbct <- BBbyCBCT.query.filter(c => c.bbByCBCTPK === bbByEPIDComposite.bbByCBCTPK)
      bbByEpid <- BBbyEPID.query.filter(b => b.outputPK === output.outputPK)
      dicomSeries <- DicomSeries.query.filter(ds => cbct.cbctSeriesInstanceUid === ds.seriesInstanceUID)
    } yield (bbByEPIDComposite, cbct, machine, output, bbByEpid, dicomSeries)

    val list = Db.run(search.result)
    val dailyQA = list.groupBy(ga => ga._1.outputPK).values.map(g => DailyDataSetComposite(g.head._1, g.head._2, g.head._3, g.head._4, g.map(gg => gg._5), g.head._6))
    val dailyQASeq = dailyQA.toSeq
    dailyQASeq
  }

  /**
   * Get all results that were acquired on one day for one institution.
   */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetComposite] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate))
      bbByEPIDComposite <- BBbyEPIDComposite.query.filter(c => (c.outputPK === output.outputPK) && c.bbByCBCTPK.isDefined)
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      cbct <- BBbyCBCT.query.filter(c => c.bbByCBCTPK === bbByEPIDComposite.bbByCBCTPK)
      bbByEpid <- BBbyEPID.query.filter(b => b.outputPK === output.outputPK)
      dicomSeries <- DicomSeries.query.filter(ds => cbct.cbctSeriesInstanceUid === ds.seriesInstanceUID)
    } yield (bbByEPIDComposite, cbct, machine, output, bbByEpid, dicomSeries)

    val list = Db.run(search.result)
    val dailyQA = list.groupBy(ga => ga._1.outputPK).values.map(g => DailyDataSetComposite(g.head._1, g.head._2, g.head._3, g.head._4, g.map(gg => gg._5), g.head._6))
    dailyQA.toSeq
  }
}
