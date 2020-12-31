package org.aqa.db

import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.webrun.phase2.Phase2Util.MaintenanceRecordBaseline
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis.makeBaselineName
import org.aqa.webrun.phase2.wedge.WedgeAnalysis

import java.sql.Timestamp

/**
 * Define values associated with specific machines that are established when the
 * machine is operating as expected.  These values can later be used to quantify
 * how much the values have changed.
 *
 * Note that <code>BaselineContent</code> records may be created to augment a
 * <code>Baseline</code> record.
 */
case class Baseline(
                     baselinePK: Option[Long], // primary key
                     maintenanceRecordPK: Long, // refers to maintenance for which to use this value
                     acquisitionDate: Timestamp, // when data was acquired at the treatment machine.  Different from when this record was created.
                     SOPInstanceUID: Option[String], // UID of DICOM image.  May be empty if not applicable.
                     id: String, // unique identifier for data.  Can contain the concatenation of values such as beam name, energy level, jaw position, energy level, etc.  Should be human readable / user friendly
                     value: String, // text version of value
                     setup: String // <code>BaselineSetup</code> value
                   ) {

  def insert: Baseline = {
    val insertQuery = Baseline.query returning Baseline.query.map(_.baselinePK) into ((baseline, baselinePK) => baseline.copy(baselinePK = Some(baselinePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(Baseline.query.insertOrUpdate(this))
}

object Baseline extends Logging {

  class BaselineTable(tag: Tag) extends Table[Baseline](tag, "baseline") {

    def baselinePK = column[Long]("baselinePK", O.PrimaryKey, O.AutoInc)

    def maintenanceRecordPK = column[Long]("maintenanceRecordPK")

    def acquisitionDate = column[Timestamp]("acquisitionDate")

    def SOPInstanceUID = column[Option[String]]("SOPInstanceUID")

    def id = column[String]("id")

    def value = column[String]("value")

    def setup = column[String]("setup")

    def * = (
      baselinePK.?,
      maintenanceRecordPK,
      acquisitionDate,
      SOPInstanceUID,
      id,
      value,
      setup) <> (Baseline.apply _ tupled, Baseline.unapply _)

    def maintenanceRecordFK = foreignKey("Baseline_maintenanceRecordPKConstraint", maintenanceRecordPK, MaintenanceRecord.query)(_.maintenanceRecordPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BaselineTable]

  def get(baselinePK: Long): Option[Baseline] = {
    val action = for {
      baseline <- query if baseline.baselinePK === baselinePK
    } yield baseline
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Given a machine and Baseline id, get the latest value on or before the given time stamp if it exists.
   */
  def findLatest(machinePK: Long, id: String, timestamp: Timestamp): Option[(MaintenanceRecord, Baseline)] = {
    // Special note: New baselines use the dataDate of the output.  Older baselines use the dataDate
    // of the individual slice.  The problem with the old way is that when searching for a baseline
    // with a timestamp, the only time available is the output time.  So a baseline will not be available
    // for the first data set.  Adding one hour should fix this (assuming that the treatment time was less
    // than one hour).
    val shiftedTimestamp = new Timestamp(timestamp.getTime + (60 * 60 * 1000))

    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.machinePK === machinePK)
        baseline <- Baseline.query.filter(b => (b.id === id) && (b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK) && (b.acquisitionDate <= shiftedTimestamp))
      } yield (maintenanceRecord, baseline)
    } sortBy (_._2.acquisitionDate.desc)

    val list = Db.run(action.result.headOption)
    if (list.isDefined)
      Some((list.get._1, list.get._2))
    else
      None
  }

  def delete(baselinePK: Long): Int = {
    val q = query.filter(_.baselinePK === baselinePK)
    logger.info("deleting baseline " + baselinePK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[Baseline]) = {
    val ops = list.map { bl => Baseline.query.insertOrUpdate(bl) }
    Db.perform(ops)
  }

  /**
   * Given a set of maintenance records, only allow that are baselines and are referenced by baseline
   * values that have an ID that have text in the <code>requiredText</code> list.  Text
   * comparisons are case-insensitive.
   *
   * Note that this will filter out non-baseline maintenance events.
   */
  def filterOutUnrelatedBaselines(maintRecPKset: Set[Long], requiredText: Set[String]): Seq[MaintenanceRecord] = {
    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.maintenanceRecordPK.inSet(maintRecPKset))
        baseline <- Baseline.query.filter(b => (b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK))
      } yield (maintenanceRecord, baseline.id)
    }

    val reqText = requiredText.map(t => t.toLowerCase)

    val list = Db.run(action.result)

    def idInSet(id: String) = {
      val idLo = id.toLowerCase
      val m = reqText.filter(rt => idLo.contains(rt))
      m.nonEmpty
    }

    val acceptable = list.filter(mb => ((!mb._1.category.equals(MaintenanceCategory.setBaseline))) || (idInSet(mb._2))).map(mb => mb._1)

    val result = acceptable.toList.groupBy(m => m.maintenanceRecordPK).values.map(v => v.head).toSeq.sortBy(_.creationTime.getTime)
    result
  }

  /**
   * Construct a baseline object using an attribute list.
   */
  def makeBaseline(maintenanceRecordPK: Long, dataDate: Timestamp, SOPInstanceUID: String, id: String, value: Double): Baseline = {
    new Baseline(None, maintenanceRecordPK, dataDate, Some(SOPInstanceUID), id, value.toString, BaselineSetup.byDefault.toString)
  }

  def main(args: Array[String]): Unit = {

    /**
     * Check off the sym+flat rows to see if a baseline can be found to prove that
     * they are stored in a consistent manner.
     */
    def checkSymFlat(): Unit = {
      val all = Db.run(SymmetryAndFlatness.query.result)
      Trace.trace(all.size)

      def check(sf: SymmetryAndFlatness): Unit = {

        val output = Output.get(sf.outputPK).get

        val sfDesc =
          "symmetryAndFlatnessPK : " + sf.symmetryAndFlatnessPK +
            "    Output: " + sf.outputPK +
            "    machinePK: " + output.machinePK +
            "    dataDate: " + output.dataDate +
            "    beamName: " + sf.beamName
        "    beamName: " + sf.beamName

        if (sf.flatness_pct == sf.flatnessBaseline_pct) {
          Trace.trace("this is a baseline " + sfDesc)
        }
        else {
          val bs = all.find(s => s.flatness_pct == sf.flatness_pct)
          if (bs.isEmpty)
            Trace.trace("could not find baseline " + sfDesc)
          else {
            Trace.trace("did find baseline " + bs.get.symmetryAndFlatnessPK + " : " + sfDesc)
          }
        }
      }

      all.foreach(check)
    }

    def markSymFlat(): Unit = {
      val action = for {
        sf <- SymmetryAndFlatness.query if
        sf.axialSymmetry_pct === sf.axialSymmetryBaseline_pct &&
          sf.transverseSymmetry_pct === sf.transverseSymmetryBaseline_pct &&
          sf.flatness_pct === sf.flatnessBaseline_pct &&
          sf.profileConstancy_pct === sf.profileConstancyBaseline_pct
      } yield {
        sf.symmetryAndFlatnessPK
      }

      // list of all baseline PKs
      val pkList = Db.run(action.result)
      Trace.trace("Sym+Flat PK list size: " + pkList.size + "    list: " + pkList.mkString("  "))

      def markAsBaseline(pk: Long): Unit = {
        val symFlat = SymmetryAndFlatness.get(pk).get
        if (symFlat.isBaseline) {
          Trace.trace("Already marked sym+flat " + pk + " as baseline")
        }
        else {
          val symFlatAsBaseline = symFlat.copy(isBaseline_text = true.toString)
          symFlatAsBaseline.insertOrUpdate()
          Trace.trace("Marked sym+flat " + pk + " as baseline")
        }
      }

      if (true) // set to true to actually change the database
        pkList.foreach(markAsBaseline)

      Trace.trace
    }

    def markWedge(): Unit = {
      val action = for {
        w <- WedgePoint.query if
        w.percentOfBackground_pct === w.baselinePercentOfBackground_pct
      } yield {
        (w.wedgePointPK)
      }

      val pkList = Db.run(action.result)
      Trace.trace("Wedge PK list size: " + pkList.size + "    list: " + pkList.mkString("  "))

      def markAsBaseline(pk: Long): Unit = {
        val wedge = WedgePoint.get(pk).get
        if (wedge.isBaseline)
          Trace.trace("Already marked wedge " + pk + " as baseline")
        else {
          val wedgeAsBaseline = wedge.copy(isBaseline_text = true.toString)
          wedgeAsBaseline.insertOrUpdate()
          Trace.trace("Marked wedge " + pk + " as baseline")
        }
      }

      if (true) // set to true to actually change the database
        pkList.foreach(markAsBaseline)

      Trace.trace
    }

    def getBaseline(machinePK: Long, beamName: String, dataName: String, value: Double, dataDate: Timestamp): MaintenanceRecordBaseline = {
      val id = makeBaselineName(beamName, dataName)
      val maintenanceRecBaseline = Baseline.findLatest(machinePK, id, dataDate) match {
        case Some((maintenanceRecord, baseline)) => MaintenanceRecordBaseline(Some(maintenanceRecord), baseline)
        case _ => MaintenanceRecordBaseline(None, Baseline.makeBaseline(-1, dataDate, SOPInstanceUID = "12.34.56.78", id, value))
      }
      maintenanceRecBaseline
    }

    def checkOldNewWedge(): Unit = {

      def check(w: WedgePoint): Unit = {
        val output = Output.get(w.outputPK).get
        val machinePK = output.machinePK.get
        val dataDate = output.dataDate.get

        val baselineId = WedgeAnalysis.makeWedgeBaselineName(w)
        val maintenanceRecordBaseline = Baseline.findLatest(machinePK, baselineId, dataDate)
        if (maintenanceRecordBaseline.isDefined) {
          val j1 = maintenanceRecordBaseline.get._1
          val oldBaseline = maintenanceRecordBaseline.get._2.value.toDouble
          val newBaseline = WedgePoint.getBaseline(machinePK, w.wedgeBeamName, dataDate).get
          if (newBaseline.percentOfBackground_pct == oldBaseline) {
            Trace.trace("wedge yay")
          }
          else {
            Trace.trace("wedge what")
            Trace.trace("\n" + oldBaseline + "\n" + newBaseline.percentOfBackground_pct + "\n")
          }
          Trace.trace()
        }
      }

      val list = Db.run(WedgePoint.query.result)
      list.foreach(check)
    }

    def checkOldNewSymFlat(): Unit = {

      def check(sf: SymmetryAndFlatness): Unit = {

        val output = Output.get(sf.outputPK).get
        val machinePK = output.machinePK.get
        val dataDate = output.dataDate.get
        val beamName = sf.beamName

        // get the baseline values the old way via maintenance records
        val axial = getBaseline(machinePK, beamName, SymmetryAndFlatnessAnalysis.axialSymmetryName, sf.axialSymmetryBaseline_pct, dataDate).baseline.value.toDouble
        val trans = getBaseline(machinePK, beamName, SymmetryAndFlatnessAnalysis.transverseSymmetryName, sf.transverseSymmetryBaseline_pct, dataDate).baseline.value.toDouble
        val flatness = getBaseline(machinePK, beamName, SymmetryAndFlatnessAnalysis.flatnessName, sf.flatnessBaseline_pct, dataDate).baseline.value.toDouble
        val constancy = getBaseline(machinePK, beamName, SymmetryAndFlatnessAnalysis.profileConstancyName, sf.profileConstancy_pct, dataDate).baseline.value.toDouble

        // Get baseline values the new way.  It should always be defined.
        val b2 = SymmetryAndFlatness.getBaseline(output.machinePK.get, sf.beamName, output.dataDate.get).get

        if (
          (b2.axialSymmetryBaseline_pct == axial) &&
            (b2.transverseSymmetry_pct == trans) &&
            (b2.flatnessBaseline_pct == flatness) &&
            (b2.profileConstancy_pct == constancy)
        )
          Trace.trace("sym+flat yay")
        else {
          Trace.trace("sym+flat What?")

          Trace.trace(b2.axialSymmetryBaseline_pct == axial)
          Trace.trace(b2.transverseSymmetry_pct == trans)
          Trace.trace(b2.flatnessBaseline_pct == flatness)
          Trace.trace(b2.profileConstancy_pct == constancy)

          Trace.trace("\n" + b2.axialSymmetryBaseline_pct + "\n" + axial + "\n")
          Trace.trace("\n" + b2.transverseSymmetry_pct + "\n" + trans + "\n")
          Trace.trace("\n" + b2.flatnessBaseline_pct + "\n" + flatness + "\n")
          Trace.trace("\n" + b2.profileConstancy_pct + "\n" + constancy + "\n")
        }
      }

      val list = Db.run(SymmetryAndFlatness.query.result)

      println("number of sym+flat: " + list.size)
      list.foreach(check)
    }

    def param(name: String) :  Boolean = {
      args.contains(name)
    }

    Trace.trace("Validate Config and DB")

    Config.validate
    DbSetup.init

    Trace.trace("--- Start baseline checks ----------------------------------------------------------")
    println("Possible args: checkSymFlat markSymFlat markWedge checkOldNewSymFlat checkOldNewWedge ")

    if (args.contains("checkSymFlat")) checkSymFlat()
    if (args.contains("markSymFlat")) markSymFlat()
    if (args.contains("markWedge")) markWedge()

    if (args.contains("checkOldNewSymFlat")) checkOldNewSymFlat()
    if (args.contains("checkOldNewWedge")) checkOldNewWedge()

    println("Possible args: checkSymFlat markSymFlat markWedge checkOldNewSymFlat checkOldNewWedge ")
    Trace.trace("--- Finish baseline checks ----------------------------------------------------------")
  }
}
