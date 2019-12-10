package org.aqa.db

import slick.driver.PostgresDriver.api._
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

case class SymmetryAndFlatness(
  symmetryAndFlatnessPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan

  axialSymmetry_pct: Double,
  axialSymmetryBaseline_pct: Double,
  axialSymmetryStatus: String,

  transverseSymmetry_pct: Double,
  transverseSymmetryBaseline_pct: Double,
  transverseSymmetryStatus: String,

  flatness_pct: Double,
  flatnessBaseline_pct: Double,
  flatnessStatus: String,

  profileConstancy_pct: Double,
  profileConstancyBaseline_pct: Double,
  profileConstancyStatus: String,

  top_cu: Double, // average value of top point pixels
  bottom_cu: Double, // average value of bottom point pixels
  left_cu: Double, // average value of left point pixels
  right_cu: Double, // average value of right point pixels
  center_cu: Double // average value of center point pixels
) {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(SymmetryAndFlatness.query.insertOrUpdate(this))

  override def toString: String = {
    "    symmetryAndFlatnessPK: " + symmetryAndFlatnessPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    axialSymmetry_pct: " + axialSymmetry_pct + "\n" +
      "    axialSymmetryBaseline_pct: " + axialSymmetryBaseline_pct + "\n" +
      "    axialSymmetryStatus: " + axialSymmetryStatus + "\n" +
      "    transverseSymmetry_pct: " + transverseSymmetry_pct + "\n" +
      "    transverseSymmetryBaseline_pct: " + transverseSymmetryBaseline_pct + "\n" +
      "    transverseSymmetryStatus: " + transverseSymmetryStatus + "\n" +
      "    flatness_pct: " + flatness_pct + "\n" +
      "    flatnessBaseline_pct: " + flatnessBaseline_pct + "\n" +
      "    flatnessStatus: " + flatnessStatus + "\n" +
      "    profileConstancy_pct: " + profileConstancy_pct + "\n" +
      "    profileConstancyBaseline_pct: " + profileConstancyBaseline_pct + "\n" +
      "    profileConstancyStatus: " + profileConstancyStatus + "\n" +
      "    top_cu: " + top_cu + "\n" +
      "    bottom_cu: " + bottom_cu + "\n" +
      "    left_cu: " + left_cu + "\n" +
      "    right_cu: " + right_cu + "\n" +
      "    center_cu: " + center_cu + "\n"
  }
}

object SymmetryAndFlatness extends ProcedureOutput {
  class SymmetryAndFlatnessTable(tag: Tag) extends Table[SymmetryAndFlatness](tag, "symmetryAndFlatness") {

    def symmetryAndFlatnessPK = column[Long]("symmetryAndFlatnessPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def axialSymmetry_pct = column[Double]("axialSymmetry_pct")
    def axialSymmetryBaseline_pct = column[Double]("axialSymmetryBaseline_pct")
    def axialSymmetryStatus = column[String]("axialSymmetryStatus")
    def transverseSymmetry_pct = column[Double]("transverseSymmetry_pct")
    def transverseSymmetryBaseline_pct = column[Double]("transverseSymmetryBaseline_pct")
    def transverseSymmetryStatus = column[String]("transverseSymmetryStatus")
    def flatness_pct = column[Double]("flatness_pct")
    def flatnessBaseline_pct = column[Double]("flatnessBaseline_pct")
    def flatnessStatus = column[String]("flatnessStatus")
    def profileConstancy_pct = column[Double]("profileConstancy_pct")
    def profileConstancyBaseline_pct = column[Double]("profileConstancyBaseline_pct")
    def profileConstancyStatus = column[String]("profileConstancyStatus")
    def top_cu = column[Double]("top_cu")
    def bottom_cu = column[Double]("bottom_cu")
    def left_cu = column[Double]("left_cu")
    def right_cu = column[Double]("right_cu")
    def center_cu = column[Double]("center_cu")

    def * = (
      symmetryAndFlatnessPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      axialSymmetry_pct,
      axialSymmetryBaseline_pct,
      axialSymmetryStatus,
      transverseSymmetry_pct,
      transverseSymmetryBaseline_pct,
      transverseSymmetryStatus,
      flatness_pct,
      flatnessBaseline_pct,
      flatnessStatus,
      profileConstancy_pct,
      profileConstancyBaseline_pct,
      profileConstancyStatus,
      top_cu,
      bottom_cu,
      left_cu,
      right_cu,
      center_cu) <> ((SymmetryAndFlatness.apply _)tupled, SymmetryAndFlatness.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  case class PointSet(top: Double, bottom: Double, right: Double, left: Double, center: Double) {
    private val list = Seq(top, bottom, right, left, center)

    private val min = list.min
    private val max = list.max

    val axialSymmetry = ((top - bottom) / bottom) * 100
    val transverseSymmetry = ((right - left) / left) * 100
    val flatness = ((max - min) / (max + min)) * 100

    def profileConstancy(baseline: PointSet) = {
      val t = (top / center) - (baseline.top / baseline.center)
      val b = (bottom / center) - (baseline.bottom / baseline.center)
      val l = (left / center) - (baseline.left / baseline.center)
      val r = (right / center) - (baseline.right / baseline.center)

      val profConst = ((t + b + l + r) * 100) / 4

      profConst
    }

    override def toString = {
      def fmt(d: Double) = d.formatted("%10f")
      "top: " + fmt(top) +
        "    bottom: " + fmt(bottom) +
        "    right: " + fmt(right) +
        "    left: " + fmt(left) +
        "    center: " + fmt(center)
    }
  }

  val query = TableQuery[SymmetryAndFlatnessTable]

  override val topXmlLabel = "SymmetryAndFlatness"

  def get(symmetryAndFlatnessPK: Long): Option[SymmetryAndFlatness] = {
    val action = for {
      inst <- SymmetryAndFlatness.query if inst.symmetryAndFlatnessPK === symmetryAndFlatnessPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[SymmetryAndFlatness] = {
    val action = for {
      inst <- SymmetryAndFlatness.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(symmetryAndFlatnessPK: Long): Int = {
    val q = query.filter(_.symmetryAndFlatnessPK === symmetryAndFlatnessPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[SymmetryAndFlatness]) = {
    val ops = list.map { imgId => SymmetryAndFlatness.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[SymmetryAndFlatness]): Unit = {
    val ops = list.map { loc => SymmetryAndFlatness.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class SymmetryAndFlatnessHistory(date: Date, symmetryAndFlatness: SymmetryAndFlatness)

  /**
   * Get the SymmetryAndFlatness results that are nearest in time to the given date, preferring those that have an earlier date.
   *
   * Implementation note: Two DB queries are performed, one for data before and including the time stamp given, and another query for after.
   *
   * @param limit: Get up to this many sets of results before and after the given date.  If a
   * limit of 10 is given then get up to 10 sets of results that occurred before and up to
   * 10 that occurred at or after.  This means that up to 20 sets of results could be returned.
   * A set of results is all the center dose values recorded from the beams of a single output.
   *
   * @param machinePK: For this machine
   *
   * @param beamName: For this beam
   *
   * @param procedurePK: For this procedure
   *
   * @param date: Relative to this date.  If None, then use current date.
   */
  def recentHistory(limit: Int, machinePK: Long, procedurePK: Long, beamName: String, date: Option[Timestamp]) = {

    import java.sql.{ Timestamp, Date, Time }
    import org.joda.time.DateTime
    import org.joda.time.{ DateTime, LocalDate, LocalTime, DateTimeZone }
    import org.joda.time.format._

    implicit def jodaTimeMapping: BaseColumnType[DateTime] = MappedColumnType.base[DateTime, Timestamp](
      dateTime => new Timestamp(dateTime.getMillis),
      timeStamp => new DateTime(timeStamp.getTime))

    val dt = if (date.isDefined) date.get else new Timestamp(Long.MaxValue)

    val before = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate < dt)).
          distinct.
          sortBy(_.dataDate.desc).take(limit).
          map(o => (o.outputPK, o.dataDate))
        symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output._1 && c.beamName === beamName)
      } yield ((output._2, symmetryAndFlatness))

      val sorted = search.distinct.sortBy(_._1.desc)
      val result = Db.run(sorted.result)
      result
    }

    def after = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate >= dt)).
          distinct.
          sortBy(_.dataDate).take(limit).
          map(o => (o.outputPK, o.dataDate))
        symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output._1 && c.beamName === beamName)
      } yield ((output._2, symmetryAndFlatness))

      val sorted = search.distinct.sortBy(_._1.asc)
      val result = Db.run(sorted.result)
      result
    }

    //    println("\nbefore:\n    " + before.map(_._1.get).mkString("\n    "))
    //    println("\nafter:\n    " + after.map(_._1.get).mkString("\n    "))
    val all = before ++ after

    // Convert to class and make sure that they are temporally ordered.
    val result = all.map(h => new SymmetryAndFlatnessHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }
}
