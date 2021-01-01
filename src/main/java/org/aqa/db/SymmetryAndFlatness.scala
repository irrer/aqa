package org.aqa.db

import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import java.sql.Timestamp
import java.util.Date
import scala.xml.Elem

/**
 * Represent the results of a symmetry, flatness, and constancy analysis.
 *
 * Note that the limit for the number columns in Slick is 22, and this is exactly 22 columns.
 */
case class SymmetryAndFlatness(
                                symmetryAndFlatnessPK: Option[Long], // primary key
                                outputPK: Long, // output primary key
                                SOPInstanceUID: String, // UID of source image
                                beamName: String, // name of beam in plan
                                isBaseline_text: String, // If true, then this is to be used as a baseline.  If not preceded chronologically by a baseline, then it will be used as a base even if it is false.  Defaults to false.   Note that this is a string instead of a boolean because boolean is not supported by some databases.

                                axialSymmetry_pct: Double, // deprecated
                                axialSymmetryBaseline_pct: Double, // deprecated
                                axialSymmetryStatus: String, // deprecated

                                transverseSymmetry_pct: Double, // deprecated
                                transverseSymmetryBaseline_pct: Double, // deprecated
                                transverseSymmetryStatus: String, // deprecated

                                flatness_pct: Double, // deprecated
                                flatnessBaseline_pct: Double, // deprecated
                                flatnessStatus: String, // deprecated

                                profileConstancy_pct: Double,
                                profileConstancyBaseline_pct: Double,
                                profileConstancyStatus: String,

                                top_cu: Double, // average value of top point pixels in CU
                                bottom_cu: Double, // average value of bottom point pixels in CU
                                left_cu: Double, // average value of left point pixels in CU
                                right_cu: Double, // average value of right point pixels in CU
                                center_cu: Double // average value of center point pixels in CU
                              ) {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val isBaseline: Boolean = {
    isBaseline_text match {
      case _ if isBaseline_text.equalsIgnoreCase("true") => true
      case _ => false
    }
  }


  private val list = Seq(top_cu, bottom_cu, right_cu, left_cu, center_cu)

  private val min = list.min
  private val max = list.max

  val axialSymmetry: Double = ((top_cu - bottom_cu) / bottom_cu) * 100
  val transverseSymmetry: Double = ((right_cu - left_cu) / left_cu) * 100
  val flatness: Double = ((max - min) / (max + min)) * 100

  def profileConstancy(baseline: SymmetryAndFlatness): Double = {
    val t = (top_cu / center_cu) - (baseline.top_cu / baseline.center_cu)
    val b = (bottom_cu / center_cu) - (baseline.bottom_cu / baseline.center_cu)
    val l = (left_cu / center_cu) - (baseline.left_cu / baseline.center_cu)
    val r = (right_cu / center_cu) - (baseline.right_cu / baseline.center_cu)

    val profConst = ((t + b + l + r) * 100) / 4

    profConst
  }

  /**
   * True if the comparison of the value to the baseline passes.  Otherwise it has failed.
   *
   * @param value         Value being checked.
   * @param baselineValue Known good baseline used as a reference.
   * @return True on pass, false on fail.
   */
  private def doesPass(value: Double, baselineValue: Double): Boolean = {
    val diff = (value - baselineValue).abs
    Config.SymmetryPercentLimit <= diff
  }

  def axialSymmetryPass(baseline: SymmetryAndFlatness): Boolean = doesPass(axialSymmetry, baseline.axialSymmetry)

  def transverseSymmetryPass(baseline: SymmetryAndFlatness): Boolean = doesPass(transverseSymmetry, baseline.transverseSymmetry)

  def flatnessPass(baseline: SymmetryAndFlatness): Boolean = doesPass(flatness, baseline.flatness)

  def profileConstancyPass(baseline: SymmetryAndFlatness): Boolean = doesPass(profileConstancy(baseline), baseline.profileConstancy(baseline))

  def allPass(baseline: SymmetryAndFlatness): Boolean =
    axialSymmetryPass(baseline) &&
      transverseSymmetryPass(baseline) &&
      flatnessPass(baseline) &&
      profileConstancyPass(baseline)

  def insertOrUpdate(): Int = Db.run(SymmetryAndFlatness.query.insertOrUpdate(this))

  override def toString: String = {
    "    symmetryAndFlatnessPK: " + symmetryAndFlatnessPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    isBaseline_text: " + isBaseline_text + "\n" +
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

object SymmetryAndFlatness extends ProcedureOutput with Logging {

  class SymmetryAndFlatnessTable(tag: Tag) extends Table[SymmetryAndFlatness](tag, "symmetryAndFlatness") {

    def symmetryAndFlatnessPK = column[Long]("symmetryAndFlatnessPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def beamName = column[String]("beamName")

    def isBaseline_text = column[String]("isBaseline_text")

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

    //noinspection LanguageFeature
    def * = (
      symmetryAndFlatnessPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      isBaseline_text,
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
      center_cu) <> (SymmetryAndFlatness.apply _ tupled, SymmetryAndFlatness.unapply _)

    def outputFK = foreignKey("SymmetryAndFlatness_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  case class XPointSet(top: Double, bottom: Double, right: Double, left: Double, center: Double) {
    def this(sf: SymmetryAndFlatness) = this(sf.top_cu, sf.bottom_cu, sf.right_cu, sf.left_cu, sf.center_cu)

    private val list = Seq(top, bottom, right, left, center)

    private val min = list.min
    private val max = list.max

    val axialSymmetry: Double = ((top - bottom) / bottom) * 100
    val transverseSymmetry: Double = ((right - left) / left) * 100
    val flatness: Double = ((max - min) / (max + min)) * 100

    def profileConstancy(baseline: XPointSet): Double = {
      val t = (top / center) - (baseline.top / baseline.center)
      val b = (bottom / center) - (baseline.bottom / baseline.center)
      val l = (left / center) - (baseline.left / baseline.center)
      val r = (right / center) - (baseline.right / baseline.center)

      val profConst = ((t + b + l + r) * 100) / 4

      profConst
    }

    override def toString: String = {
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
    } yield inst
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[SymmetryAndFlatness] = {
    val action = for {
      inst <- SymmetryAndFlatness.query if inst.outputPK === outputPK
    } yield inst
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

  def insert(list: Seq[SymmetryAndFlatness]): Seq[Int] = {
    val ops = list.map { imgId => SymmetryAndFlatness.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    logger.error("the insert method should never be called")
    throw new RuntimeException("the insert method should never be called")
  }

  def insertSeq(list: Seq[SymmetryAndFlatness]): Unit = {
    val ops = list.map { loc => SymmetryAndFlatness.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class SymmetryAndFlatnessHistory(date: Date, symmetryAndFlatness: SymmetryAndFlatness)

  /**
   * Get the SymmetryAndFlatness results.
   *
   * @param machinePK   : For this machine
   * @param beamName    : For this beam
   * @param procedurePK : For this procedure
   *
   */
  def history(machinePK: Long, procedurePK: Long, beamName: String): Seq[SymmetryAndFlatnessHistory] = {

    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output._1 && c.beamName === beamName)
    } yield {
      (output._2, symmetryAndFlatness)
    }

    val result = Db.run(search.result).map(h => SymmetryAndFlatnessHistory(h._1.get, h._2)).sortBy(_.date.getTime)

    result
  }

  /**
   * Get the baseline by finding another set of values that
   *   - were captured before the given time stamp
   *   - belong to the same machine
   *   - were produced by the same beam
   *   - are defined as a baseline because <code>isBaseline_text</code> is true, or failing that, have the chronologically earliest preceding <code>SymmetryAndFlatness</code>.
   *
   * @param machinePK Match this machine
   * @param beamName  Match this beam
   * @param dataDate  Most recent that is at or before this time
   * @return The baseline value to use, or None if not found.
   */
  def getBaseline(machinePK: Long, beamName: String, dataDate: Timestamp): Option[SymmetryAndFlatness] = {
    val ts = new Timestamp(dataDate.getTime + (60 * 60 * 1000)) // allow for some leeway in the timestamp

    val result = {
      val search = for {
        output <- Output.query.filter(o => (o.dataDate <= ts) && o.machinePK === machinePK).map(o => o)
        symAndFlat <- SymmetryAndFlatness.query.filter(saf => (saf.outputPK === output.outputPK) && (saf.beamName === beamName))
      } yield (output, symAndFlat)

      // make list of all results, with the most recent first
      val list = Db.run(search.result).sortBy(o => o._1.dataDate.get.getTime).map(os => os._2).reverse
      val b = list.find(_.isBaseline) match {

        // Use the most recent set of values that is marked as a baseline.
        case Some(symmetryAndFlatness: SymmetryAndFlatness) => Some(symmetryAndFlatness)

        // Use the earliest set of results as a baseline even though it is not marked as a baseline.
        case _ => list.lastOption
      }
      b
    }

    result
  }

}
