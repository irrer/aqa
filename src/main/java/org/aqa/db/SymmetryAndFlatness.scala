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

import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.Db.driver.api._

import java.sql.Timestamp

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
    isBaseline: Boolean, // If true, then this is to be used as a baseline.  If not preceded chronologically by a baseline, then it will be used as a base even if it is false.  Defaults to false.
    top_cu: Double, // average value of top point pixels in CU
    bottom_cu: Double, // average value of bottom point pixels in CU
    left_cu: Double, // average value of left point pixels in CU
    right_cu: Double, // average value of right point pixels in CU
    center_cu: Double, // average value of center point pixels in CU
    topStdDev_cu: Double, // standard deviation of top point pixels in CU
    bottomStdDev_cu: Double, // standard deviation of bottom point pixels in CU
    leftStdDev_cu: Double, // standard deviation of left point pixels in CU
    rightStdDev_cu: Double, // standard deviation of right point pixels in CU
    centerStdDev_cu: Double // standard deviation of center point pixels in CU
) {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val isBaselineFunc: Boolean = isBaseline

  private val list = Seq(top_cu, bottom_cu, right_cu, left_cu, center_cu)

  private val min = list.min
  private val max = list.max

  val axialSymmetry: Double = ((top_cu - bottom_cu) / bottom_cu) * 100
  val transverseSymmetry: Double = ((right_cu - left_cu) / left_cu) * 100
  val flatness: Double = ((max - min) / (max + min)) * 100

  /** Coefficients of Variation. */
  val topCOV: Double = topStdDev_cu / top_cu
  val bottomCOV: Double = bottomStdDev_cu / bottom_cu
  val leftCOV: Double = leftStdDev_cu / left_cu
  val rightCOV: Double = rightStdDev_cu / right_cu
  val centerCOV: Double = centerStdDev_cu / center_cu

  def profileConstancy(baseline: SymmetryAndFlatness): Double = {
    if (symmetryAndFlatnessPK.nonEmpty && baseline.symmetryAndFlatnessPK.get == symmetryAndFlatnessPK.get) {
      0
    } else {
      val t = (top_cu / center_cu) - (baseline.top_cu / baseline.center_cu)
      val b = (bottom_cu / center_cu) - (baseline.bottom_cu / baseline.center_cu)
      val l = (left_cu / center_cu) - (baseline.left_cu / baseline.center_cu)
      val r = (right_cu / center_cu) - (baseline.right_cu / baseline.center_cu)

      val profConst = ((t + b + l + r) * 100) / 4

      profConst
    }
  }

  /**
    * True if the comparison of the value to the baseline passes.  Otherwise it has failed.
    *
    * @param value         Value being checked.
    * @param baselineValue Known good baseline used as a reference.
    * @return True on pass, false on fail.
    */
  private def doesPass(value: Double, baselineValue: Double, limit: Double): Boolean = {
    val diff = (value - baselineValue).abs
    val pass = limit >= diff
    pass
  }

  def axialSymmetryPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass(axialSymmetry, baseline.axialSymmetry, Config.SymmetryPercentLimit)

  def transverseSymmetryPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass(transverseSymmetry, baseline.transverseSymmetry, Config.SymmetryPercentLimit)

  def flatnessPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass(flatness, baseline.flatness, Config.FlatnessPercentLimit)

  def profileConstancyPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass(profileConstancy(baseline), baseline.profileConstancy(baseline), Config.ProfileConstancyPercentLimit)

  def allPass(baseline: SymmetryAndFlatness): Boolean = {
    axialSymmetryPass(baseline) &&
    transverseSymmetryPass(baseline) &&
    flatnessPass(baseline) &&
    profileConstancyPass(baseline)
  }

  def insertOrUpdate(): Int = Db.run(SymmetryAndFlatness.query.insertOrUpdate(this))

  override def toString: String = {
    "    symmetryAndFlatnessPK: " + symmetryAndFlatnessPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    top_cu: " + top_cu + "\n" +
      "    bottom_cu: " + bottom_cu + "\n" +
      "    left_cu: " + left_cu + "\n" +
      "    right_cu: " + right_cu + "\n" +
      "    center_cu: " + center_cu + "\n" +
      "    topStdDev_cu: " + topStdDev_cu + "\n" +
      "    bottomStdDev_cu: " + bottomStdDev_cu + "\n" +
      "    leftStdDev_cu: " + leftStdDev_cu + "\n" +
      "    rightStdDev_cu: " + rightStdDev_cu + "\n" +
      "    centerStdDev_cu: " + centerStdDev_cu + "\n"
  }

}

object SymmetryAndFlatness extends Logging {

  class SymmetryAndFlatnessTable(tag: Tag) extends Table[SymmetryAndFlatness](tag, "symmetryAndFlatness") {

    def symmetryAndFlatnessPK = column[Long]("symmetryAndFlatnessPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def beamName = column[String]("beamName")

    def isBaseline = column[Boolean]("isBaseline")

    def top_cu = column[Double]("top_cu")

    def bottom_cu = column[Double]("bottom_cu")

    def left_cu = column[Double]("left_cu")

    def right_cu = column[Double]("right_cu")

    def center_cu = column[Double]("center_cu")

    def topStdDev_cu = column[Double]("topStdDev_cu")

    def bottomStdDev_cu = column[Double]("bottomStdDev_cu")

    def leftStdDev_cu = column[Double]("leftStdDev_cu")

    def rightStdDev_cu = column[Double]("rightStdDev_cu")

    def centerStdDev_cu = column[Double]("centerStdDev_cu")

    //noinspection LanguageFeature
    def * =
      (
        symmetryAndFlatnessPK.?,
        outputPK,
        SOPInstanceUID,
        beamName,
        isBaseline,
        top_cu,
        bottom_cu,
        left_cu,
        right_cu,
        center_cu,
        topStdDev_cu,
        bottomStdDev_cu,
        leftStdDev_cu,
        rightStdDev_cu,
        centerStdDev_cu
      ) <> (SymmetryAndFlatness.apply _ tupled, SymmetryAndFlatness.unapply)

    def outputFK = foreignKey("SymmetryAndFlatness_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[SymmetryAndFlatnessTable]

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

  case class SymmetryAndFlatnessHistory(output: Output, symmetryAndFlatness: SymmetryAndFlatness, baselineOutput: Output, baseline: SymmetryAndFlatness) extends HasOutput {
    override def getOutput: Output = output
  }

  private case class OutputSymFlat(output: Output, sf: SymmetryAndFlatness) {}

  /**
    * For each member in the list, associate it with its baseline.
    *
    * @param osfListUnsorted List of output sym+flat not sorted
    * @return osfList associated with baselines.
    */
  private def associateBaseline(osfListUnsorted: Seq[OutputSymFlat]): Seq[SymmetryAndFlatnessHistory] = {

    // Sort by timestamp
    val osfList = osfListUnsorted.sortBy(_.output.dataDate.get.getTime)

    case class BaselineAndList(baseline: OutputSymFlat, list: Seq[SymmetryAndFlatnessHistory]) {}

    if (osfList.isEmpty)
      Seq[SymmetryAndFlatnessHistory]()
    else {
      // For each entry, find its baseline.  For the first one this will always be itself.
      val histList = {
        val init = BaselineAndList(osfList.head, Seq[SymmetryAndFlatnessHistory]())
        osfList.foldLeft(init)((baselineAndList, os) =>
          if (os.sf.isBaseline)
            BaselineAndList(os, baselineAndList.list :+ SymmetryAndFlatnessHistory(os.output, os.sf, os.output, os.sf))
          else
            BaselineAndList(
              baselineAndList.baseline,
              baselineAndList.list :+ SymmetryAndFlatnessHistory(os.output, os.sf, baselineAndList.baseline.output, baselineAndList.baseline.sf)
            )
        )
      }
      histList.list
    }
  }

  /**
    * Get the SymmetryAndFlatness results.
    *
    * @param machinePK : For this machine
    * @param beamName  : For this beam
    * @return Complete history with baselines sorted by date.
    *
    */
  def history(machinePK: Long, beamName: String, procedurePK: Long): Seq[SymmetryAndFlatnessHistory] = {
    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output.outputPK && c.beamName === beamName)
    } yield {
      (output, symmetryAndFlatness)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => OutputSymFlat(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime + "  " + os.sf.beamName)

    associateBaseline(tsList)
  }

  /**
    * Get the SymmetryAndFlatness history for all beams on the given machine.
    *
    * @param machinePK : For this machine
    * @return Complete history with baselines.
    *
    */
  def history(machinePK: Long, procedurePK: Long): Seq[SymmetryAndFlatnessHistory] = {

    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, symmetryAndFlatness)
    }

    // Fetch entire history from the database.  Also sort by beam and dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val tsList = Db.run(search.result).map(os => OutputSymFlat(os._1, os._2)).sortBy(os => os.sf.beamName + ":" + os.output.dataDate.get.getTime)

    tsList.groupBy(_.sf.beamName).flatMap(tsGroup => associateBaseline(tsGroup._2)).toSeq
  }

  /**
    * Get the baseline by finding another set of values that
    *   - were captured before the given time stamp
    *   - belong to the same machine
    *   - were produced by the same beam
    *   - are defined as a baseline because <code>isBaseline</code> is true, or failing that, have the chronologically earliest preceding <code>SymmetryAndFlatness</code>.
    *
    * @param machinePK Match this machine
    * @param beamName  Match this beam
    * @param dataDate  Most recent that is at or before this time
    * @return The baseline value to use, or None if not found.
    */
  def getBaseline(machinePK: Long, beamName: String, dataDate: Timestamp, procedurePK: Long): Option[SymmetryAndFlatnessHistory] = {
    //noinspection ReverseFind
    val reverseHistory = history(machinePK, beamName, procedurePK).reverse
    val baseline = reverseHistory.find(h => h.output.dataDate.get.getTime <= dataDate.getTime)
    if (baseline.isDefined)
      baseline
    else
      reverseHistory.lastOption
  }

  /**
    * Get the list of symmetry and flatness entries that were explicitly marked to be used as baselines that
    * reference the given output.  The outputPK must match the passed outputPK and the
    * <code>isBaseline</code> must be "true".
    *
    * @param outputPK SymmetryAndFlatness rows must point to this output.
    * @return
    */
  def getBaselineByOutput(outputPK: Long): Seq[SymmetryAndFlatness] = {
    val search = for { symFlat <- SymmetryAndFlatness.query.filter(sf => (sf.outputPK === outputPK) && sf.isBaseline) } yield symFlat
    val list = Db.run(search.result)
    list
  }

}
