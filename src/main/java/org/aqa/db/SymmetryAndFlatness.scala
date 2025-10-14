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
import org.aqa.webrun.psm.PSMGrid

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
    centerStdDev_cu: Double, // standard deviation of center point pixels in CU
    psmDataDate: Option[Timestamp], // if defined, references the PSM by its dataDate
    span_mm: Option[Double], // distance in mm between opposing measurement areas (both left-right and top-bottom).
    diameter_mm: Option[Double], // diameter in mm of the area sampled.
    RTImageSID_mm: Option[Double] // distance in mm from source to image (DICOM metadata 3002,0026)
) extends Logging {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val isBaselineFunc: Boolean = isBaseline

  /** Coefficients of Variation. */
  val topCOV: Double = topStdDev_cu / top_cu
  val bottomCOV: Double = bottomStdDev_cu / bottom_cu
  val leftCOV: Double = leftStdDev_cu / left_cu
  val rightCOV: Double = rightStdDev_cu / right_cu
  val centerCOV: Double = centerStdDev_cu / center_cu

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def rawImageQaTop(psmGrid: PSMGrid): Double = {
    psmGrid.topBeam.rawImage * top_cu
  }

  def beamResponseQaTop(psmGrid: PSMGrid): Double = {
    rawImageQaTop(psmGrid) / psmGrid.topBeam.psm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def rawImageQaBottom(psmGrid: PSMGrid): Double = {
    psmGrid.bottomBeam.rawImage * bottom_cu
  }

  def beamResponseQaBottom(psmGrid: PSMGrid): Double = {
    rawImageQaBottom(psmGrid) / psmGrid.bottomBeam.psm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def rawImageQaLeft(psmGrid: PSMGrid): Double = {
    psmGrid.leftBeam.rawImage * left_cu
  }

  def beamResponseQaLeft(psmGrid: PSMGrid): Double = {
    rawImageQaLeft(psmGrid) / psmGrid.leftBeam.psm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def rawImageQaRight(psmGrid: PSMGrid): Double = {
    psmGrid.rightBeam.rawImage * right_cu
  }

  def beamResponseQaRight(psmGrid: PSMGrid): Double = {
    rawImageQaRight(psmGrid) / psmGrid.rightBeam.psm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def rawImageQaCenter(psmGrid: PSMGrid): Double = {
    psmGrid.centerBeam.rawImage * center_cu
  }

  def beamResponseQaCenter(psmGrid: PSMGrid): Double = {
    rawImageQaCenter(psmGrid) / psmGrid.centerBeam.psm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  private def checkPsmMode(psmGrid: Option[PSMGrid]): Boolean = {
    val ok = (psmGrid.isEmpty && psmDataDate.isEmpty) || (psmGrid.isDefined && psmDataDate.isDefined)

    if (!ok)
      logger.warn(
        s"psmGrid parameter and psmDataDate must either both be defined or neither defined." +
          s"  grid.isDefined: ${psmGrid.isDefined}   psmDataDate.isDefined  ${psmDataDate.isDefined}"
      )
    ok
  }

  def axialSymmetry(psmGrid: Option[PSMGrid]): Option[Double] = {
    if (checkPsmMode(psmGrid)) {
      if (psmGrid.isEmpty) {
        val sym = ((top_cu - bottom_cu) / bottom_cu) * 100
        Some(sym)
      } else {
        val t = beamResponseQaTop(psmGrid.get)
        val b = beamResponseQaBottom(psmGrid.get)
        val sym = ((t - b) / b) * 100
        Some(sym)
      }
    } else
      None
  }

  def transverseSymmetry(psmGrid: Option[PSMGrid]): Option[Double] = {
    if (checkPsmMode(psmGrid)) {
      if (psmGrid.isEmpty) {
        val ts = ((right_cu - left_cu) / left_cu) * 100
        Some(ts)
      } else {
        val r = beamResponseQaRight(psmGrid.get)
        val l = beamResponseQaLeft(psmGrid.get)
        val ts = ((r - l) / l) * 100
        Some(ts)
      }
    } else {
      None
    }
  }

  private val list = Seq(top_cu, bottom_cu, right_cu, left_cu, center_cu)

  private def psmList(psmGrid: Option[PSMGrid]): Seq[Double] = {
    Seq( //
      beamResponseQaTop(psmGrid.get),
      beamResponseQaBottom(psmGrid.get),
      beamResponseQaLeft(psmGrid.get),
      beamResponseQaRight(psmGrid.get),
      beamResponseQaCenter(psmGrid.get)
    )
  }

  /**
    * Get the minimum value of the 5 points.
    * @param psmGrid Used if this is a PSM data set.
    * @return Minimum value, or None on error.
    */
  private def min(psmGrid: Option[PSMGrid]): Double = {
    checkPsmMode(psmGrid)
    if (psmGrid.isEmpty)
      list.min
    else
      psmList(psmGrid).min
  }

  /**
    * Get the maximum value of the 5 points.
    * @param psmGrid Used if this is a PSM data set.
    * @return Maximum value, or None on error.
    */
  private def max(psmGrid: Option[PSMGrid]): Double = {
    checkPsmMode(psmGrid)
    if (psmGrid.isEmpty)
      list.max
    else
      psmList(psmGrid).max
  }

  /**
    * Get the flatness.
    * @param psmGrid Used if this is a PSM data set.
    * @return Flatness, or None on error.
    */
  def flatness(psmGrid: Option[PSMGrid]): Option[Double] = {
    if (checkPsmMode(psmGrid)) {
      val f = ((max(psmGrid) - min(psmGrid)) / (max(psmGrid) + min(psmGrid))) * 100
      Some(f)
    } else
      None
  }

  def profileConstancy(psmGrid: Option[PSMGrid], baseline: SymmetryAndFlatness, baselinePsmGrid: Option[PSMGrid]): Option[Double] = {

    val psmOk = checkPsmMode(psmGrid)
    val psmBaselineOk = baseline.checkPsmMode(baselinePsmGrid)

    // check to make sure that either both or neither of the data sets use PSM.
    val bothMatch = (psmDataDate.isDefined && baseline.psmDataDate.isDefined) || (psmDataDate.isEmpty && baseline.psmDataDate.isEmpty)

    val allOk = psmOk && psmBaselineOk && bothMatch

    if (!allOk) {
      logger.warn(
        s"SymFlat PSM mismatch. " +
          s" psmGrid.isDefined:${psmGrid.isDefined}    psmDataDate.isDefined:${psmDataDate.isDefined}" +
          s"     psmGrid.isDefined:${baselinePsmGrid.isDefined}    psmDataDate.isDefined:${baseline.psmDataDate.isDefined}"
      )
      None // there is something wrong with the data.
    } else {
      // if this is the baseline, then the answer is zero.
      if (symmetryAndFlatnessPK.nonEmpty && (baseline.symmetryAndFlatnessPK.get == symmetryAndFlatnessPK.get)) {
        Some(0.0)
      } else {

        if (psmGrid.isEmpty) {

          val t = (top_cu / center_cu) - (baseline.top_cu / baseline.center_cu)
          val b = (bottom_cu / center_cu) - (baseline.bottom_cu / baseline.center_cu)
          val l = (left_cu / center_cu) - (baseline.left_cu / baseline.center_cu)
          val r = (right_cu / center_cu) - (baseline.right_cu / baseline.center_cu)

          val profConst = ((t + b + l + r) * 100) / 4

          Some(profConst)

        } else
          //
          {
            // process for PSM

            val tCu = beamResponseQaTop(psmGrid.get)
            val bCu = beamResponseQaBottom(psmGrid.get)
            val lCu = beamResponseQaLeft(psmGrid.get)
            val rCu = beamResponseQaRight(psmGrid.get)
            val cCu = beamResponseQaCenter(psmGrid.get)

            val tCuBase = baseline.beamResponseQaTop(baselinePsmGrid.get)
            val bCuBase = baseline.beamResponseQaBottom(baselinePsmGrid.get)
            val lCuBase = baseline.beamResponseQaLeft(baselinePsmGrid.get)
            val rCuBase = baseline.beamResponseQaRight(baselinePsmGrid.get)
            val cCuBase = baseline.beamResponseQaCenter(baselinePsmGrid.get)

            val t = (tCu / cCu) - (tCuBase / cCuBase)
            val b = (bCu / cCu) - (bCuBase / cCuBase)
            val l = (lCu / cCu) - (lCuBase / cCuBase)
            val r = (rCu / cCu) - (rCuBase / cCuBase)

            val profConst = ((t + b + l + r) * 100) / 4

            Some(profConst)
          }
      }
    }
  }

  /**
    * True if the comparison of the value to the baseline passes.  Otherwise, it has failed.
    *
    * @param value         Value being checked.
    * @param baselineValue Known good baseline used as a reference.
    * @return True on pass, false on fail.
    */
  private def doesPass(value: Option[Double], baselineValue: Option[Double], limit: Double): Boolean = {
    if (value.isEmpty || baselineValue.isEmpty)
      false
    else {
      val diff = (value.get - baselineValue.get).abs
      val pass = limit >= diff
      pass
    }
  }

  def axialSymmetryPass(baseline: SymmetryAndFlatness, psmGrid: Option[PSMGrid], baselinePsmGrid: Option[PSMGrid]): Boolean =
    doesPass(axialSymmetry(psmGrid), baseline.axialSymmetry(baselinePsmGrid), Config.SymmetryPercentLimit)

  def transverseSymmetryPass(baseline: SymmetryAndFlatness, psmGrid: Option[PSMGrid], baselinePsmGrid: Option[PSMGrid]): Boolean =
    doesPass(transverseSymmetry(psmGrid), baseline.transverseSymmetry(baselinePsmGrid), Config.SymmetryPercentLimit)

  def flatnessPass(baseline: SymmetryAndFlatness, psmGrid: Option[PSMGrid], baselinePsmGrid: Option[PSMGrid]): Boolean =
    doesPass(flatness(psmGrid), baseline.flatness(baselinePsmGrid), Config.FlatnessPercentLimit)

  def profileConstancyPass(baseline: SymmetryAndFlatness, psmGrid: Option[PSMGrid], baselinePsmGrid: Option[PSMGrid]): Boolean =
    doesPass(
      profileConstancy(psmGrid, baseline, baselinePsmGrid),
      baseline.profileConstancy(baselinePsmGrid, baseline, baselinePsmGrid),
      Config.ProfileConstancyPercentLimit
    )

  def allPass(baseline: SymmetryAndFlatness, psmGrid: Option[PSMGrid], baselinePsmGrid: Option[PSMGrid]): Boolean = {
    axialSymmetryPass(baseline, psmGrid, baselinePsmGrid) &&
    transverseSymmetryPass(baseline, psmGrid, baselinePsmGrid) &&
    flatnessPass(baseline, psmGrid, baselinePsmGrid) &&
    profileConstancyPass(baseline, psmGrid, baselinePsmGrid)
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
      "    centerStdDev_cu: " + centerStdDev_cu + "\n" +
      "    hasPsm: " + psmDataDate.isDefined + "\n" +
      "    span_mm: " + span_mm + "\n" +
      "    diameter_mm: " + diameter_mm + "\n" +
      "    RTImageSID_mm: " + RTImageSID_mm + "\n"
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

    def psmDataDate = column[Option[Timestamp]]("psmDataDate")

    def span_mm = column[Option[Double]]("span_mm")

    def diameter_mm = column[Option[Double]]("diameter_mm")

    def RTImageSID_mm = column[Option[Double]]("RTImageSID_mm")

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
        centerStdDev_cu,
        psmDataDate,
        span_mm,
        diameter_mm,
        RTImageSID_mm
      ) <> (SymmetryAndFlatness.apply _ tupled, SymmetryAndFlatness.unapply)

    def outputFK = foreignKey("SymmetryAndFlatness_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[SymmetryAndFlatnessTable]

  private val defaultSpan_mm = Config.SymmetryPointRight.x_mm - Config.SymmetryPointLeft.x_mm

  private val defaultDiameter_mm = Config.SymmetryAndFlatnessDiameter_mm

  private val defaultRTImageSID_mm = 1500.0

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

    val psmGrid: Option[PSMGrid] = {
      if (symmetryAndFlatness.psmDataDate.isDefined)
        PSMGrid.get(output.machinePK.get, symmetryAndFlatness.psmDataDate.get)
      else
        None
    }

    val psmGridBaseline: Option[PSMGrid] = {
      if (baseline.psmDataDate.isDefined)
        PSMGrid.get(output.machinePK.get, baseline.psmDataDate.get)
      else
        None
    }

    val axialSymmetry: Option[Double] = symmetryAndFlatness.axialSymmetry(psmGrid)
    val transverseSymmetry: Option[Double] = symmetryAndFlatness.transverseSymmetry(psmGrid)
    val flatness: Option[Double] = symmetryAndFlatness.flatness(psmGrid)
    val profileConstancy: Option[Double] = symmetryAndFlatness.profileConstancy(psmGrid, baseline, psmGridBaseline)
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
    * @param span_mm Distance between opposing sample points.
    * @param diameter_mm Diameter of each sample point.
    * @param RTImageSID_mm  : Matching this RTImageSID_mm (source to image distance)
    * @param procedurePK  : For this procedure (will be either Phase2 or Phase3)
    * @return Complete history with baselines sorted by date.
    *
    */
  def history( //
      machinePK: Long,
      beamName: String,
      span_mm: Option[Double],
      diameter_mm: Option[Double],
      hasPsm: Boolean,
      RTImageSID_mm: Option[Double],
      procedurePK: Long
  ): Seq[SymmetryAndFlatnessHistory] = {
    val notHasPsm = !hasPsm
    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c =>
        (c.outputPK === output.outputPK) &&
          (c.beamName === beamName)
      )
    } yield {
      (output, symmetryAndFlatness)
    }

    def sfOk(sf: SymmetryAndFlatness): Boolean = {

      val psmSame: Boolean = {
        (sf.psmDataDate.isDefined && hasPsm) ||
        (sf.psmDataDate.isEmpty && notHasPsm)
      }

      val spanOk: Boolean = {
        (sf.span_mm.isDefined, span_mm.isDefined) match {
          case (true, true)   => sf.span_mm.get == span_mm.get
          case (true, false)  => sf.span_mm.get == defaultSpan_mm
          case (false, true)  => span_mm.get == defaultSpan_mm
          case (false, false) => true
        }
      }
      val diameterOk: Boolean = {
        (sf.diameter_mm.isDefined, diameter_mm.isDefined) match {
          case (true, true)   => sf.diameter_mm.get == diameter_mm.get
          case (true, false)  => sf.diameter_mm.get == defaultDiameter_mm
          case (false, true)  => diameter_mm.get == defaultDiameter_mm
          case (false, false) => true
        }
      }

      def RTImageSIDApproximatelyEqual(a: Double, b: Double) = {
        val eq = (a - b).abs < Config.SymmetryAndFlatnessRTImageSIDProximity_mm
        eq
      }

      val RTImageSIDOk: Boolean = {
        (sf.RTImageSID_mm.isDefined, RTImageSID_mm.isDefined) match {
          case (true, true)   => RTImageSIDApproximatelyEqual(sf.RTImageSID_mm.get, RTImageSID_mm.get)
          case (true, false)  => RTImageSIDApproximatelyEqual(sf.RTImageSID_mm.get, defaultRTImageSID_mm)
          case (false, true)  => RTImageSIDApproximatelyEqual(RTImageSID_mm.get, defaultRTImageSID_mm)
          case (false, false) => true
        }
      }

      psmSame && spanOk && diameterOk && RTImageSIDOk
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = {
      val list = Db.run(sr)
      val filteredList = list.filter(os => sfOk(os._2))
      filteredList.map(os => OutputSymFlat(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime + "  " + os.sf.beamName + "  " + os.sf.psmDataDate.isDefined.toString)
    }

    associateBaseline(tsList)
  }

  /**
    * Get the SymmetryAndFlatness history for all beams on the given machine.
    *
    * @param machinePK For this machine
    * @param procedurePK Procedure.  As the code is now, it will be either Phase2 or Phase3.
    * @return Complete history with baselines.
    */
  def historyForMachine(machinePK: Long, procedurePK: Long): Seq[SymmetryAndFlatnessHistory] = {

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
    * @param hasPsm  Matching this
    * @param span_mm Distance between opposing sample points.
    * @param diameter_mm Diameter of each sample point.
    * @param RTImageSID_mm Source to image distance in mm.
    * @param dataDate  Most recent that is at or before this time
    * @param procedurePK For this procedure
    * @return The baseline value to use, or None if not found.
    */
  def getBaseline( //
      machinePK: Long,
      span_mm: Option[Double],
      diameter_mm: Option[Double],
      RTImageSID_mm: Option[Double],
      beamName: String,
      hasPsm: Boolean,
      dataDate: Timestamp,
      procedurePK: Long
  ): Option[SymmetryAndFlatnessHistory] = {
    //noinspection ReverseFind
    val reverseHistory = history( //
      machinePK,
      beamName,
      span_mm,
      diameter_mm,
      hasPsm,
      RTImageSID_mm,
      procedurePK
    ).reverse
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
