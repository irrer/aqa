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
import org.aqa.db.SymmetryAndFlatness.TopBottomLeftRightCenter
import org.aqa.webrun.psm.PSMCache

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
                                span_mm: Option[Double], // distance in mm between opposing measurement areas (both left-right and top-bottom).
                                diameter_mm: Option[Double], // diameter in mm of the area sampled.
                                RTImageSID_mm: Option[Double], // distance in mm from source to image (DICOM metadata 3002,0026)
                                KVP: Option[Double], // Peak kilo voltage output of the X-Ray generator used (DICOM metadata 0018,0060)
                                isFlatteningFilterFree: Option[Boolean] // True if this is an FFF beam. (see DICOM metadata 3002,0051 FluenceMode and 3002,0052 FluenceModeID in RTPLAN)
                              ) extends Logging {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(SymmetryAndFlatness.query.insertOrUpdate(this))

  val isBaselineFunc: Boolean = isBaseline

  /** Coefficients of Variation. */
  val topCOV: Double = topStdDev_cu / top_cu
  val bottomCOV: Double = bottomStdDev_cu / bottom_cu
  val leftCOV: Double = leftStdDev_cu / left_cu
  val rightCOV: Double = rightStdDev_cu / right_cu
  val centerCOV: Double = centerStdDev_cu / center_cu

  /**
   * Get the PSM grid and flood field for this instance.
   *
   * @return PSM grid and flood field if available, otherwise None.
   */
  def psmGridWithFFFrom(): Option[PSMCache.PSMGridWithFF] = {
    if ( //
      KVP.isDefined &&
        isFlatteningFilterFree.isDefined &&
        span_mm.isDefined &&
        diameter_mm.isDefined
    ) {
      PSMCache.get( //
        outputPK = outputPK,
        KVP = KVP.get,
        isFFF = isFlatteningFilterFree.get,
        span_mm = span_mm.get,
        diameter_mm = diameter_mm.get
      )
    } else
      None
  }

  /**
   * Get the flood field for this record.
   *
   * @return flood field if available, None if not.
   */
  private def floodFieldOf(): Option[FloodField] = psmGridWithFFFrom().map(_.floodField)

  // - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Determine if this instance can use PSM, meaning that compatible PSM and flood field data is available..
   *
   * @return True if it can do PSM.
   */
  def canDoPSM: Boolean = floodFieldOf().isDefined

  // - - - - - - - - - - - - - - - - - - - - - - - -

  def topWithPSM_cu(): Option[Double] = {
    psmGridWithFFFrom() match {
      case Some(gridWithFFF) =>
        Some((top_cu * gridWithFFF.psmGrid.topBeam.getFloodField_cu(gridWithFFF)) / gridWithFFF.normalizedTop)
      case _ =>
        None
    }
  }

  def bottomWithPSM_cu(): Option[Double] = {
    psmGridWithFFFrom() match {
      case Some(gridWithFFF) =>
        Some((bottom_cu * gridWithFFF.psmGrid.bottomBeam.getFloodField_cu(gridWithFFF)) / gridWithFFF.normalizedBottom)
      case _ =>
        None
    }
  }

  def leftWithPSM_cu(): Option[Double] = {
    psmGridWithFFFrom() match {
      case Some(gridWithFFF) =>
        Some((left_cu * gridWithFFF.psmGrid.leftBeam.getFloodField_cu(gridWithFFF)) / gridWithFFF.normalizedLeft)
      case _ =>
        None
    }
  }

  def rightWithPSM_cu(): Option[Double] = {
    psmGridWithFFFrom() match {
      case Some(gridWithFFF) =>
        Some((right_cu * gridWithFFF.psmGrid.rightBeam.getFloodField_cu(gridWithFFF)) / gridWithFFF.normalizedRight)
      case _ =>
        None
    }
  }

  def centerWithPSM_cu(): Option[Double] = {
    psmGridWithFFFrom() match {
      case Some(gridWithFFF) =>
        Some((center_cu * gridWithFFF.psmGrid.centerBeam.getFloodField_cu(gridWithFFF)) / gridWithFFF.normalizedCenter)
      case _ =>
        None
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Perform the math of calculating axial symmetry.  This function isolates and highlights the actual calculation performed.
   *
   * @param topValue_cu    Top value in CU.
   * @param bottomValue_cu Bottom value in CU.
   * @return axial symmetry.
   */
  private def axialSymmetryCalculation(topValue_cu: Double, bottomValue_cu: Double): Double = {
    if (bottomValue_cu == 0) {
      logger.warn("Unexpected error.  Invalid value of 0 given for bottom cu.")
      -1
    } else {
      ((topValue_cu - bottomValue_cu) / bottomValue_cu) * 100 // the math
    }
  }

  /**
   * Perform the math of calculating transvers symmetry.  This function isolates and highlights the actual calculation performed.
   *
   * @param rightValue_cu Top value in CU.
   * @param leftValue_cu  Bottom value in CU.
   * @return axial symmetry.
   */
  private def transverseSymmetryCalculation(rightValue_cu: Double, leftValue_cu: Double): Double = {
    if (leftValue_cu == 0) {
      logger.warn("Unexpected error.  Invalid value of 0 given for left cu.")
      -1
    } else {
      ((rightValue_cu - leftValue_cu) / leftValue_cu) * 100 // the math
    }
  }

  /**
   * Perform the math of calculating flatness.  This function isolates and highlights the actual calculation performed.
   *
   * @param list List of all 5 values in cu.
   * @return axial symmetry.
   */
  private def flatnessCalculation(list: Seq[Double]): Double = {
    val min_cu = list.min
    val max_cu = list.max
    val v = ((max_cu - min_cu) / (max_cu + min_cu)) * 100
    v
  }

  private def makeTBLRCNoPSM: SymmetryAndFlatness.TopBottomLeftRightCenter =
    SymmetryAndFlatness.TopBottomLeftRightCenter(top = top_cu, bottom = bottom_cu, left = left_cu, right = right_cu, center = center_cu)

  private def makeTBLRCWithPSM(): SymmetryAndFlatness.TopBottomLeftRightCenter = SymmetryAndFlatness.TopBottomLeftRightCenter( //
    top = topWithPSM_cu().get,
    bottom = bottomWithPSM_cu().get,
    left = leftWithPSM_cu().get,
    right = rightWithPSM_cu().get,
    center = centerWithPSM_cu().get)

  /**
   * Perform the math of calculating constancy.  This function isolates and highlights the actual calculation performed.
   *
   * @param data     5 points sampled for this beam.
   * @param baseline 5 points from baseline beam.
   * @return Constancy.
   */
  private def constancyCalculation(data: TopBottomLeftRightCenter, baseline: TopBottomLeftRightCenter): Double = {
    // @formatter:off
    val t = (data.top    / data.center) - (baseline.top    / baseline.center)

    val b = (data.bottom / data.center) - (baseline.bottom / baseline.center)

    val l = (data.left   / data.center) - (baseline.left   / baseline.center)

    val r = (data.right  / data.center) - (baseline.right  / baseline.center)
    // @formatter:on

    val v = ((t + r + b + l) * 100) / 4
    v
  }


  /**
   * Calculate axial symmetry using PSM.  If not possible, return None.
   *
   * @return Axial symmetry
   */
  def axialSymmetryWithPSM(): Option[Double] = {
    if (canDoPSM) {
      val v = axialSymmetryCalculation(topWithPSM_cu().get, bottomWithPSM_cu().get)
      Some(v)
    } else
      None
  }

  /**
   * Calculate transverse symmetry using PSM.  If not possible, return None.
   *
   * @return Transverse symmetry
   */
  def transverseSymmetryWithPSM(): Option[Double] = {
    if (canDoPSM) {
      val v = transverseSymmetryCalculation(leftWithPSM_cu().get, rightWithPSM_cu().get)
      Some(v)
    } else
      None
  }


  /**
   * Calculate flatness using PSM.  If not possible, return None.
   *
   * @return Flatness
   */
  def flatnessWithPSM(): Option[Double] = {
    if (canDoPSM) {
      val list = Seq(
        topWithPSM_cu(),
        bottomWithPSM_cu(),
        leftWithPSM_cu(),
        rightWithPSM_cu(),
        centerWithPSM_cu(),
      ).map(_.get)

      val v = flatnessCalculation(list)
      Some(v)
    } else
      None
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


  private def axialSymmetryWithPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      axialSymmetryWithPSM(),
      baseline.axialSymmetryWithPSM(),
      Config.SymmetryPercentLimit)

  private def transverseSymmetryWithPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      transverseSymmetryWithPSM(),
      baseline.transverseSymmetryWithPSM(),
      Config.SymmetryPercentLimit)

  private def flatnessWithPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      flatnessWithPSM(),
      baseline.flatnessWithPSM(),
      Config.FlatnessPercentLimit)

  private def profileConstancyWithPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      profileConstancyWithPSM(baseline),
      baseline.profileConstancyWithPSM(baseline),
      Config.ProfileConstancyPercentLimit
    )

  def allWithPSMPass(baseline: SymmetryAndFlatness): Boolean = { //
    axialSymmetryWithPSMPass(baseline) &&
      transverseSymmetryWithPSMPass(baseline) &&
      flatnessWithPSMPass(baseline) &&
      profileConstancyWithPSMPass(baseline)
  }




  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


  /**
   * Calculate axial symmetry with no PSM.
   *
   * @return Axial symmetry
   */
  def axialSymmetryNoPSM(): Double = {
    val v = axialSymmetryCalculation(topValue_cu = top_cu, bottomValue_cu = bottom_cu)
    v
  }


  /**
   * Calculate axial symmetry with no PSM.
   *
   * @return Axial symmetry
   */
  def transverseSymmetryNoPSM(): Double = {
    val v = transverseSymmetryCalculation(rightValue_cu = right_cu, leftValue_cu = left_cu)
    v
  }


  // private val listNoPSM = Seq(top_cu, bottom_cu, right_cu, left_cu, center_cu)


  /**
   * Get the flatness.
   *
   * @return Flatness, or None on error.
   */
  def flatnessNoPSM(): Double = {
    val list = Seq( //
      top_cu,
      bottom_cu,
      left_cu,
      right_cu,
      center_cu,
    )
    flatnessCalculation(list)
  }

  def profileConstancyWithPSM(baseline: SymmetryAndFlatness): Option[Double] = {
    if (canDoPSM && baseline.canDoPSM) {
      val value = makeTBLRCWithPSM()
      val baselineValue = baseline.makeTBLRCWithPSM()
      val v = Some(constancyCalculation(value, baselineValue))
      v
    }
    else
      None
  }


  def profileConstancyNoPSM(baseline: SymmetryAndFlatness): Double = {
    constancyCalculation(makeTBLRCNoPSM, baseline.makeTBLRCNoPSM)
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


  def axialSymmetryNoPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      Some(axialSymmetryNoPSM()),
      Some(baseline.axialSymmetryNoPSM()),
      Config.SymmetryPercentLimit)

  def transverseSymmetryNoPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      Some(transverseSymmetryNoPSM()),
      Some(baseline.transverseSymmetryNoPSM()),
      Config.SymmetryPercentLimit)

  def flatnessNoPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      Some(flatnessNoPSM()),
      Some(baseline.flatnessNoPSM()),
      Config.FlatnessPercentLimit)

  def profileConstancyNoPSMPass(baseline: SymmetryAndFlatness): Boolean =
    doesPass( //
      Some(profileConstancyNoPSM(baseline)),
      Some(baseline.profileConstancyNoPSM(baseline)),
      Config.ProfileConstancyPercentLimit
    )

  def allNoPSMPass(baseline: SymmetryAndFlatness): Boolean = { //
    axialSymmetryNoPSMPass(baseline) &&
      transverseSymmetryNoPSMPass(baseline) &&
      flatnessNoPSMPass(baseline) &&
      profileConstancyNoPSMPass(baseline)
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // paramaterized get functions

  def getTopCu(withPSM: Boolean): Double = {
    if (withPSM)
      topWithPSM_cu().get
    else
      top_cu
  }


  def getBottomCu(withPSM: Boolean): Double = {
    if (withPSM)
      bottomWithPSM_cu().get
    else
      bottom_cu
  }

  def getLeftCu(withPSM: Boolean): Double = {
    if (withPSM)
      leftWithPSM_cu().get
    else
      left_cu
  }


  def getRightCu(withPSM: Boolean): Double = {
    if (withPSM)
      rightWithPSM_cu().get
    else
      right_cu
  }


  def getCenterCu(withPSM: Boolean): Double = {
    if (withPSM)
      centerWithPSM_cu().get
    else
      center_cu
  }


  def getAxialSymmetry(withPSM: Boolean): Double = {
    if (withPSM)
      axialSymmetryWithPSM().get
    else
      axialSymmetryNoPSM()
  }


  def getTransverseSymmetry(withPSM: Boolean): Double = {
    if (withPSM)
      transverseSymmetryWithPSM().get
    else
      transverseSymmetryNoPSM()
  }


  def getFlatness(withPSM: Boolean): Double = {
    if (withPSM)
      flatnessWithPSM().get
    else
      flatnessNoPSM()
  }


  def getConstancy(withPSM: Boolean, baseline: SymmetryAndFlatness): Double = {
    if (withPSM)
      profileConstancyWithPSM(baseline).get
    else
      profileConstancyNoPSM(baseline)
  }

  /**
   * True if the sampling points are spatially compatible with PSM analysis.
   *
   * @return True if defined and close enough.
   */
  def isPSMSpatiallyCompatible(): Boolean = {
    val spanOk = span_mm.isDefined && ((Config.PSMSpan_mm - span_mm.get).abs < SymmetryAndFlatness.SpanTolerance_mm)
    val diameterOk = diameter_mm.isDefined && (((Config.PSMRadius_mm * 2) - diameter_mm.get).abs < SymmetryAndFlatness.DiameterTolerance_mm)

    spanOk && diameterOk
  }


  /**
   * Get the span (distance between centers of opposing sample areas) in mm.
   *
   * @return Span.
   */
  def getSpan_mm: Double = {
    if (span_mm.isDefined) span_mm.get else Config.SymmetrySpan_mm
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
      "    span_mm: " + span_mm + "\n" +
      "    diameter_mm: " + diameter_mm + "\n" +
      "    RTImageSID_mm: " + RTImageSID_mm + "\n"
  }

}

object SymmetryAndFlatness extends Logging {


  private case class TopBottomLeftRightCenter( //
                                               top: Double,
                                               bottom: Double,
                                               left: Double,
                                               right: Double,
                                               center: Double,
                                             ) {}

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

    def span_mm = column[Option[Double]]("span_mm")

    def diameter_mm = column[Option[Double]]("diameter_mm")

    def RTImageSID_mm = column[Option[Double]]("RTImageSID_mm")

    def KVP = column[Option[Double]]("KVP")

    def isFlatteningFilterFree = column[Option[Boolean]]("isFlatteningFilterFree")

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
        span_mm,
        diameter_mm,
        RTImageSID_mm,
        KVP,
        isFlatteningFilterFree
      ) <> (SymmetryAndFlatness.apply _ tupled, SymmetryAndFlatness.unapply)

    def outputFK = foreignKey("SymmetryAndFlatness_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  /** When comparing two spans, they must be at least this close together to considered equal. */
  val SpanTolerance_mm: Double = 2.0

  /** When comparing two diameters, they must be at least this close together to considered equal. */
  val DiameterTolerance_mm: Double = 0.5

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

    /*
    val psmGrid: Option[PSMGrid] = {
      if (symmetryAndFlatness.XsmDataDate.isDefined)
        PSMGrid.get(output.machinePK.get, symmetryAndFlatness.XsmDataDate.get)
      else
        None
    }
    */


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
   * @param machinePK     : For this machine
   * @param beamName      : For this beam
   * @param span_mm       Distance between opposing sample points.
   * @param diameter_mm   Diameter of each sample point.
   * @param RTImageSID_mm : Matching this RTImageSID_mm (source to image distance)
   * @param procedurePK   : For this procedure (will be either Phase2 or Phase3)
   * @return Complete history with baselines sorted by date.
   *
   */
  def history( //
               machinePK: Long,
               beamName: String,
               span_mm: Long,
               diameter_mm: Option[Double],
               RTImageSID_mm: Option[Double],
               procedurePK: Long,
             ): Seq[SymmetryAndFlatnessHistory] = {

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

      val spanOk: Boolean = {
        sf.getSpan_mm.round == span_mm
      }
      val diameterOk: Boolean = {
        (sf.diameter_mm.isDefined, diameter_mm.isDefined) match {
          case (true, true) => sf.diameter_mm.get == diameter_mm.get
          case (true, false) => sf.diameter_mm.get == defaultDiameter_mm
          case (false, true) => diameter_mm.get == defaultDiameter_mm
          case (false, false) => true
        }
      }

      def RTImageSIDApproximatelyEqual(a: Double, b: Double) = {
        val eq = (a - b).abs < Config.SymmetryAndFlatnessRTImageSIDProximity_mm
        eq
      }

      val RTImageSIDOk: Boolean = {
        (sf.RTImageSID_mm.isDefined, RTImageSID_mm.isDefined) match {
          case (true, true) => RTImageSIDApproximatelyEqual(sf.RTImageSID_mm.get, RTImageSID_mm.get)
          case (true, false) => RTImageSIDApproximatelyEqual(sf.RTImageSID_mm.get, defaultRTImageSID_mm)
          case (false, true) => RTImageSIDApproximatelyEqual(RTImageSID_mm.get, defaultRTImageSID_mm)
          case (false, false) => true
        }
      }

      spanOk && diameterOk && RTImageSIDOk
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = {
      val list = Db.run(sr)
      val filteredList = list.filter(os => sfOk(os._2))
      filteredList.map(os => OutputSymFlat(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime + "  " + os.sf.beamName + "  " + os.sf.canDoPSM.toString)
    }

    associateBaseline(tsList)
  }

  /**
   * Get the SymmetryAndFlatness history for all beams on the given machine.
   *
   * @param machinePK   For this machine
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
   * @param machinePK     Match this machine
   * @param beamName      Match this beam
   * @param hasPSM        Matching this
   * @param span_mm       Distance between opposing sample points.
   * @param diameter_mm   Diameter of each sample point.
   * @param RTImageSID_mm Source to image distance in mm.
   * @param dataDate      Most recent that is at or before this time
   * @param procedurePK   For this procedure
   * @return The baseline value to use, or None if not found.
   */
  def getBaseline( //
                   machinePK: Long,
                   span_mm: Long,
                   diameter_mm: Option[Double],
                   RTImageSID_mm: Option[Double],
                   beamName: String,
                   dataDate: Timestamp,
                   procedurePK: Long
                 ): Option[SymmetryAndFlatnessHistory] = {
    //noinspection ReverseFind
    val reverseHistory = history( //
      machinePK,
      beamName,
      span_mm,
      diameter_mm,
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
    val search = for {symFlat <- SymmetryAndFlatness.query.filter(sf => (sf.outputPK === outputPK) && sf.isBaseline)} yield symFlat
    val list = Db.run(search.result)
    list
  }

}
