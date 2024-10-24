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

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import scala.xml.Elem

/**
 * Store the analysis results for a single gap skew image.
 *
 * Data is associated with a "bank", which indicates a pair of X1 and X2 MLCs or Jaws, or
 * a pair of Y1 and Y2 MLCs or Jaws.  If only one edge is measured, then only one set of
 * values will be valid (non-null).
 */
case class GapSkew(
                    // @formatter:off
    gapSkewPK: Option[Long],                  // primary key
    outputPK: Long,                           // output primary key
    rtimageUID: String,                       // SOP series instance UID of EPID image
    beamName: String,                         // name of beam
    collimatorAngle_deg: Double,              // Angle of collimator in degrees.  This is the raw value from the RTIMAGE and is not rounded.
    //
    measurementWidth_mm: Double,              // width of measurement rectangle.  Not required to calculate values, but provides an indication of the number of pixels used to establish the edge measurements.
    measurementSeparation_mm: Double,         // distance between measurements of the same edge.  Needed to calculate skew angle.
    //
    topLeftEdgeTypeName: Option[String],      // name of top jaw type and bank, Must be one of: "X1_Jaw", "X1_MLC", "Y1_Jaw", "Y1_MLC".  If None/null, then this bank has no values.  See <code>GapSkew.EdgeType</code>
    topLeftValue_mm: Option[Double],          // isoplane coordinate of end of leaf or jaw position at higher numbered leaves.
    topLeftPlanned_mm: Option[Double],        // planned isoplane position (expected value of bank1Low_mm and bank1High_mm)
    //
    topRightEdgeTypeName: Option[String],     // name of top jaw type and bank, Must be one of: "X1_Jaw", "X1_MLC", "Y1_Jaw", "Y1_MLC".  If None/null, then this bank has no values.  See <code>GapSkew.EdgeType</code>
    topRightValue_mm: Option[Double],         // isoplane coordinate of end of leaf or jaw position at higher numbered leaves.
    topRightPlanned_mm: Option[Double],       // planned isoplane position (expected value of bank1Low_mm and bank1High_mm)
    //
    bottomLeftEdgeTypeName: Option[String],   // name of top jaw type and bank, Must be one of: "X1_Jaw", "X1_MLC", "Y1_Jaw", "Y1_MLC".  If None/null, then this bank has no values.  See <code>GapSkew.EdgeType</code>
    bottomLeftValue_mm: Option[Double],       // isoplane coordinate of end of leaf or jaw position at higher numbered leaves.
    bottomLeftPlanned_mm: Option[Double],     // planned isoplane position (expected value of bank1Low_mm and bank1High_mm)
    //
    bottomRightEdgeTypeName: Option[String],  // name of top jaw type and bank, Must be one of: "X1_Jaw", "X1_MLC", "Y1_Jaw", "Y1_MLC".  If None/null, then this bank has no values.  See <code>GapSkew.EdgeType</code>
    bottomRightValue_mm: Option[Double],      // isoplane coordinate of end of leaf or jaw position at higher numbered leaves.
    bottomRightPlanned_mm: Option[Double],    // planned isoplane position (expected value of bank1Low_mm and bank1High_mm)
    // @formatter:on
                  ) {

  def insert: GapSkew = {
    val insertQuery = GapSkew.query returning GapSkew.query.map(_.gapSkewPK) into ((gapSkew, gapSkewPK) => gapSkew.copy(gapSkewPK = Some(gapSkewPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(GapSkew.query.insertOrUpdate(this))

  private val edgeTypeNameList = Seq(topLeftEdgeTypeName, topRightEdgeTypeName, bottomLeftEdgeTypeName, bottomRightEdgeTypeName).flatten

  /** True if there are two edges defined, false if just one. */
  def hasTwoEdges: Boolean = edgeTypeNameList.size == 4

  /** True if at least one of the edges was defined by an MLC. */
  private def hasMlc: Boolean = {
    val edgeTypeList = edgeTypeNameList.map(GapSkew.EdgeType.toEdgeType)
    edgeTypeList.exists(et => !et.isJaw)
  }

  /** Collimator angle rounded to nearest 90 degrees. */
  def angleRounded: Int = Util.angleRoundedTo90(collimatorAngle_deg)

  /** True if one of the edges is the MLC and the collimator is at 270 degrees. */
  def colIs270: Boolean = hasMlc && (Util.angleRoundedTo90(collimatorAngle_deg) == 270)

  /*
   * ---------------------------------------------------------------------------------------------------------
   *
   * Below are convenience functions that encapsulate abstractions of the data and
   * make it more easily consumable.  They are defined as functions instead of values
   * because they may or may not be valid at run time.  If not valid, they will throw
   * an exception.
   *
   * The nomenclature is to use 'Horz' in the function names to indicate that they
   * are processing on the assumption that the edges being measured are horizontal.
   * If the need to support vertical edges arises, then the function names should
   * include 'Vert'.
   *
   * Note also that functions have been written to support a pair of horizontal edges.
   * The database fields support:
   *    - single edge horizontal
   *    - single edge vertical
   *    - double edge vertical
   * but the functions have not been written because the use case has not been presented.
   *
   * ---------------------------------------------------------------------------------------------------------
   */

  def topLeftEdgeType: GapSkew.EdgeType = GapSkew.EdgeType.toEdgeType(topLeftEdgeTypeName.get)

  private def topRightEdgeType: GapSkew.EdgeType = GapSkew.EdgeType.toEdgeType(topRightEdgeTypeName.get)

  def bottomLeftEdgeType: GapSkew.EdgeType = GapSkew.EdgeType.toEdgeType(bottomLeftEdgeTypeName.get)

  def bottomRightEdgeType: GapSkew.EdgeType = GapSkew.EdgeType.toEdgeType(bottomRightEdgeTypeName.get)

  def edgeList: Seq[GapSkew.EdgeType] = Seq(topLeftEdgeType, topRightEdgeType, bottomLeftEdgeType, bottomRightEdgeType)

  private def math(a: OD, b: OD, func: (Double, Double) => Double): Option[Double] = {
    (a, b) match {
      case (Some(aa), Some(bb)) => Some(func(aa, bb))
      case _ => None
    }
  }

  private def minus(a: OD, b: OD): Option[Double] = math(a, b, (aa, bb) => aa - bb)

  private def minusAbs(a: OD, b: OD): Option[Double] = math(a, b, (aa, bb) => (aa - bb).abs)

  private def negate(a: OD): Option[Double] = {
    a match {
      case Some(aa) => Some(-aa)
      case _ => None
    }
  }

  /** Planned separation of edges.  Only valid if there are two edges. */
  def plannedEdgeSeparation_mm: Option[Double] = {
    if (topLeftEdgeType.isHorz)
      minusAbs(topLeftPlanned_mm, bottomRightPlanned_mm)
    else
      minusAbs(topLeftPlanned_mm, topRightPlanned_mm)
  }

  private type OD = Option[Double]

  /** Rise or fall of top edge (if defined, else exception). */
  def topHorzDelta_mm: Option[Double] = minus(topRightValue_mm, topLeftValue_mm)

  /** Rise or fall of bottom edge (if defined, else exception). */
  def bottomHorzDelta_mm: Option[Double] = minus(bottomRightValue_mm, bottomLeftValue_mm)

  /** Vertical separation between upper and lower left horizontal edges. (if defined, else exception). */
  def leftSeparationOfHorzEdges_mm: Option[Double] = minus(topLeftValue_mm, bottomLeftValue_mm)

  /** Vertical separation between upper and lower right horizontal edges. (if defined, else exception). */
  def rightSeparationOfHorzEdges_mm: Option[Double] = math(bottomRightValue_mm, topRightValue_mm, (a, b) => (a - b).abs)

  /** Change (error) in vertical separation between upper and lower left horizontal edges. (planned - measured) (if defined, else exception). */
  def leftDeltaSeparationOfHorzEdges_mm: Option[Double] = minus(plannedEdgeSeparation_mm, leftSeparationOfHorzEdges_mm)

  /** Change (error) in vertical separation between upper and lower right horizontal edges. (planned - measured) (if defined, else exception). */
  def rightDeltaSeparationOfHorzEdges_mm: Option[Double] = minus(plannedEdgeSeparation_mm, rightSeparationOfHorzEdges_mm)

  /** Skew (angle) of top edge (if defined, else exception). */
  def topHorzSkew_deg: Option[Double] = math(topHorzDelta_mm, Some(measurementSeparation_mm), (a, b) => Math.toDegrees(Math.atan(a / b)))

  /** Skew as mm per 40 cm (if defined, else exception). */
  def topHorzSkew_mmPer40cm: Option[Double] = math(topHorzDelta_mm, Some(measurementSeparation_mm), (a, b) => (a / b) * 400)

  /** Skew (angle) of bottom edge (if defined, else exception). */
  def bottomHorzSkew_deg: Option[Double] = math(bottomHorzDelta_mm, Some(measurementSeparation_mm), (a, b) => Math.toDegrees(Math.atan(a / b)))

  /** Skew as mm per 40 cm (if defined, else exception). */
  def bottomHorzSkew_mmPer40cm: Option[Double] = math(bottomHorzDelta_mm, Some(measurementSeparation_mm), (a, b) => (a / b) * 400)

  /** Difference (error) in top left horizontal edge measurement from plan (planned - measured). */
  def topLeftHorzDelta_mm: Option[Double] = minus(topLeftPlanned_mm, topLeftValue_mm)

  /** Difference (error) in top right horizontal edge measurement from plan (planned - measured). */
  def topRightHorzDelta_mm: Option[Double] = minus(topRightPlanned_mm, topRightValue_mm)

  /** Difference (error) in bottom left horizontal edge measurement from plan (planned - measured). */
  def bottomLeftHorzDelta_mm: Option[Double] = minus(bottomLeftPlanned_mm, bottomLeftValue_mm)

  /** Difference (error) in bottom right horizontal edge measurement from plan (planned - measured). */
  def bottomRightHorzDelta_mm: Option[Double] = minus(bottomRightPlanned_mm, bottomRightValue_mm)

  def collimatorMinusJawDiffSkew_deg: Option[Double] = {
    val diff = minus(topHorzSkew_deg, bottomHorzSkew_deg)
    if (bottomRightEdgeType.isJaw)
      diff
    else {
      negate(diff)
    }
  }

  def collimatorMinusJawDiffSkew_mmPer40cm: Option[Double] = {
    val diff = minus(topHorzSkew_mmPer40cm, bottomHorzSkew_mmPer40cm)
    if (bottomRightEdgeType.isJaw)
      diff
    else {
      negate(diff)
    }
  }

  def collimatorMinusJawDiffDelta_mm: Option[Double] = {
    val diff = minus(topHorzDelta_mm, bottomHorzDelta_mm)
    if (bottomRightEdgeType.isJaw)
      diff
    else
      negate(diff)
  }

  /**
   * Controls what order the beams are shown to the user.
   *
   * @return Number that can be used to sort them.
   */
  def viewOrdering: Int = {

    val topIs0 = topLeftPlanned_mm.isDefined && (topLeftPlanned_mm.get.round == 0)
    val bottomIs0 = bottomLeftPlanned_mm.isDefined && (bottomLeftPlanned_mm.get.round == 0)
    val topIsMlc = topIs0 && topLeftEdgeTypeName.isDefined && topLeftEdgeType.isMlc
    val bottomIsMlc = bottomIs0 && bottomLeftEdgeTypeName.isDefined && bottomLeftEdgeType.isMlc

    val ordering = 0 match {
      case _ if (angleRounded == 90) && topIsMlc => 1 // ABank C90
      case _ if (angleRounded == 90) && bottomIsMlc => 2 // BBank C90
      case _ if (angleRounded == 270) && bottomIsMlc => 3 // ABank C270
      case _ if (angleRounded == 270) && topIsMlc => 4 // BBank C270
      case _ if (angleRounded == 270) && (edgeList.count(_.isJaw) == 4) => 5 // 270 Jaw-Jaw
      case _ => 6 // Don't know what this is.  Put it last.
    }
    ordering
  }
}

object GapSkew extends ProcedureOutput with Logging {

  class GapSkewTable(tag: Tag) extends Table[GapSkew](tag, "gapSkew") {

    def gapSkewPK = column[Long]("gapSkewPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def rtimageUID = column[String]("rtimageUID")

    def beamName = column[String]("beamName")

    def collimatorAngle_deg = column[Double]("collimatorAngle_deg")

    //
    def measurementWidth_mm = column[Double]("measurementWidth_mm")

    def measurementSeparation_mm = column[Double]("measurementSeparation_mm")

    //
    def topLeftEdgeTypeName = column[Option[String]]("topLeftEdgeTypeName")

    def topLeftValue_mm = column[Option[Double]]("topLeftValue_mm")

    def topLeftPlanned_mm = column[Option[Double]]("topLeftPlanned_mm")

    //
    def topRightEdgeTypeName = column[Option[String]]("topRightEdgeTypeName")

    def topRightValue_mm = column[Option[Double]]("topRightValue_mm")

    def topRightPlanned_mm = column[Option[Double]]("topRightPlanned_mm")

    //
    def bottomLeftEdgeTypeName = column[Option[String]]("bottomLeftEdgeTypeName")

    def bottomLeftValue_mm = column[Option[Double]]("bottomLeftValue_mm")

    def bottomLeftPlanned_mm = column[Option[Double]]("bottomLeftPlanned_mm")

    //
    def bottomRightEdgeTypeName = column[Option[String]]("bottomRightEdgeTypeName")

    def bottomRightValue_mm = column[Option[Double]]("bottomRightValue_mm")

    def bottomRightPlanned_mm = column[Option[Double]]("bottomRightPlanned_mm")

    def * =
      (
        gapSkewPK.?,
        outputPK,
        rtimageUID,
        beamName,
        collimatorAngle_deg,
        measurementWidth_mm,
        measurementSeparation_mm,
        topLeftEdgeTypeName,
        topLeftValue_mm,
        topLeftPlanned_mm,
        topRightEdgeTypeName,
        topRightValue_mm,
        topRightPlanned_mm,
        bottomLeftEdgeTypeName,
        bottomLeftValue_mm,
        bottomLeftPlanned_mm,
        bottomRightEdgeTypeName,
        bottomRightValue_mm,
        bottomRightPlanned_mm
      ) <> (GapSkew.apply _ tupled, GapSkew.unapply)

    def outputFK = foreignKey("GapSkew_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[GapSkewTable]

  /*
   * The following diagram shows the jaw and collimator names when the collimator angle is 90.
   *
   *  ..........................................
   *  .                                        .
   *  .                                        .
   *  .              Jaw Positions             .
   *  .                                        .
   *  .                                        .
   *  .                   X2                   .
   *  .          --------------------          .
   *  .          |                  |          .
   *  .          |                  |          .
   *  .          |                  |          .
   *  .       Y1 |                  | Y2       .
   *  .          |                  |          .
   *  .          |                  |          .
   *  .          |                  |          .
   *  .          --------------------          .
   *  .                   X1                   .
   *  .                                        .
   *  .                                        .
   *  .                                        .
   *  ..........................................
   *
   */

  case class EdgeType(isX: Boolean, bank: Int, isJaw: Boolean, isHorz: Boolean) {
    val name: String = {
      (if (isX) "X" else "Y") +
        bank + " " +
        (if (isJaw) "Jaw" else "MLC") + " " +
        (if (isHorz) "Horz" else "Vert")
    }

    val isMlc: Boolean = !isJaw

    override def toString: String = name + "    isX: " + isX + "    is1: " + bank + "    isJaw: " + isJaw
  }

  /**
   * All possible types of edges.
   */
  //noinspection ScalaWeakerAccess
  object EdgeType {
    // @formatter:off
    val X1_Jaw_Horz: EdgeType = EdgeType( isX =  true, bank = 1, isJaw =  true, isHorz =  true)
    val X1_Jaw_Vert: EdgeType = EdgeType( isX =  true, bank = 1, isJaw =  true, isHorz = false)
    val X1_MLC_Horz: EdgeType = EdgeType( isX =  true, bank = 1, isJaw = false, isHorz =  true)
    val X1_MLC_Vert: EdgeType = EdgeType( isX =  true, bank = 1, isJaw = false, isHorz = false)
    val X2_Jaw_Horz: EdgeType = EdgeType( isX =  true, bank = 2, isJaw =  true, isHorz =  true)
    val X2_Jaw_Vert: EdgeType = EdgeType( isX =  true, bank = 2, isJaw =  true, isHorz = false)
    val X2_MLC_Horz: EdgeType = EdgeType( isX =  true, bank = 2, isJaw = false, isHorz =  true)
    val X2_MLC_Vert: EdgeType = EdgeType( isX =  true, bank = 2, isJaw = false, isHorz = false)

    val Y1_Jaw_Horz: EdgeType = EdgeType( isX = false, bank = 1, isJaw =  true, isHorz =  true)
    val Y1_Jaw_Vert: EdgeType = EdgeType( isX = false, bank = 1, isJaw =  true, isHorz = false)
    val Y1_MLC_Horz: EdgeType = EdgeType( isX = false, bank = 1, isJaw = false, isHorz =  true) // not supported by Varian
    val Y1_MLC_Vert: EdgeType = EdgeType( isX = false, bank = 1, isJaw = false, isHorz = false) // not supported by Varian
    val Y2_Jaw_Horz: EdgeType = EdgeType( isX = false, bank = 2, isJaw =  true, isHorz =  true)
    val Y2_Jaw_Vert: EdgeType = EdgeType( isX = false, bank = 2, isJaw =  true, isHorz = false)
    val Y2_MLC_Horz: EdgeType = EdgeType( isX = false, bank = 2, isJaw = false, isHorz =  true) // not supported by Varian
    val Y2_MLC_Vert: EdgeType = EdgeType( isX = false, bank = 2, isJaw = false, isHorz = false) // not supported by Varian

    val list: Seq[EdgeType] = List(
      X1_Jaw_Horz, X1_Jaw_Vert, X1_MLC_Horz, X1_MLC_Vert, X2_Jaw_Horz, X2_Jaw_Vert, X2_MLC_Horz, X2_MLC_Vert,
      Y1_Jaw_Horz, Y1_Jaw_Vert, Y1_MLC_Horz, Y1_MLC_Vert, Y2_Jaw_Horz, Y2_Jaw_Vert, Y2_MLC_Horz, Y2_MLC_Vert
    )
    // @formatter:on

    /**
     * Find the EdgeType with the given characteristics.
     *
     * @param isX   True if X, false if Y.
     * @param bank  Bank number.  Can only be 1 or 2, as in X1 or Y2.
     * @param isJaw True if is jaw, false if MLC.
     * @return The corresponding EdgeType.
     */
    def find(isX: Boolean, bank: Int, isJaw: Boolean, isHorz: Boolean): EdgeType = {
      val t = EdgeType(isX, bank, isJaw, isHorz)
      list.find(et => et.name.equals(t.name)).get
    }

    /**
     * Convert text to an EdgeType.  EdgeTypes are stored as text in the database.
     *
     * @param text Text version of EdgeType.
     * @return The corresponding EdgeType.
     */
    def toEdgeType(text: String): EdgeType = list.find(_.name.equals(text)).get
  }

  /*
  val edgeTypeJaw = "Jaw" // edge was formed by jaw
  val edgeTypeMlc = "MLC" // edge was formed by MLC leaf ends
  val edgeTypeNone = "None" // indicates that there was no measurement
   */

  override val topXmlLabel = "GapSkew"

  def get(gapSkewPK: Long): Option[GapSkew] = {
    val action = for {
      inst <- GapSkew.query if inst.gapSkewPK === gapSkewPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all GapSkew for the given output
   */
  def getByOutput(outputPK: Long): Seq[GapSkew] = {
    val action = for {
      inst <- GapSkew.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(gapSkewPK: Long): Int = {
    val q = query.filter(_.gapSkewPK === gapSkewPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[GapSkew] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[GapSkew]()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[GapSkew]): Unit = {
    val ops = list.map { loc => GapSkew.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class GapSkewHistory(output: Output, gapSkew: GapSkew) {}

  /**
   * Get the history of GapSkew results.
   *
   * @param machinePK : For this machine
   * @param beamName  : For this beam
   * @return Complete history with baselines sorted by date.
   *
   */
  def historyByBeam(machinePK: Long, beamName: String): Seq[GapSkewHistory] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      gapSkew <- GapSkew.query.filter(c => c.outputPK === output.outputPK && c.beamName === beamName)
    } yield {
      (output, gapSkew)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => GapSkewHistory(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList
  }

  /**
   * Get the history of GapSkew results.
   *
   * @param machinePK : For this machine
   * @return Complete history sorted by date.
   *
   */
  def historyByMachine(machinePK: Long): Seq[GapSkewHistory] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      gapSkew <- GapSkew.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, gapSkew)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => GapSkewHistory(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList
  }

  /**
   * Get all GapSkew results.
   *
   * @param outputSet : List of output PKs.  Each GapSkew must point to one of the items in this set.
   * @return List of GapSkew that point to the output set.
   *
   */
  def listByOutputSet(outputSet: Set[Long]): Seq[GapSkew] = {

    val search = for {
      gapSkew <- GapSkew.query.filter(c => c.outputPK.inSet(outputSet))
    } yield {
      gapSkew
    }

    val tsList = Db.run(search.result)

    tsList
  }

}
