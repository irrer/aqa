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
import org.aqa.db.Db.driver.api._
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.rnd

import java.sql.Timestamp
import java.util.Date
import scala.xml.Elem

/**
 * Store the analysis results for a single gap skew image.
 *
 * Data is associated with a "bank", which indicates a pair of X1 and X2 MLCs or Jaws, or
 * a pair of Y1 and Y2 MLCs or Jaws.  If only one edge is measured, then only one set of
 * values will be valid (non-null).
 *
 * @param winstonLutz2PK      primary key
 * @param outputPK            output primary key
 * @param rtimageUID          SOP series instance UID of EPID image
 * @param beamName            Name of beam in RTPLAN (if available)
 * @param gantryAngle_deg     Angle of gantry in degrees.  This is the raw value from the RTIMAGE and is not rounded.
 * @param collimatorAngle_deg Angle of collimator in degrees.  This is the raw value from the RTIMAGE and is not rounded.
 * @param tableAngle_deg      Angle of table (couch) in degrees.  This is the raw value PatientSupportAngle from the RTIMAGE and is not rounded.
 * @param X1x_mm              X coordinate of point on X1 that is closest to the origin.  Not defined for some cardinal angles.
 * @param X1y_mm              Y coordinate of point on X1 that is closest to the origin.  Not defined for some cardinal angles.
 * @param X2x_mm              X coordinate of point on X2 that is closest to the origin.  Not defined for some cardinal angles.
 * @param X2y_mm              Y coordinate of point on X2 that is closest to the origin.  Not defined for some cardinal angles.
 * @param Y1x_mm              X coordinate of point on Y1 that is closest to the origin.  Not defined for some cardinal angles.
 * @param Y1y_mm              Y coordinate of point on Y1 that is closest to the origin.  Not defined for some cardinal angles.
 * @param Y2x_mm              X coordinate of point on Y2 that is closest to the origin.  Not defined for some cardinal angles.
 * @param Y2y_mm              Y coordinate of point on Y2 that is closest to the origin.  Not defined for some cardinal angles.
 * @param plannedOffsetX1_mm  planned collimator or jaw leaf offset from center in mm of the X1 edge
 * @param plannedOffsetX2_mm  planned collimator or jaw leaf offset from center in mm of the X1 edge
 * @param plannedOffsetY1_mm  planned collimator or jaw leaf offset from center in mm of the X1 edge
 * @param plannedOffsetY2_mm  planned collimator or jaw leaf offset from center in mm of the X1 edge
 * @param ballX_mm            X coordinate of center of ball in mm
 * @param ballY_mm            Y coordinate of center of ball in mm
 *
 */

case class WinstonLutzNonCardinal(
                         // @formatter:off
    winstonLutz2PK       : Option[Long]       ,
    outputPK             : Long               ,
    rtimageUID           : String             ,
    beamName             : Option[String]     ,
    gantryAngle_deg      : Double             ,
    collimatorAngle_deg  : Double             ,
    tableAngle_deg       : Double             ,
    //
    X1x_mm               : Option[Double]     ,
    X1y_mm               : Option[Double]     ,
    X2x_mm               : Option[Double]     ,
    X2y_mm               : Option[Double]     ,
    Y1x_mm               : Option[Double]     ,
    Y1y_mm               : Option[Double]     ,
    Y2x_mm               : Option[Double]     ,
    Y2y_mm               : Option[Double]     ,
    //
    plannedOffsetX1_mm   : Option[Double]     ,
    plannedOffsetX2_mm   : Option[Double]     ,
    plannedOffsetY1_mm   : Option[Double]     ,
    plannedOffsetY2_mm   : Option[Double]     ,
    //
    ballX_mm             : Double             ,
    ballY_mm             : Double               // Y coordinate of center of ball in mm

  // @formatter:on
                       ) {

  def insert: WinstonLutzNonCardinal = {
    val insertQuery = WinstonLutzNonCardinal.query returning WinstonLutzNonCardinal.query.map(_.winstonLutz2PK) into ((winstonLutz2, winstonLutz2PK) => winstonLutz2.copy(winstonLutz2PK = Some(winstonLutz2PK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val gantryAngleRounded: Int = Util.angleRoundedTo90(gantryAngle_deg)
  val collimatorAngleRounded: Int = Util.angleRoundedTo90(collimatorAngle_deg)
  val tableAngleRounded: Int = WLXlsxUtil.angleRounded(tableAngle_deg)

  /**
   * Construct beam name based on the gantry, collimator, and table angles.
   *
   * @return text name.
   */
  private def isoBeamName: String = {
    val table: String =
      if (tableAngleRounded != 0)
        " T" + Util.angleRoundedTo90(tableAngleRounded)
      else
        ""
    val name = "WL G" + Util.angleRoundedTo90(gantryAngle_deg) + " C" + Util.angleRoundedTo90(collimatorAngle_deg) + table
    name
  }


  /**
   * Get a beam name.  Use the one in the RTPLAN, but if that is not available, construct one based on the gantry, collimator, and table angles.
   *
   * @return The name of the beam.
   */
  def beamNameOf: String = {
    if (beamName.isDefined)
      beamName.get.replaceFirst("^[0-9] ", "").trim
    else
      isoBeamName
  }

  private val collimatorAngle_radians: Double = Math.toRadians(collimatorAngle_deg)

  /** Sine of collimator angle.  Calculated once here so it does not need to be calculated multiple times. */
  val collimatorSin: Double = Math.sin(collimatorAngle_radians)

  /** Cosine of collimator angle.  Calculated once here so it does not need to be calculated multiple times. */
  val collimatorCos: Double = Math.cos(collimatorAngle_radians)


  def insertOrUpdate(): Int = Db.run(WinstonLutzNonCardinal.query.insertOrUpdate(this))

  //noinspection ScalaWeakerAccess
  def boxCenterX_mm: Double = {
    val list = Seq(X1x_mm, X2x_mm, Y1x_mm, Y2x_mm).flatten
    list.sum / list.size
  }

  //noinspection ScalaWeakerAccess
  def boxCenterY_mm: Double = {
    val list = Seq(X1y_mm, X2y_mm, Y1y_mm, Y2y_mm).flatten
    list.sum / list.size
  }

  //noinspection ScalaWeakerAccess
  val errorX_mm: Double = boxCenterX_mm - ballX_mm

  //noinspection ScalaWeakerAccess
  val errorY_mm: Double = boxCenterY_mm - ballY_mm

  val errorXY_mm: Double = Math.sqrt((errorX_mm * errorX_mm) + (errorY_mm * errorY_mm))

  /** Analysis F */
  val caX: Option[Double] = {
    val value = gantryAngleRounded match {
      case 0 => Some(errorX_mm)
      case 180 => Some(-errorX_mm)
      case _ => None
    }
    value.map(rnd)
  }

  /** Analysis G */
  val caY: Option[Double] = {
    val value = gantryAngleRounded match {
      case 90 => Some(errorX_mm)
      case 270 => Some(-errorX_mm)
      case _ => None
    }
    value.map(rnd)
  }

  /** Analysis H */
  val caZ: Option[Double] = Some(-errorY_mm).map(rnd)

  private val tableAngle_radians: Double = Math.toRadians(tableAngle_deg)
  val yawSin: Double = Math.sin(tableAngle_radians)
  val yawCos: Double = Math.cos(tableAngle_radians)

  override def toString: String = {
    // @formatter:off
      s"""    winstonLutz2PK       : $winstonLutz2PK\n"""                                +
      s"""    outputPK             : $outputPK\n"""                                      +
      s"""    rtimageUID           : $rtimageUID\n"""                                    +
      s"""    beamName             : $beamName\n"""                                      +
      s"""    gantryAngle_deg      : $gantryAngle_deg\n"""                               +
      s"""    collimatorAngle_deg  : $collimatorAngle_deg\n"""                           +
      s"""    tableAngle_deg       : $tableAngle_deg\n"""                                +
      s"""    gantryAngle_deg      : ${Util.angleRoundedTo90(gantryAngle_deg)}\n"""      +
      s"""    collimatorAngle_deg  : ${Util.angleRoundedTo90(collimatorAngle_deg)}\n"""  +
      s"""    ballX_mm             : $ballX_mm\n"""                                      +
      s"""    ballY_mm             : $ballY_mm\n"""                                      +
      s"""    boxCenterX_mm        : $boxCenterX_mm\n"""                                 +
      s"""    boxCenterY_mm        : $boxCenterY_mm\n"""                                 +
      s"""    errorX_mm            : $errorX_mm\n"""                                     +
      s"""    errorY_mm            : $errorY_mm\n"""                                     +
      s"""    errorXY_mm           : $errorXY_mm\n"""
    // @formatter:on
  }
}

//noinspection ScalaWeakerAccess
object WinstonLutzNonCardinal extends Logging {

  // @formatter:off
  class WinstonLutz2Table(tag: Tag) extends Table[WinstonLutzNonCardinal](tag, "winstonLutz2") {
    def winstonLutz2PK        = column[Long]("winstonLutz2PK", O.PrimaryKey, O.AutoInc)
    def outputPK              = column[Long]("outputPK")
    def rtimageUID            = column[String]("rtimageUID")
    def beamName              = column[Option[String]]("beamName")
    def gantryAngle_deg       = column[Double]("gantryAngle_deg")
    def collimatorAngle_deg   = column[Double]("collimatorAngle_deg")
    def tableAngle_deg        = column[Double]("tableAngle_deg")
    def X1x_mm                = column[Option[Double]]("X1x_mm")
    def X1y_mm                = column[Option[Double]]("X1y_mm")
    def X2x_mm                = column[Option[Double]]("X2x_mm")
    def X2y_mm                = column[Option[Double]]("X2y_mm")
    def Y1x_mm                = column[Option[Double]]("Y1x_mm")
    def Y1y_mm                = column[Option[Double]]("Y1y_mm")
    def Y2x_mm                = column[Option[Double]]("Y2x_mm")
    def Y2y_mm                = column[Option[Double]]("Y2y_mm")
    def plannedOffsetX1_mm    = column[Option[Double]]("plannedOffsetX1_mm")
    def plannedOffsetX2_mm    = column[Option[Double]]("plannedOffsetX2_mm")
    def plannedOffsetY1_mm    = column[Option[Double]]("plannedOffsetY1_mm")
    def plannedOffsetY2_mm    = column[Option[Double]]("plannedOffsetY2_mm")
    def ballX_mm              = column[Double]("ballX_mm")
    def ballY_mm              = column[Double]("ballY_mm")

    def * =
      (
        winstonLutz2PK.?, //
        outputPK              ,
        rtimageUID            ,
        beamName              ,
        gantryAngle_deg       ,
        collimatorAngle_deg   ,
        tableAngle_deg        ,
        X1x_mm                ,
        X1y_mm                ,
        X2x_mm                ,
        X2y_mm                ,
        Y1x_mm                ,
        Y1y_mm                ,
        Y2x_mm                ,
        Y2y_mm                ,
        plannedOffsetX1_mm    ,
        plannedOffsetX2_mm    ,
        plannedOffsetY1_mm    ,
        plannedOffsetY2_mm    ,
        ballX_mm              ,
        ballY_mm              ,
      ) <> (WinstonLutzNonCardinal.apply _ tupled, WinstonLutzNonCardinal.unapply)
    // @formatter:on

    def outputFK = foreignKey("WinstonLutz2_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[WinstonLutz2Table]

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

  def get(winstonLutz2PK: Long): Option[WinstonLutzNonCardinal] = {
    val action = for {
      inst <- WinstonLutzNonCardinal.query if inst.winstonLutz2PK === winstonLutz2PK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all WinstonLutz2 for the given output
   */
  def getByOutput(outputPK: Long): Seq[WinstonLutzNonCardinal] = {
    val action = for {
      inst <- WinstonLutzNonCardinal.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list.toIndexedSeq
  }

  def delete(winstonLutz2PK: Long): Int = {
    val q = query.filter(_.winstonLutz2PK === winstonLutz2PK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[WinstonLutzNonCardinal] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[WinstonLutzNonCardinal]()
  }

  def insertSeq(list: Seq[WinstonLutzNonCardinal]): Unit = {
    list.foreach(_.insertOrUpdate())
  }

  case class WinstonLutz2History(output: Output, winstonLutz2: WinstonLutzNonCardinal) extends HasOutput {
    override def getOutput: Output = output
  }


  /**
   * Get a given number of history of WinstonLutz2 results that are on or earlier than the given date.
   *
   * @param date          : At or earlier than this date
   * @param count         : Return up to this many results
   * @param institutionPK : Restrict to this institution.
   * @return Chunk of history sorted by date.
   *
   */
  def historyByDate(date: Date, count: Int, institutionPK: Long): Seq[WinstonLutz2History] = {

    val timeStamp = new Timestamp(date.getTime)

    val search = for {
      machine <- Machine.query.filter(m => m.institutionPK === institutionPK)
      output <- Output.valid.filter(o => (o.dataDate <= timeStamp) && (o.machinePK === machine.machinePK))
      winstonLutz2 <- WinstonLutzNonCardinal.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, winstonLutz2)
    }

    val sortedSearch = search.sortBy(_._1.dataDate.desc).take(count)

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = sortedSearch.result
    val tsList = Db.run(sr).map(os => WinstonLutz2History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }


  /**
   * Get the history of WinstonLutz2 results.
   *
   * @param machinePK : For this machine
   * @param beamName  : For this beam
   * @return Complete history with baselines sorted by date.
   *
   */
  def historyByBeam(machinePK: Long, beamName: String): Seq[WinstonLutz2History] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      winstonLutz2 <- WinstonLutzNonCardinal.query.filter(c => c.outputPK === output.outputPK && c.beamName === beamName)
    } yield {
      (output, winstonLutz2)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinstonLutz2History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }

  /**
   * Get the history of WinstonLutz2 results.
   *
   * @param machinePK : For this machine
   * @return Complete history sorted by date.
   *
   */
  def historyByMachine(machinePK: Long): Seq[WinstonLutz2History] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      winstonLutz2 <- WinstonLutzNonCardinal.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, winstonLutz2)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinstonLutz2History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }

  /**
   * Get all WinstonLutz2 results.
   *
   * @param outputSet List of output PKs.  Each WinstonLutz2 must point to one of the items in this set.
   * @return List of WinstonLutz2 that point to the output set.
   *
   */
  def listByOutputSet(outputSet: Set[Long]): Seq[WinstonLutzNonCardinal] = {

    val search = for {
      winstonLutz2 <- WinstonLutzNonCardinal.query.filter(c => c.outputPK.inSet(outputSet))
    } yield {
      winstonLutz2
    }

    val tsList = Db.run(search.result)

    tsList.toIndexedSeq
  }

}
