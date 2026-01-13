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
import slick.collection.heterogeneous.HNil

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
 * @param winLutz360PK             primary key
 * @param outputPK                 output primary key
 * @param rtimageUID               SOP series instance UID of EPID image
 * @param beamName                 Name of beam in RTPLAN (if available)
 * @param gantryAngle_deg          Angle of gantry in degrees.  This is the raw value from the RTIMAGE and is not rounded.
 * @param collimatorAngle_deg      Angle of collimator in degrees.  This is the raw value from the RTIMAGE and is not rounded.
 * @param tableAngle_deg           Angle of table (couch) in degrees.  This is the raw value PatientSupportAngle from the RTIMAGE and is not rounded.
 * @param boxCenterX_mm            X coordinate of center of box (field) in mm in the isoplane
 * @param boxCenterY_mm            Y coordinate of center of box (field) in mm in the isoplane
 * @param ballCenterX_mm           X coordinate of center of ball (phantom) in mm in the isoplane
 * @param ballCenterY_mm           Y coordinate of center of ball (phantom) in mm in the isoplane
 * @param X1Offset_mm              Distance from the X1 edge to the center of the beam in mm in the isoplane.  If None, then the plan was not available and the center was not zero.
 * @param X2Offset_mm              Distance from the X2 edge to the center of the beam in mm in the isoplane.  If None, then the plan was not available and the center was not zero.
 * @param Y1Offset_mm              Distance from the Y1 edge to the center of the beam in mm in the isoplane.  If None, then the plan was not available and the center was not zero.
 * @param Y2Offset_mm              Distance from the Y2 edge to the center of the beam in mm in the isoplane.  If None, then the plan was not available and the center was not zero.
 * @param X1Type                   For X1, type of edge, Jaw, MLC, or JawAndMLC from origin.  The latter only occurs if both the jaw and edge are defined at the same position.  Only defined if the plan was available.
 * @param X2Type                   For X2, type of edge, Jaw, MLC, or JawAndMLC from origin.  The latter only occurs if both the jaw and edge are defined at the same position.  Only defined if the plan was available.
 * @param Y1Type                   For Y1, type of edge, Jaw, MLC, or JawAndMLC from origin.  The latter only occurs if both the jaw and edge are defined at the same position.  Only defined if the plan was available.
 * @param Y2Type                   For Y2, type of edge, Jaw, MLC, or JawAndMLC from origin.  The latter only occurs if both the jaw and edge are defined at the same position.  Only defined if the plan was available.
 * @param X1PlannedOffset_mm       For X1, planned distance of Jaw or MLC from origin.  Only defined if the plan was available at the time of analysis.
 * @param X2PlannedOffset_mm       For X1, planned distance of Jaw or MLC from origin.  Only defined if the plan was available at the time of analysis.
 * @param Y1PlannedOffset_mm       For X1, planned distance of Jaw or MLC from origin.  Only defined if the plan was available at the time of analysis.
 * @param Y2PlannedOffset_mm       For X2, planned distance of Jaw or MLC from origin.  Only defined if the plan was available at the time of analysis.
 */

case class WinLutz360(
                                   // @formatter:off
    winLutz360PK : Option[Long]   ,
    outputPK                 : Long           ,
    rtimageUID               : String         ,
    beamName                 : Option[String] ,
    gantryAngle_deg          : Double         ,
    collimatorAngle_deg      : Double         ,
    tableAngle_deg           : Option[Double] ,
    //
    boxCenterX_mm            : Double         ,
    boxCenterY_mm            : Double         ,
    //
    ballCenterX_mm           : Double         ,
    ballCenterY_mm           : Double         ,
    //
    X1Offset_mm              : Option[Double] ,
    X2Offset_mm              : Option[Double] ,
    Y1Offset_mm              : Option[Double] ,
    Y2Offset_mm              : Option[Double] ,
    //
    X1Type                   : Option[String] ,
    X2Type                   : Option[String] ,
    Y1Type                   : Option[String] ,
    Y2Type                   : Option[String] ,
    //
    X1PlannedOffset_mm       : Option[Double] ,
    X2PlannedOffset_mm       : Option[Double] ,
    Y1PlannedOffset_mm       : Option[Double] ,
    Y2PlannedOffset_mm       : Option[Double]
  // @formatter:on
                                 ) extends Logging with WinstonLutzGeneric {

  override val PK = winLutz360PK

  def insert: WinLutz360 = {
    val insertQuery = WinLutz360.query returning
      WinLutz360.query.map(_.winLutz360PK) into
      ((winstonLutz, winLutz360PK) => winstonLutz.copy(winLutz360PK = winLutz360PK))
    if (false) { // TODO put back
      val action = insertQuery += this
      val result = Db.run(action)
      result
    }
    this
  }

  val gantryAngleRounded: Int = Util.angleRoundedTo90(gantryAngle_deg)
  val collimatorAngleRounded: Int = Util.angleRoundedTo90(collimatorAngle_deg)
  val tableAngleRounded: Int = WLXlsxUtil.angleRounded(tableAngle_deg.get)

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


  def insertOrUpdate(): Int = Db.run(WinLutz360.query.insertOrUpdate(this))

  //noinspection ScalaWeakerAccess
  val errorX_mm: Double = boxCenterX_mm - ballCenterX_mm

  //noinspection ScalaWeakerAccess
  val errorY_mm: Double = boxCenterY_mm - ballCenterY_mm

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

  private val tableAngle_radians: Double = Math.toRadians(tableAngle_deg.get)
  val yawSin: Double = Math.sin(tableAngle_radians)
  val yawCos: Double = Math.cos(tableAngle_radians)

  override def toString: String = {
    // @formatter:off
      s"""    winLutz360PK       : $winLutz360PK\n"""                                +
      s"""    outputPK             : $outputPK\n"""                                      +
      s"""    rtimageUID           : $rtimageUID\n"""                                    +
      s"""    beamName             : $beamName\n"""                                      +
      s"""    gantryAngle_deg      : $gantryAngle_deg\n"""                               +
      s"""    collimatorAngle_deg  : $collimatorAngle_deg\n"""                           +
      s"""    tableAngle_deg       : $tableAngle_deg\n"""                                +
      s"""    gantryAngle_deg      : ${Util.angleRoundedTo90(gantryAngle_deg)}\n"""      +
      s"""    collimatorAngle_deg  : ${Util.angleRoundedTo90(collimatorAngle_deg)}\n"""  +
      s"""    ballX_mm             : $ballCenterX_mm\n"""                                +
      s"""    ballY_mm             : $ballCenterY_mm\n"""                                +
      s"""    boxX_mm              : $boxCenterX_mm\n"""                                 +
      s"""    boxY_mm              : $boxCenterY_mm\n"""                                 +
      s"""    errorX_mm            : $errorX_mm\n"""                                     +
      s"""    errorY_mm            : $errorY_mm\n"""                                     +
      s"""    errorXY_mm           : $errorXY_mm\n"""
    // @formatter:on
  }
}

//noinspection ScalaWeakerAccess
object WinLutz360 extends Logging {

  class WinLutz360Table(tag: Tag) extends Table[WinLutz360](tag, "winLutz360PK") {
    // @formatter:off
    def winLutz360PK = column[Option[Long]]   ("winLutz360PK", O.PrimaryKey, O.AutoInc)
    def outputPK                 = column[Long]           ("outputPK")
    def rtimageUID               = column[String]         ("rtimageUID")
    def beamName                 = column[Option[String]] ("beamName")
    def gantryAngle_deg          = column[Double]         ("gantryAngle_deg")
    def collimatorAngle_deg      = column[Double]         ("collimatorAngle_deg")
    def tableAngle_deg           = column[Option[Double]] ("tableAngle_deg")
    //
    def boxCenterX_mm            = column[Double]         ("boxCenterX_mm")
    def boxCenterY_mm            = column[Double]         ("boxCenterY_mm")
    //
    def ballCenterX_mm           = column[Double]         ("ballCenterX_mm")
    def ballCenterY_mm           = column[Double]         ("ballCenterY_mm")
    //
    def X1Offset_mm              = column[Option[Double]] ("X1Offset_mm")
    def X2Offset_mm              = column[Option[Double]] ("X2Offset_mm")
    def Y1Offset_mm              = column[Option[Double]] ("Y1Offset_mm")
    def Y2Offset_mm              = column[Option[Double]] ("Y2Offset_mm")
    //
    def X1Type                   = column[Option[String]] ("X1Type")
    def X2Type                   = column[Option[String]] ("X2Type")
    def Y1Type                   = column[Option[String]] ("Y1Type")
    def Y2Type                   = column[Option[String]] ("Y2Type")
    //
    def X1PlannedOffset_mm       = column[Option[Double]] ("X1PlannedOffset_mm")
    def X2PlannedOffset_mm       = column[Option[Double]] ("X2PlannedOffset_mm")
    def Y1PlannedOffset_mm       = column[Option[Double]] ("Y1PlannedOffset_mm")
    def Y2PlannedOffset_mm       = column[Option[Double]] ("Y2PlannedOffset_mm")
    // @formatter:on

    def * =
      (
        // @formatter:off
        winLutz360PK  :: //
        outputPK                  ::
        rtimageUID                ::
        beamName                  ::
        gantryAngle_deg           ::
        collimatorAngle_deg       ::
        tableAngle_deg            ::
        boxCenterX_mm             ::
        boxCenterY_mm             ::
        ballCenterX_mm            ::
        ballCenterY_mm            ::
        X1Offset_mm               ::
        X2Offset_mm               ::
        Y1Offset_mm               ::
        Y2Offset_mm               ::
        X1Type                    ::
        X2Type                    ::
        Y1Type                    ::
        Y2Type                    ::
        X1PlannedOffset_mm        ::
        X2PlannedOffset_mm        ::
        Y1PlannedOffset_mm        ::
        Y2PlannedOffset_mm        ::
        HNil
        // @formatter:on
        ).mapTo[WinLutz360]
    //<> (WinLutz360.apply _ tupled, WinLutz360.unapply)

    def outputFK = foreignKey("WinLutz360_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[WinLutz360Table]

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

  def get(winLutz360PK: Long): Option[WinLutz360] = {
    val action = for {
      inst <- WinLutz360.query if inst.winLutz360PK === winLutz360PK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all WinLutz360 for the given output
   */
  def getByOutput(outputPK: Long): Seq[WinLutz360] = {
    val action = for {
      inst <- WinLutz360.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list.toIndexedSeq
  }

  def delete(winLutz360PK: Long): Int = {
    val q = query.filter(_.winLutz360PK === winLutz360PK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[WinLutz360] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[WinLutz360]()
  }

  def insertSeq(list: Seq[WinLutz360]): Unit = {
    list.foreach(_.insertOrUpdate())
  }

  case class WinLutz360History(output: Output, winLutz360: WinLutz360) extends HasOutput {
    override def getOutput: Output = output
  }


  /**
   * Get a given number of history of WinLutz360 results that are on or earlier than the given date.
   *
   * @param date          : At or earlier than this date
   * @param count         : Return up to this many results
   * @param institutionPK : Restrict to this institution.
   * @return Chunk of history sorted by date.
   *
   */
  def historyByDate(date: Date, count: Int, institutionPK: Long): Seq[WinLutz360History] = {

    val timeStamp = new Timestamp(date.getTime)

    val search = for {
      machine <- Machine.query.filter(m => m.institutionPK === institutionPK)
      output <- Output.valid.filter(o => (o.dataDate <= timeStamp) && (o.machinePK === machine.machinePK))
      winLutz360 <- WinLutz360.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, winLutz360)
    }

    val sortedSearch = search.sortBy(_._1.dataDate.desc).take(count)

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = sortedSearch.result
    val tsList = Db.run(sr).map(os => WinLutz360History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }


  /**
   * Get the history of WinLutz360 results.
   *
   * @param machinePK : For this machine
   * @param beamName  : For this beam
   * @return Complete history with baselines sorted by date.
   *
   */
  def historyByBeam(machinePK: Long, beamName: String): Seq[WinLutz360History] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      winLutz360 <- WinLutz360.query.filter(c => c.outputPK === output.outputPK && c.beamName === beamName)
    } yield {
      (output, winLutz360)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinLutz360History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }

  /**
   * Get the history of WinLutz360 results.
   *
   * @param machinePK : For this machine
   * @return Complete history sorted by date.
   *
   */
  def historyByMachine(machinePK: Long): Seq[WinLutz360History] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      winLutz360 <- WinLutz360.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, winLutz360)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinLutz360History(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList.toIndexedSeq
  }

  /**
   * Get all WinLutz360 results.
   *
   * @param outputSet List of output PKs.  Each WinLutz360 must point to one of the items in this set.
   * @return List of WinLutz360 that point to the output set.
   *
   */
  def listByOutputSet(outputSet: Set[Long]): Seq[WinLutz360] = {

    val search = for {
      winLutz360 <- WinLutz360.query.filter(c => c.outputPK.inSet(outputSet))
    } yield {
      winLutz360
    }

    val tsList = Db.run(search.result)

    tsList.toIndexedSeq
  }

}
