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
import org.aqa.procedures.ProcedureOutput
import org.aqa.Util

import scala.xml.Elem

/**
  * Store the analysis results for a single gap skew image.
  *
  * Data is associated with a "bank", which indicates a pair of X1 and X2 MLCs or Jaws, or
  * a pair of Y1 and Y2 MLCs or Jaws.  If only one edge is measured, then only one set of
  * values will be valid (non-null).
  */
case class WinstonLutz(
    // @formatter:off
    winstonLutzPK: Option[Long],  // primary key
    outputPK: Long,               // output primary key
    rtimageUID: String,           // SOP series instance UID of EPID image
    rtplanUID: String,            // SOP series instance UID of referenced RTPLAN
    beamName: Option[String],     // Name of beam in RTPLAN (if available)
    gantryAngle_deg: Double,      // Angle of gantry in degrees.  This is the raw value from the RTIMAGE and is not rounded.
    collimatorAngle_deg: Double,  // Angle of collimator in degrees.  This is the raw value from the RTIMAGE and is not rounded.
    dataDate: java.sql.Timestamp,          // time and date that this image was captured at the treatment machine
    //
    topEdge_mm: Double,           // width of measurement rectangle.  Not required to calculate values, but provides an indication of the number of pixels used to establish the edge measurements.
    bottomEdge_mm: Double,        // distance between measurements of the same edge.  Needed to calculate skew angle.
    leftEdge_mm: Double,          // width of measurement rectangle.  Not required to calculate values, but provides an indication of the number of pixels used to establish the edge measurements.
    rightEdge_mm: Double,         // distance between measurements of the same edge.  Needed to calculate skew angle.
    ballX_mm: Double,             // width of measurement rectangle.  Not required to calculate values, but provides an indication of the number of pixels used to establish the edge measurements.
    ballY_mm: Double              // width of measurement rectangle.  Not required to calculate values, but provides an indication of the number of pixels used to establish the edge measurements.
    // @formatter:on
) {

  def insert: WinstonLutz = {
    val insertQuery = WinstonLutz.query returning WinstonLutz.query.map(_.winstonLutzPK) into ((winstonLutz, winstonLutzPK) => winstonLutz.copy(winstonLutzPK = Some(winstonLutzPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  /**
    * Get a beam name.  Use the one in the RTPLAN, but if that is not available, construct one based on the gantry and collimator angles.
    * @return The name of the beam.
    */
  def beamNameOf: String = {
    if (beamName.isDefined)
      beamName.get.replaceFirst("^[0-9] ", "").trim
    else {
      val name = "WL G" + Util.angleRoundedTo90(gantryAngle_deg) + " C" + Util.angleRoundedTo90(collimatorAngle_deg)
      name
    }
  }

  def insertOrUpdate(): Int = Db.run(WinstonLutz.query.insertOrUpdate(this))

  //noinspection ScalaWeakerAccess
  def boxCenterX: Double = (leftEdge_mm + rightEdge_mm) / 2
  //noinspection ScalaWeakerAccess
  def boxCenterY: Double = (topEdge_mm + bottomEdge_mm) / 2
  //noinspection ScalaWeakerAccess
  def errorX: Double = boxCenterX - ballX_mm
  //noinspection ScalaWeakerAccess
  def errorY: Double = boxCenterY - ballY_mm
  def errorXY: Double = Math.sqrt((errorX * errorX) + (errorY * errorY))
}

object WinstonLutz extends ProcedureOutput with Logging {

  class WinstonLutzTable(tag: Tag) extends Table[WinstonLutz](tag, "winstonLutz") {
    def winstonLutzPK = column[Long]("winstonLutzPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def rtimageUID = column[String]("rtimageUID")

    def rtplanUID = column[String]("rtplanUID")

    def beamName = column[Option[String]]("beamName")

    def gantryAngle_deg = column[Double]("gantryAngle_deg")

    def collimatorAngle_deg = column[Double]("collimatorAngle_deg")

    def dataDate = column[java.sql.Timestamp]("dataDate")

    def topEdge_mm = column[Double]("topEdge_mm")

    def bottomEdge_mm = column[Double]("bottomEdge_mm")

    def leftEdge_mm = column[Double]("leftEdge_mm")

    def rightEdge_mm = column[Double]("rightEdge_mm")

    def ballX_mm = column[Double]("ballX_mm")

    def ballY_mm = column[Double]("ballY_mm")

    def * =
      (
        winstonLutzPK.?,
        outputPK,
        rtimageUID,
        rtplanUID,
        beamName,
        gantryAngle_deg,
        collimatorAngle_deg,
        dataDate,
        topEdge_mm,
        bottomEdge_mm,
        leftEdge_mm,
        rightEdge_mm,
        ballX_mm,
        ballY_mm
      ) <> (WinstonLutz.apply _ tupled, WinstonLutz.unapply)

    def outputFK = foreignKey("WinstonLutz_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[WinstonLutzTable]

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

  override val topXmlLabel = "WinstonLutz"

  def get(winstonLutzPK: Long): Option[WinstonLutz] = {
    val action = for {
      inst <- WinstonLutz.query if inst.winstonLutzPK === winstonLutzPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all WinstonLutz for the given output
    */
  def getByOutput(outputPK: Long): Seq[WinstonLutz] = {
    val action = for {
      inst <- WinstonLutz.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(winstonLutzPK: Long): Int = {
    val q = query.filter(_.winstonLutzPK === winstonLutzPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[WinstonLutz] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[WinstonLutz]()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[WinstonLutz]): Unit = {
    val ops = list.map { loc => WinstonLutz.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class WinstonLutzHistory(output: Output, winstonLutz: WinstonLutz) {}

  /**
    * Get the history of WinstonLutz results.
    *
    * @param machinePK : For this machine
    * @param beamName  : For this beam
    * @return Complete history with baselines sorted by date.
    *
    */
  def historyByBeam(machinePK: Long, beamName: String): Seq[WinstonLutzHistory] = {

    val search = for {
      output <- Output.query.filter(o => o.machinePK === machinePK)
      winstonLutz <- WinstonLutz.query.filter(c => c.outputPK === output.outputPK && c.beamName === beamName)
    } yield {
      (output, winstonLutz)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinstonLutzHistory(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList
  }

  /**
    * Get the history of WinstonLutz results.
    *
    * @param machinePK : For this machine
    * @return Complete history sorted by date.
    *
    */
  def historyByMachine(machinePK: Long): Seq[WinstonLutzHistory] = {

    val search = for {
      output <- Output.query.filter(o => o.machinePK === machinePK)
      winstonLutz <- WinstonLutz.query.filter(c => c.outputPK === output.outputPK)
    } yield {
      (output, winstonLutz)
    }

    // Fetch entire history from the database.  Also sort by dataDate.  This sorting also has the
    // side effect of ensuring that the dataDate is defined.  If it is not defined, this will
    // throw an exception.
    val sr = search.result
    val tsList = Db.run(sr).map(os => WinstonLutzHistory(os._1, os._2)).sortBy(os => os.output.dataDate.get.getTime)

    tsList
  }

  /**
    * Get all WinstonLutz results.
    *
    * @param outputSet  : List of output PKs.  Each WinstonLutz must point to one of the items in this set.
    * @return List of WinstonLutz that point to the output set.
    *
    */
  def listByOutputSet(outputSet: Set[Long]): Seq[WinstonLutz] = {

    val search = for {
      winstonLutz <- WinstonLutz.query.filter(c => c.outputPK.inSet(outputSet))
    } yield {
      winstonLutz
    }

    val tsList = Db.run(search.result)

    tsList
  }

}
