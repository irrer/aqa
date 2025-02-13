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

import com.pixelmed.dicom.AttributeList
import org.aqa.db.Db.driver.api._
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLIsoTable

import java.sql.Timestamp

/**
  * Encapsulate data from a single IsoCheck measurement.  There will only be one for WL output.
  * This contains all the values determined by gradient descent (the yellow fields), which in
  * Excel are found using the "Solver".  They will only be defined if the table analysis was
  * performed.  This means that either all or none of them will be defined.
  */
case class IsoCheck(
    isoCheckPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    dX_mm: Option[Double], // spreadsheet Analysis L14
    dZ_mm: Option[Double], // spreadsheet Analysis M14
    tableX_mm: Option[Double], // spreadsheet Analysis N14
    tableZ_mm: Option[Double], // spreadsheet Analysis O14
    collX_mm: Option[Double], // spreadsheet Collimator H4
    collZ_mm: Option[Double] // spreadsheet Collimator I4
) {

  def insert: IsoCheck = {
    val insertQuery = IsoCheck.query returning IsoCheck.query.map(_.isoCheckPK) into
      ((isoCheck, isoCheckPK) => isoCheck.copy(isoCheckPK = Some(isoCheckPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(IsoCheck.query.insertOrUpdate(this))

}

object IsoCheck {
  class IsoCheckTable(tag: Tag) extends Table[IsoCheck](tag, "isoCheck") {

    def isoCheckPK = column[Long]("isoCheckPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    private def dX_mm = column[Option[Double]]("dX_mm")
    private def dZ_mm = column[Option[Double]]("dZ_mm")
    private def tableX_mm = column[Option[Double]]("tableX_mm")
    private def tableZ_mm = column[Option[Double]]("tableZ_mm")
    private def collX_mm = column[Option[Double]]("collX_mm")
    private def collZ_mm = column[Option[Double]]("collZ_mm")

    def * =
      (
        isoCheckPK.?,
        outputPK,
        dX_mm,
        dZ_mm,
        tableX_mm,
        tableZ_mm,
        collX_mm,
        collZ_mm
      ) <> (IsoCheck.apply _ tupled, IsoCheck.unapply)

    def outputFK = foreignKey("IsoCheck_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[IsoCheckTable]

  def get(isoCheckPK: Long): Option[IsoCheck] = {
    val action = for {
      inst <- IsoCheck.query if inst.isoCheckPK === isoCheckPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[IsoCheck] = {
    val action = for {
      inst <- IsoCheck.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(isoCheckPK: Long): Int = {
    val q = query.filter(_.isoCheckPK === isoCheckPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  case class IsoCheckHistory(output: Output, isoCheck: WLIsoCheck, collimator: WLCollimator, isoTable: Option[WLIsoTable], wlList: Seq[WinstonLutz]) extends HasOutput {

    val date: Timestamp = output.dataDate.get
    def getTime: Long = date.getTime
    val hasTable: Boolean = isoTable.isDefined

    override def getOutput: Output = output

    def getBeam(gantry: Int, collimatorAngle: Int, tableAngle: Int = 0): Option[WinstonLutz] =
      wlList.find(wl =>
        (wl.gantryAngleRounded == gantry) &&
          (wl.collimatorAngleRounded == collimatorAngle) &&
          (wl.tableAngleRounded.get == tableAngle)
      )
  }

  private def makeIsoCheckHistory(output: Output, isoCheck: IsoCheck, wlList: Seq[WinstonLutz]): Option[IsoCheckHistory] = {

    val beamList = wlList.map(wl => org.aqa.webrun.wl.isoCheck.WLBeam(wl, new AttributeList))

    val wlIsoCheck = WLIsoCheck.make(beamList)

    val wlCollimator = WLCollimator.make(beamList).get

    val wlIsoTable = WLIsoTable.make(beamList)

    wlCollimator.setColl_X_Optimized(isoCheck.collX_mm.get)
    wlCollimator.setColl_Z_Optimized(isoCheck.collZ_mm.get)

    if (
      wlIsoTable.isDefined &&
      isoCheck.dX_mm.isDefined &&
      isoCheck.dZ_mm.isDefined &&
      isoCheck.tableX_mm.isDefined &&
      isoCheck.tableZ_mm.isDefined
    ) {
      wlIsoTable.get.set_dXT__0_Optimized(isoCheck.dX_mm.get)
      wlIsoTable.get.set_dZT__0_Optimized(isoCheck.dZ_mm.get)
      wlIsoTable.get.set_IsoTable_X_Optimized(isoCheck.tableX_mm.get)
      wlIsoTable.get.set_IsoTable_Z_Optimized(isoCheck.tableZ_mm.get)
    }

    if (wlIsoCheck.isDefined) {
      val ich = IsoCheckHistory(output, wlIsoCheck.get, wlCollimator, wlIsoTable, wlList)
      Some(ich)
    } else
      None
  }

  /**
    * Get IsoCheck results.
    *
    * @param machinePK: For this machine
    */
  def history(machinePK: Long): Seq[IsoCheckHistory] = {
    val searchIsoCheck = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      isoCheck <- IsoCheck.query.filter(c => c.outputPK === output.outputPK)
    } yield (output, isoCheck)

    val isoCheckList = Db.run(searchIsoCheck.result).sortBy(_._1.dataDate.get.getTime)

    val searchWL = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      _ <- IsoCheck.query.filter(c => c.outputPK === output.outputPK)
      wl <- WinstonLutz.query.filter(wl => wl.outputPK === output.outputPK)
    } yield wl

    val wlList = Db.run(searchWL.result)

    val result = isoCheckList.flatMap(h => makeIsoCheckHistory(h._1, h._2, wlList.filter(_.outputPK == h._2.outputPK).sortBy(_.dataDate.getTime)))
    result
  }

}
