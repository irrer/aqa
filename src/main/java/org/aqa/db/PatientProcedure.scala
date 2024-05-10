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
import org.aqa.web.PatientProcedureXml

import java.sql.Timestamp

/**
  * Store the analysis results for one patient --> procedure association.
  */
case class PatientProcedure(
    patientProcedurePK: Option[Long], // primary key
    dicomAnonymousPK: Long, // references DICOM tag
    institutionPK: Long, // institution to which this belongs
    procedurePK: Long, // procedure associated with the given patient ID.
    active: Boolean // True if patient is active and should be checked for new data.
) {

  def insert: PatientProcedure = {
    PatientProcedureXml.cacheClear(Some(institutionPK))
    val insertQuery =
      PatientProcedure.query returning PatientProcedure.query.map(_.patientProcedurePK) into ((patientProcedure, patientProcedurePK) =>
        patientProcedure.copy(patientProcedurePK = Some(patientProcedurePK))
      )
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = {
    PatientProcedureXml.cacheClear(Some(institutionPK))
    Db.run(PatientProcedure.query.insertOrUpdate(this))
  }

  override def toString: String =
    "patientProcedurePK : " + patientProcedurePK +
      "    dicomAnonymousPK : " + dicomAnonymousPK +
      "    institutionPK : " + institutionPK +
      "    procedurePK : " + procedurePK +
      "    active : " + active
}

object PatientProcedure extends Logging {

  class PatientProcedureTable(tag: Tag) extends Table[PatientProcedure](tag, "patientProcedure") {

    def patientProcedurePK = column[Long]("patientProcedurePK", O.PrimaryKey, O.AutoInc)
    def dicomAnonymousPK = column[Long]("dicomAnonymousPK")
    def institutionPK = column[Long]("institutionPK")
    def procedurePK = column[Long]("procedurePK")
    def active = column[Boolean]("active")

    def * =
      (
        patientProcedurePK.?,
        dicomAnonymousPK,
        institutionPK,
        procedurePK,
        active
      ) <> (PatientProcedure.apply _ tupled, PatientProcedure.unapply)

    def institutionFK =
      foreignKey("PatientProcedure_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def procedureFK =
      foreignKey("PatientProcedure_procedurePKConstraint", procedurePK, Procedure.query)(_.procedurePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def dicomAnonymousFK =
      foreignKey("PatientProcedure_dicomAnonymousPKConstraint", dicomAnonymousPK, DicomAnonymous.query)(_.dicomAnonymousPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PatientProcedureTable]

  /**
    * Given the public key, retrieve the given row.
    * @param patientProcedurePK Public key.
    * @return Row of data, if it exists.
    */
  def get(patientProcedurePK: Long): Option[PatientProcedure] = {
    val action = for {
      inst <- PatientProcedure.query if inst.patientProcedurePK === patientProcedurePK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  def delete(patientProcedurePK: Long): Int = {
    get(patientProcedurePK) match {
      case Some(pp) => PatientProcedureXml.cacheClear(Some(pp.institutionPK))
      case _        =>
    }

    val q = query.filter(_.patientProcedurePK === patientProcedurePK)
    val action = q.delete
    Db.run(action)
  }

  /**
    * Container for the listing of PatientProcedure data in web interface.
    * @param patientProcedure Association between patient --> procedure
    * @param institution Institution this belongs to.
    * @param procedure Procedure to be used.
    */
  case class ExtendedData(patientProcedure: PatientProcedure, institution: Institution, procedure: Procedure, dicomAnonymous: DicomAnonymous, mostRecentSeriesDate: Option[Timestamp]) {}

  /**
    * Get the listing of PatientProcedure data in web interface.
    * @param institutionPK For this institution.
    * @return List of rows and their associated data.
    */
  def listExtended(institutionPK: Long): Seq[ExtendedData] = {

    val action = for {
      patientProcedure <- query.filter(_.institutionPK === institutionPK)
      dicomAnon <- DicomAnonymous.query.filter(_.dicomAnonymousPK === patientProcedure.dicomAnonymousPK)
      institution <- Institution.query.filter(_.institutionPK === institutionPK)
      procedure <- Procedure.query.filter(_.procedurePK === patientProcedure.procedurePK)
    } yield (patientProcedure, institution, procedure, dicomAnon)

    val listWithoutDates = Db.run(action.result).map(pip => ExtendedData(pip._1, pip._2, pip._3, pip._4, None))

    def addDate(extData: ExtendedData): ExtendedData = {
      val action = for {
        ds <- DicomSeries.query.filter(_.patientID === extData.dicomAnonymous.value)
        inputDate <- Input.query.filter(i => i.inputPK === ds.inputPK).map(_.dataDate)
      } yield inputDate

      val date = Db.run(action.result).flatten.sortBy(_.getTime).lastOption

      extData.copy(mostRecentSeriesDate = date)
    }

    val list = listWithoutDates.map(addDate)

    list
  }

}
