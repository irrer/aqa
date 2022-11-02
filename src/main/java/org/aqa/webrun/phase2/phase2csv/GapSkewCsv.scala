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

package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.GapSkew
import org.aqa.db.Output
import org.aqa.webrun.gapSkew.ColAngle
import org.aqa.webrun.gapSkew.GapOffsetSkew
import org.aqa.webrun.gapSkew.GosValue
import org.aqa.webrun.gapSkew.JawJaw

import scala.Double.NaN

case class GapOffsetSkewHistory(output: Output, gapOffsetSkew: GapOffsetSkew) {}

class GapSkewCsv extends Phase2Csv[GapOffsetSkewHistory] {

  // abbreviation for the long name
  type GOSH = GapOffsetSkewHistory

  /** Name of open beam. */
  // protected def beamNameOpen(): String

  /** List of the X coordinates of the centers of areas of interest in the RTPLAN in mm. */
  // protected def centerList_mm(): Seq[Double]

  override val dataName: String = "GapOffsetSkew"

  /**
    * Given a field within a <code>ColAngle</code>, make a pair of column, one for
    * each of the 90 and 270 degree MLC angles.
    * @param field For this field.
    * @return Pair of CSV columns.
    */
  private def mlcToCsvCol(field: ColAngle => GosValue): Seq[CsvCol[GapOffsetSkewHistory]] = {
    val sg = GapSkewCsv.staticGapOffsetSkew
    val sf090 = field(sg.col090)
    val sf270 = field(sg.col270)

    val csvCol090 = CsvCol(s"${sf090.fullName} (${sf090.units})", sf090.description, (gosh: GOSH) => field(gosh.gapOffsetSkew.col090).v.toString)
    val csvCol270 = CsvCol(s"${sf270.fullName} (${sf270.units})", sf270.description, (gosh: GOSH) => field(gosh.gapOffsetSkew.col270).v.toString)

    Seq(csvCol090, csvCol270)
  }

  /**
    * Given a field within a <code>ColAngle</code>, make a pair of column, one for
    * each of the 90 and 270 degree MLC angles.
    * @param field For this field.
    * @return Pair of CSV columns.
    */
  private def jawToCsvCol(field: JawJaw => GosValue): Seq[CsvCol[GOSH]] = {
    val sg = GapSkewCsv.staticGapOffsetSkew
    val sf = field(sg.jawJaw)

    val csvCol = CsvCol(s"${sf.fullName} (${sf.units})", sf.description, (gosh: GOSH) => field(gosh.gapOffsetSkew.jawJaw).v.toString)

    Seq(csvCol)
  }

  /**
    * Given a field within a <code>ColAngle</code>, make a pair of column, one for
    * each of the 90 and 270 degree MLC angles.
    * @param field For this field.
    * @return Pair of CSV columns.
    */
  private def gapSkewCsvCol(name: String, description: String, field: GapSkew => Option[Double]): Seq[CsvCol[GOSH]] = {

    val C090A = CsvCol(
      "C90 A " + name,
      "Collimator 90 Bank A " + description,
      (gosh: GOSH) =>
        field(gosh.gapOffsetSkew.col090.bankA) match {
          case Some(d) => d.toString
          case _       => "NA"
        }
    )

    val C090B = CsvCol(
      "C90 B " + name,
      "Collimator 90 Bank B " + description,
      (gosh: GOSH) =>
        field(gosh.gapOffsetSkew.col090.bankB) match {
          case Some(d) => d.toString
          case _       => "NA"
        }
    )

    val C270A = CsvCol(
      "C270 A " + name,
      "Collimator 270 Bank A " + description,
      (gosh: GOSH) =>
        field(gosh.gapOffsetSkew.col270.bankA) match {
          case Some(d) => d.toString
          case _       => "NA"
        }
    )

    val C270B = CsvCol(
      "C270 B " + name,
      "Collimator 270 Bank B " + description,
      (gosh: GOSH) =>
        field(gosh.gapOffsetSkew.col270.bankB) match {
          case Some(d) => d.toString
          case _       => "NA"
        }
    )

    val J270 = CsvCol(
      "J270 " + name,
      "Jaw 270 " + description,
      (gosh: GOSH) =>
        field(gosh.gapOffsetSkew.jawJaw.jawPair) match {
          case Some(d) => d.toString
          case _       => "NA"
        }
    )

    val list = Seq(C090A, C090B, C270A, C270B, J270)

    list
  }

  override protected def makeColList: Seq[CsvCol[GOSH]] = {

    val list =
      mlcToCsvCol((ca: ColAngle) => ca.gap) ++
        mlcToCsvCol((ca: ColAngle) => ca.offset) ++
        mlcToCsvCol((ca: ColAngle) => ca.aSkew_mmPer40cm) ++
        mlcToCsvCol((ca: ColAngle) => ca.bSkew_mmPer40cm) ++
        mlcToCsvCol((ca: ColAngle) => ca.aRight) ++
        mlcToCsvCol((ca: ColAngle) => ca.aLeft) ++
        mlcToCsvCol((ca: ColAngle) => ca.bRight) ++
        mlcToCsvCol((ca: ColAngle) => ca.bLeft) ++
        mlcToCsvCol((ca: ColAngle) => ca.separation) ++
        mlcToCsvCol((ca: ColAngle) => ca.aRightLeftDiff) ++
        mlcToCsvCol((ca: ColAngle) => ca.aSkew_deg) ++
        mlcToCsvCol((ca: ColAngle) => ca.aRightLeftAvg) ++
        mlcToCsvCol((ca: ColAngle) => ca.bRightLeftDiff) ++
        mlcToCsvCol((ca: ColAngle) => ca.bSkew_deg) ++
        mlcToCsvCol((ca: ColAngle) => ca.bRightLeftAvg) ++
        mlcToCsvCol((ca: ColAngle) => ca.rightDiff) ++
        mlcToCsvCol((ca: ColAngle) => ca.leftDiff) ++
        mlcToCsvCol((ca: ColAngle) => ca.rightLeftDiff) ++
        mlcToCsvCol((ca: ColAngle) => ca.skewDiff_deg) ++
        mlcToCsvCol((ca: ColAngle) => ca.skewDiff_mmPer40cm) ++
        mlcToCsvCol((ca: ColAngle) => ca.rightAvg) ++
        mlcToCsvCol((ca: ColAngle) => ca.leftAvg) ++
        mlcToCsvCol((ca: ColAngle) => ca.abRightLeftAvg) ++
        mlcToCsvCol((ca: ColAngle) => ca.abSkewAvg_deg) ++
        mlcToCsvCol((ca: ColAngle) => ca.abSkewAvg_mmPer40cm) ++
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        jawToCsvCol((jaw: JawJaw) => jaw.gap) ++
        jawToCsvCol((jaw: JawJaw) => jaw.offset) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawSkew_mmPer40cm) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawSkew_deg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawRight) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawLeft) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawRightLeftDiff) ++
        jawToCsvCol((jaw: JawJaw) => jaw.jawAvg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawRightDiff) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawLeftDiff) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawRightLeftDiff) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawSkewDiff_mmPer40cm) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawSkewDiff_deg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawRightAvg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawLeftAvg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawRightLeftAvg) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawSkewAvg_mmPer40cm) ++
        jawToCsvCol((jaw: JawJaw) => jaw.aJawSkewAvg_deg) ++
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        gapSkewCsvCol("Top Right", "Y values measured at top right.", _.topRightValue_mm) ++
        gapSkewCsvCol("Top Left", "Y values measured at top left.", _.topLeftValue_mm) ++
        gapSkewCsvCol("Bottom Right", "Y values measured at bottom right.", _.bottomRightValue_mm) ++
        gapSkewCsvCol("Bottom Left", "Y values measured at bottom left.", _.bottomLeftValue_mm)
    list
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[GOSH] = {
    val data = GapSkew.historyByMachine(machinePK)
    // @formatter:off
    val gapSkewGroupList = data.                                   // get all history for this machine
      groupBy(gs => gs.output).
      values.
      map(group => group.sortBy(_.gapSkew.beamName)).
      toSeq
    // @formatter:on

    def toGosh(list: Seq[GapSkew.GapSkewHistory]): Option[GOSH] = {
      GapOffsetSkew.makeGapOffsetSkew(list.map(_.gapSkew)) match {
        case Right(gos) => Some(GapOffsetSkewHistory(list.head.output, gos))
        case _          => None
      }
    }

    val goshList = gapSkewGroupList.flatMap(toGosh)
    goshList
  }

  override def getOutput(data: GOSH): Output = data.output

  /**
    * Get the SOP of the DICOM for this data set.
    *
    * @param data Data using DICOM data.
    * @return SOP instance UID.
    */
  override protected def getSopUID(data: GOSH): Option[String] = None

  override protected val showDicomMetadata: Boolean = false

  override protected def getSopUID2(data: GOSH): String = "" // data.head.gapSkew.SOPInstanceUIDOpen

}

object GapSkewCsv {

  private def prototype =
    GapSkew(
      gapSkewPK = Some(-1),
      outputPK = -1.toLong,
      rtimageUID = "",
      beamName = "",
      collimatorAngle_deg = NaN,
      //
      measurementWidth_mm = NaN,
      measurementSeparation_mm = NaN,
      //
      topLeftEdgeTypeName = Some("filler"),
      topLeftValue_mm = Some(NaN),
      topLeftPlanned_mm = Some(NaN),
      //
      topRightEdgeTypeName = Some("filler"),
      topRightValue_mm = Some(NaN),
      topRightPlanned_mm = Some(NaN),
      //
      bottomLeftEdgeTypeName = Some("filler"),
      bottomLeftValue_mm = Some(NaN),
      bottomLeftPlanned_mm = Some(NaN),
      //
      bottomRightEdgeTypeName = Some("filler"),
      bottomRightValue_mm = Some(NaN),
      bottomRightPlanned_mm = Some(NaN)
    )

  // @formatter:off

  private def c090A = prototype.copy(
    collimatorAngle_deg = 90,
    topLeftEdgeTypeName = Some("X2 MLC Horz"),
    topRightEdgeTypeName = Some("X2 MLC Horz"),
    bottomLeftEdgeTypeName = Some("X1 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X1 Jaw Horz"))

  private def c090B = prototype.copy(
    collimatorAngle_deg = 90,
    topLeftEdgeTypeName = Some("X2 Jaw Horz"),
    topRightEdgeTypeName = Some("X2 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X1 MLC Horz"),
    bottomRightEdgeTypeName = Some("X1 MLC Horz"))

  private def c270A = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 Jaw Horz"),
    topRightEdgeTypeName = Some("X1 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X2 MLC Horz"),
    bottomRightEdgeTypeName = Some("X2 MLC Horz"))

  private def c270B = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 MLC Horz"),
    topRightEdgeTypeName = Some("X1 MLC Horz"),
    bottomLeftEdgeTypeName = Some("X2 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X2 Jaw Horz"))

  private def c270Jaw = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 Jaw Horz"),
    topRightEdgeTypeName = Some("X1 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X2 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X2 Jaw Horz"))
  // @formatter:on

  // construct a static instance of the GapOffsetSkew class that is useful for accessing names and descriptions.
  def staticGapOffsetSkew = new GapOffsetSkew(c090A, c090B, c270A, c270B, c270Jaw)
}
