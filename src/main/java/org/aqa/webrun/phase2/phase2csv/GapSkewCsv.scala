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
import org.aqa.webrun.gapSkew.GapOffsetSkew

case class GapOffsetSkewHistory(output: Output, gapOffsetSkew: GapOffsetSkew){
  def gapSkew : GapSkew = ???
}

//abstract class GapSkewCsv extends Phase2Csv[GapOffsetSkewHistory] {

class FooFoo {
  """
  // abbreviation for the long name
  type GOSH = GapOffsetSkewHistory


  String

  /** Name of open beam. */
  protected def beamNameOpen(): String

  /** List of the X coordinates of the centers of areas of interest in the RTPLAN in mm. */
  protected def centerList_mm(): Seq[Double]

  override val dataName: String = "GapSkew"

  override protected def makeColList: Seq[CsvCol[GOSH]] = {
    Seq(
      CsvCol("Beam Name", "Name of beam in RTPLAN", (gsh: GOSH) => gsh.gapSkew.beamName),
      CsvCol("Coll. Angle Rounded deg", "Collimator in degrees rounded to nearest 90", (gsh: GOSH) => Util.angleRoundedTo90(gsh.gapSkew.collimatorAngle_deg),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
      CsvCol("jjjjjjjj", "jjjjjjjjjjjjjjjjjjjj", (gsh: GOSH) => gsh.gapSkew.jjjjj),
        CsvCol("Coll Angle deg", "Collimator angle in degrees not rounded.", (gsh: GOSH) => gsh.gapSkew.collimatorAngle_deg),
      CsvCol("RTIMAGE UID", "UID of RTIMAGE", (gsh: GOSH) => gsh.gapSkew.rtimageUID),

    )
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
        case Right(gos) => Some(GapOffsetSkewHistory(list.head.output,  gos))
        case _ => None
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


  override protected val dicom2HeaderPrefix: Option[String] = Some("Open")

  override protected def getSopUID2(data: GOSH): String = data.head.gapSkew.SOPInstanceUIDOpen

  private def fmtCenter(center_mm: Double) = (center_mm / 10.0).toString.trim + " cm"

  def gapSkewValue(center: Double, vhs: GOSH, h: GapSkew.GapSkewHistory => Double): String = {
    // The GapSkew entry with the correct center of AOI
    val gapSkewOf = vhs.find(history => (history.gapSkew.leftRtplan_mm < center) && (history.gapSkew.rightRtplan_mm > center))
    gapSkewOf match {
      case Some(history) => h(history).toString
      case _             => "NA"
    }
  }

  private def colLS(center_mm: Double): CsvCol[GOSH] = {
    val header = "R LS " + fmtCenter(center_mm)
    val doc = "Avg CU of T3MLCSpeed for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: GOSH) => gapSkewValue(center_mm, vhs, (h: GapSkew.GapSkewHistory) => h.gapSkew.doseMLC_cu)
    CsvCol(header, doc, function)
  }

  private def colOpen(center_mm: Double): CsvCol[GOSH] = {
    val header = "R Open " + fmtCenter(center_mm)
    val doc = "Avg CU of T3 Open for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: GOSH) => gapSkewValue(center_mm, vhs, (h: GapSkew.GapSkewHistory) => h.gapSkew.doseOpen_cu)
    CsvCol(header, doc, function)
  }

  private def colCorr(center_mm: Double): CsvCol[GOSH] = {
    val header = "R Corr " + fmtCenter(center_mm)
    val doc = "100 * LS / Open for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: GOSH) => gapSkewValue(center_mm, vhs, (h: GapSkew.GapSkewHistory) => h.gapSkew.doseMLC_cu * 100 / h.gapSkew.doseOpen_cu)
    CsvCol(header, doc, function)
  }

  private def colDiff(center_mm: Double): CsvCol[GOSH] = {
    val header = "Diff(X) " + fmtCenter(center_mm)
    val doc = "R corr minus avg R corr for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: GOSH) => gapSkewValue(center_mm, vhs, (h: GapSkew.GapSkewHistory) => h.gapSkew.diff_pct)
    CsvCol(header, doc, function)
  }

  private def avgAbsDev(centerList_mm: Seq[Double]) = {
    val header = "Avg of abs Diff(X)"
    val doc = "Average of absolute deviations (Diff Abs)"
    val function = (vhs: GOSH) => {
      Trace.trace(vhs.size)
      vhs.map(_.gapSkew.diff_pct.abs).sum / vhs.size
    }
    CsvCol(header, doc, function)
  }

  def beamList(): Seq[CsvCol[GOSH]] =
    Seq(
      CsvCol("Beam Name MLC", "Name of RTPLAN MLC beam.", (vm: GOSH) => vm.head.gapSkew.beamNameMLC),
      CsvCol("Beam Name Open", "Name of RTPLAN Open beam.", (vm: GOSH) => vm.head.gapSkew.beamNameOpen)
    )

"""
}
