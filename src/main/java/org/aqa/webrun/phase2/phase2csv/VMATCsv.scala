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

import edu.umro.ScalaUtil.Trace
import org.aqa.db.Output
import org.aqa.db.VMAT

abstract class VMATCsv extends Phase2Csv[Seq[VMAT.VMATHistory]] {

  // abbreviation for the long name
  //type VHS = VMATCsv.VHS
  private type VHS = Seq[VMAT.VMATHistory]

  /** Name of MLC beam. */
  protected def beamNameMLC(): String

  /** Name of open beam. */
  protected def beamNameOpen(): String

  /** List of the X coordinates of the centers of areas of interest in the RTPLAN in mm. */
  protected def centerList_mm(): Seq[Double]

  override val dataName: String = "VMAT " + beamNameMLC

  override protected def makeColList: Seq[CsvCol[VHS]] = makeVHColList(centerList_mm())

  /**
   * Get the data for a particular machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of data for the particular machine.
   */
  override protected def getData(machinePK: Long): Seq[VHS] = {
    // @formatter:off
    val data = VMAT.history(machinePK, MetadataCache.metadataCache.phase2ProcedurePK) ++ VMAT.history(machinePK, MetadataCache.metadataCache.phase3ProcedurePK)
    val vmatList = data.                                   // get all history for this machine
      filter(_.vmat.beamNameMLC.equals(beamNameMLC())).    // only for this beam
      groupBy(_.vmat.SOPInstanceUIDMLC).                   // group all items for a given beam together
      values.                                              // do not need SOP UID
      map(seq => seq.sortBy(v => v.vmat.leftRtplan_mm)).   // sort within each set by X position
      toSeq.                                               // allow sorting
      sortBy(vhs => vhs.head.output.dataDate.get.getTime)  // sort all by data date
    // @formatter:on

    vmatList
  }

  override def getOutput(data: VHS): Output = data.head.output

  /**
   * Get the SOP of the DICOM for this data set.
   *
   * @param data Data using DICOM data.
   * @return SOP instance UID.
   */

  override def getSopUidList(data: VHS): Seq[String] = Seq(data.head.vmat.SOPInstanceUIDMLC)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("", "Open")

  private def fmtCenter(center_mm: Double) = (center_mm / 10.0).toString.trim + " cm"

  def vmatValue(center: Double, vhs: VHS, h: VMAT.VMATHistory => Double): String = {
    // The VMAT entry with the correct center of AOI
    val vmatOf = vhs.find(history => (history.vmat.leftRtplan_mm < center) && (history.vmat.rightRtplan_mm > center))
    vmatOf match {
      case Some(history) => h(history).toString
      case _ => "NA"
    }
  }

  private def colLS(center_mm: Double): CsvCol[VHS] = {
    val header = "R LS " + fmtCenter(center_mm)
    val doc = "Avg CU of T3MLCSpeed for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: VHS) => vmatValue(center_mm, vhs, (h: VMAT.VMATHistory) => h.vmat.doseMLC_cu)
    CsvCol(header, doc, function)
  }

  private def colOpen(center_mm: Double): CsvCol[VHS] = {
    val header = "R Open " + fmtCenter(center_mm)
    val doc = "Avg CU of T3 Open for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: VHS) => vmatValue(center_mm, vhs, (h: VMAT.VMATHistory) => h.vmat.doseOpen_cu)
    CsvCol(header, doc, function)
  }

  private def colCorr(center_mm: Double): CsvCol[VHS] = {
    val header = "R Corr " + fmtCenter(center_mm)
    val doc = "100 * LS / Open for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: VHS) => vmatValue(center_mm, vhs, (h: VMAT.VMATHistory) => h.vmat.doseMLC_cu * 100 / h.vmat.doseOpen_cu)
    CsvCol(header, doc, function)
  }

  private def colDiff(center_mm: Double): CsvCol[VHS] = {
    val header = "Diff(X) " + fmtCenter(center_mm)
    val doc = "R corr minus avg R corr for AOI centered at X = " + fmtCenter(center_mm)
    val function = (vhs: VHS) => vmatValue(center_mm, vhs, (h: VMAT.VMATHistory) => h.vmat.diff_pct)
    CsvCol(header, doc, function)
  }

  private def avgAbsDev(centerList_mm: Seq[Double]) = {
    val header = "Avg of abs Diff(X)"
    val doc = "Average of absolute deviations (Diff Abs)"
    val function = (vhs: VHS) => {
      Trace.trace(vhs.size)
      vhs.map(_.vmat.diff_pct.abs).sum / vhs.size
    }
    CsvCol(header, doc, function)
  }

  def beamList(): Seq[CsvCol[VHS]] =
    Seq(
      CsvCol("Beam Name MLC", "Name of RTPLAN MLC beam.", (vm: VHS) => vm.head.vmat.beamNameMLC),
      CsvCol("Beam Name Open", "Name of RTPLAN Open beam.", (vm: VHS) => vm.head.vmat.beamNameOpen)
    )

  def makeVHColList(centerList_mm: Seq[Double]): Seq[CsvCol[VHS]] = {
    beamList() ++
      centerList_mm.map(center => colLS(center)) ++
      centerList_mm.map(center => colOpen(center)) ++
      centerList_mm.map(center => colCorr(center)) ++
      centerList_mm.map(center => colDiff(center)) :+ avgAbsDev(centerList_mm)
  }

}
