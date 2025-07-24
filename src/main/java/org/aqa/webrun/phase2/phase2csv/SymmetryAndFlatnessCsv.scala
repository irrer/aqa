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

import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness
import org.aqa.webrun.psm.PSMGrid

import java.sql.Timestamp

class SymmetryAndFlatnessCsv(metadataCache: MetadataCache) extends Phase2Csv[SymmetryAndFlatness.SymmetryAndFlatnessHistory](metadataCache: MetadataCache) {

  // abbreviation for the long name
  private type SF = SymmetryAndFlatness.SymmetryAndFlatnessHistory

  override val dataName: String = "Symmetry And Flatness"

  /**
    * Indicate how and if this is used as a baseline.  Will be one of:
    * <p/>
    * explicit : The user has explicitly indicated that this is to be used as a baseline.
    * <p/>
    * implicit : In lieu of an explicit baseline, this is being used.  Note that the
    * chronologically first reading will always be used as a baseline.
    * <p/>
    * [blank] : This is not a baseline.
    *
    * @param sf Sym+Flat to be examined.
    * @return Baseline designation.
    */
  private def baselineDesignation(sf: SF): String = {
    (sf.symmetryAndFlatness.isBaseline, sf.symmetryAndFlatness.symmetryAndFlatnessPK.get == sf.baseline.symmetryAndFlatnessPK.get) match {
      case _ if sf.symmetryAndFlatness.isBaseline                                                         => "explicit"
      case _ if sf.symmetryAndFlatness.symmetryAndFlatnessPK.get == sf.baseline.symmetryAndFlatnessPK.get => "implicit"
      case _                                                                                              => ""
    }

  }

  private def optDate(ts: Option[Timestamp]): String = {
    if (ts.isDefined) {
      Util.formatDate(Util.standardDateFormat, ts.get).replace('T', ' ')
    } else
      "NA"
  }

  private def psmGridOf(sf: SF): Option[PSMGrid] = {
    if (sf.symmetryAndFlatness.psmDataDate.isDefined)
      PSMGrid.get(sf.output.machinePK.get, sf.symmetryAndFlatness.psmDataDate.get)
    else
      None
  }

  private def psmGridBaselineOf(sf: SF): Option[PSMGrid] = {
    if (sf.baseline.psmDataDate.isDefined)
      PSMGrid.get(sf.output.machinePK.get, sf.baseline.psmDataDate.get)
    else
      None
  }

  private def optDouble(d: Option[Double]): Any = {
    if (d.isDefined)
      d.get
    else
      "NA"
  }

  private def axialSym(sf: SF): Any = {
    optDouble(sf.symmetryAndFlatness.axialSymmetry(psmGridOf(sf)))
  }

  private def transSym(sf: SF): Any = {
    optDouble(sf.symmetryAndFlatness.transverseSymmetry(psmGridOf(sf)))
  }

  private def flat(sf: SF): Any = {
    optDouble(sf.symmetryAndFlatness.flatness(psmGridOf(sf)))
  }

  private def constancy(sf: SF): Any = {
    optDouble(sf.symmetryAndFlatness.profileConstancy(psmGridOf(sf), sf.baseline, psmGridBaselineOf(sf)))
  }

  override protected def makeColList: Seq[CsvCol[SF]] = {

    Seq(
      CsvCol("Beam Name", "Common name of RTPLAN beam.", (sf: SF) => sf.symmetryAndFlatness.beamName),
      CsvCol("Uses PSM", "True if PSM (Pixel Sensitivity Matrix) was used to correct image.", (sf: SF) => sf.symmetryAndFlatness.psmDataDate.isDefined.toString),
      CsvCol("Axial Symmetry", "((top - bottom) / bottom) * 100", axialSym),
      CsvCol("Transverse Symmetry", "((right - left ) / left ) * 100", transSym),
      CsvCol("Flatness", "(max - min) / (max + min) * 100", flat),
      CsvCol(
        "Profile Constancy",
        "average of (X / center) - (baseline X / baseline center), where X iterates through top, bottom, right, left",
        constancy
      ),
      CsvCol(
        "Baseline Designation",
        "explicit: Designated by user as a baseline.  implicit: Used as baseline when an explicit one is not defined.  If blank, then it is not used as a baseline.",
        (sf: SF) => baselineDesignation(sf)
      ),
      CsvCol("PSM Date", "Date and time that PSM was delivered.  Used only when available.", (sf: SF) => optDate(sf.symmetryAndFlatness.psmDataDate)),
      CsvCol("Span", "Distance in mm between centers of opposing sample points.", (sf: SF) => sf.symmetryAndFlatness.span_mm.get),
      CsvCol("Diameter", "Diameter in mm in the isoplane of the circle of pixels sampled at each of the five points.", (sf: SF) => sf.symmetryAndFlatness.diameter_mm.get),
      CsvCol("RTImageSID", "Distance mm from source to image DICOM tag 3002,0026.", (sf: SF) => sf.symmetryAndFlatness.RTImageSID_mm.get),
      CsvCol("Top CU", "Average CU of pixel values in the top circle of the image.", (sf: SF) => sf.symmetryAndFlatness.top_cu),
      CsvCol("Bottom CU", "Average CU of pixel values in the bottom circle of the image.", (sf: SF) => sf.symmetryAndFlatness.bottom_cu),
      CsvCol("Left CU", "Average CU of pixel values in the left hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.left_cu),
      CsvCol("Right CU", "Average CU of pixel values in the right hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.right_cu),
      CsvCol("Center CU", "Average CU of pixel values in center circle of the image.", (sf: SF) => sf.symmetryAndFlatness.center_cu),
      CsvCol("Baseline Acquisition", "When data for baseline was acquired", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.dataDate.get)),
      CsvCol("Baseline Analysis", "When data for baseline was analyzed", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.startDate)),
      CsvCol("Baseline Top CU", "For baseline image, average CU of pixel values in the top circle.", (sf: SF) => sf.baseline.top_cu),
      CsvCol("Baseline Bottom CU", "For baseline image, average CU of pixel values in the bottom circle.", (sf: SF) => sf.baseline.bottom_cu),
      CsvCol("Baseline Left CU", "For baseline image, average CU of pixel values in the left circle.", (sf: SF) => sf.baseline.left_cu),
      CsvCol("Baseline Right CU", "For baseline image, average CU of pixel values in the right circle.", (sf: SF) => sf.baseline.right_cu),
      CsvCol("Baseline Center CU", "For baseline image, average CU of pixel values in the center circle.", (sf: SF) => sf.baseline.center_cu),
      CsvCol("Top Std Dev CU", "Standard Deviation in CU of pixel values in the top circle of the image.", (sf: SF) => sf.symmetryAndFlatness.topStdDev_cu),
      CsvCol("Bottom Std Dev CU", "Standard Deviation in CU of pixel values in the bottom circle of the image.", (sf: SF) => sf.symmetryAndFlatness.bottomStdDev_cu),
      CsvCol("Left Std Dev CU", "Standard Deviation in CU of pixel values in the left hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.leftStdDev_cu),
      CsvCol("Right Std Dev CU", "Standard Deviation in CU of pixel values in the right hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.rightStdDev_cu),
      CsvCol("Center Std Dev CU", "Standard Deviation in CU of pixel values in center circle of the image.", (sf: SF) => sf.symmetryAndFlatness.centerStdDev_cu),
      CsvCol("Top COV", "Coefficient of Variation (stdDev/mean) of pixel values in the top circle of the image.", (sf: SF) => sf.symmetryAndFlatness.topCOV),
      CsvCol("Bottom COV", "Coefficient of Variation (stdDev/mean) of pixel values in the bottom circle of the image.", (sf: SF) => sf.symmetryAndFlatness.bottomCOV),
      CsvCol("Left COV", "Coefficient of Variation (stdDev/mean) of pixel values in the left hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.leftCOV),
      CsvCol("Right COV", "Coefficient of Variation (stdDev/mean) of pixel values in the right hand circle of the image.", (sf: SF) => sf.symmetryAndFlatness.rightCOV),
      CsvCol("Center COV", "Coefficient of Variation (stdDev/mean) of pixel values in center circle of the image.", (sf: SF) => sf.symmetryAndFlatness.centerCOV),
      CsvCol("PSM Date", "If present, indicates the date of the Pixel Sensitivity map applied to this result.", (sf: SF) => optDate(sf.symmetryAndFlatness.psmDataDate)),
      CsvCol(
        "span",
        "Distance in mm between opposing sample points.",
        (sf: SF) => if (sf.symmetryAndFlatness.span_mm.isDefined) sf.symmetryAndFlatness.span_mm.get else "NA"
      ),
      CsvCol("diameter", "Diameter in mm of point sampled.", (sf: SF) => if (sf.symmetryAndFlatness.diameter_mm.isDefined) sf.symmetryAndFlatness.diameter_mm.get else "NA"),
      CsvCol(
        "RTImageSID",
        "Source to image distance in mm (SID).",
        (sf: SF) => if (sf.symmetryAndFlatness.RTImageSID_mm.isDefined) sf.symmetryAndFlatness.RTImageSID_mm.get else "NA"
      )
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[SF] = {
    val data = SymmetryAndFlatness.historyForMachine(machinePK, metadataCache.phase2ProcedurePK) ++ SymmetryAndFlatness.historyForMachine(machinePK, metadataCache.phase3ProcedurePK)
    val symFlat = data.sortBy(h => h.output.dataDate.get.getTime + h.symmetryAndFlatness.beamName)
    symFlat
  }

  override def getSopUidList(data: SF): Seq[String] = Seq(data.symmetryAndFlatness.SOPInstanceUID)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")
  override def getOutput(data: SF): Output = data.output
}
