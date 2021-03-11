package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness


class CsvSymmetryAndFlatness extends Phase2Csv[SymmetryAndFlatness.SymmetryAndFlatnessHistory] {

  // Get the list of all possible sym+flat beams.  Sort them so that they always show up in the same order.
  private val beamNameList = Config.SymmetryAndFlatnessBeamList.sorted

  // abbreviation for the long name
  type SF = SymmetryAndFlatness.SymmetryAndFlatnessHistory

  override protected def makeColList: Seq[Col] = {
    Trace.trace()
    Seq(
      Col("Beam Name", (sf: SF) => sf.symmetryAndFlatness.beamName),

      Col("Acquisition Date", (sf: SF) => Util.standardDateFormat.format(sf.output.dataDate.get)),
      Col("Analysis Date", (sf: SF) => Util.standardDateFormat.format(sf.output.startDate)),

      Col("Baseline Delivery Date", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.dataDate.get)),
      Col("Baseline Analysis Date", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.startDate)),

      Col("Axial Symmetry", (sf: SF) => sf.symmetryAndFlatness.axialSymmetry_pct.toString),
      Col("Transverse Symmetry", (sf: SF) => sf.symmetryAndFlatness.transverseSymmetry.toString),
      Col("Flatness", (sf: SF) => sf.symmetryAndFlatness.flatness.toString),
      Col("Profile Constancy", (sf: SF) => sf.symmetryAndFlatness.profileConstancy(sf.baseline).toString),

      Col("Top CU", (sf: SF) => sf.symmetryAndFlatness.top_cu.toString),
      Col("Bottom CU", (sf: SF) => sf.symmetryAndFlatness.bottom_cu.toString),
      Col("Left CU", (sf: SF) => sf.symmetryAndFlatness.left_cu.toString),
      Col("Right CU", (sf: SF) => sf.symmetryAndFlatness.right_cu.toString),
      Col("Center CU", (sf: SF) => sf.symmetryAndFlatness.center_cu.toString),

      Col("Baseline Top CU", (sf: SF) => sf.baseline.top_cu.toString),
      Col("Baseline Bottom CU", (sf: SF) => sf.baseline.bottom_cu.toString),
      Col("Baseline Left CU", (sf: SF) => sf.baseline.left_cu.toString),
      Col("Baseline Right CU", (sf: SF) => sf.baseline.right_cu.toString),
      Col("Baseline Center CU", (sf: SF) => sf.baseline.center_cu.toString),
      // Col("Baseline Center CU", (sf: SF) => getAl(sf).get(TagByName.SoftwareVersions).getSingleStringValueOrEmptyString)
    )
  }


  override protected def getAl(data: SF): AttributeList = Phase2Csv.getAlBySop(data.symmetryAndFlatness.SOPInstanceUID)


  /**
   * Get the data for a particular machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of data for the particular machine.
   */
  override protected def getData(machinePK: Long): Seq[SF] = {
    val symFlat = SymmetryAndFlatness.history(machinePK)
    symFlat
  }


  override def getOutput(data: SF): Output = data.output
}
