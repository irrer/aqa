package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness

class CsvSymmetryAndFlatness extends Phase2Csv[SymmetryAndFlatness.SymmetryAndFlatnessHistory] {

  // abbreviation for the long name
  type SF = SymmetryAndFlatness.SymmetryAndFlatnessHistory

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
    *
    * @return Baseline designation.
    */
  private def baselineDesignation(sf: SF): String = {
    (sf.symmetryAndFlatness.isBaseline, sf.symmetryAndFlatness.symmetryAndFlatnessPK.get == sf.baseline.symmetryAndFlatnessPK.get) match {
      case _ if sf.symmetryAndFlatness.isBaseline                                                         => "explicit"
      case _ if sf.symmetryAndFlatness.symmetryAndFlatnessPK.get == sf.baseline.symmetryAndFlatnessPK.get => "implicit"
      case _                                                                                              => ""
    }

  }

  override protected def makeColList: Seq[Col] = {
    Trace.trace()
    Seq(
      Col("Beam Name", (sf: SF) => sf.symmetryAndFlatness.beamName),
      Col("Baseline Acquisition", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.dataDate.get)),
      Col("Baseline Analysis", (sf: SF) => Util.standardDateFormat.format(sf.baselineOutput.startDate)),
      Col("Axial Symmetry", (sf: SF) => sf.symmetryAndFlatness.axialSymmetry),
      Col("Transverse Symmetry", (sf: SF) => sf.symmetryAndFlatness.transverseSymmetry),
      Col("Flatness", (sf: SF) => sf.symmetryAndFlatness.flatness),
      Col("Profile Constancy", (sf: SF) => sf.symmetryAndFlatness.profileConstancy(sf.baseline)),
      Col("Baseline Designation", (sf: SF) => baselineDesignation(sf)),
      Col("Top CU", (sf: SF) => sf.symmetryAndFlatness.top_cu),
      Col("Bottom CU", (sf: SF) => sf.symmetryAndFlatness.bottom_cu),
      Col("Left CU", (sf: SF) => sf.symmetryAndFlatness.left_cu),
      Col("Right CU", (sf: SF) => sf.symmetryAndFlatness.right_cu),
      Col("Center CU", (sf: SF) => sf.symmetryAndFlatness.center_cu),
      Col("Baseline Top CU", (sf: SF) => sf.baseline.top_cu),
      Col("Baseline Bottom CU", (sf: SF) => sf.baseline.bottom_cu),
      Col("Baseline Left CU", (sf: SF) => sf.baseline.left_cu),
      Col("Baseline Right CU", (sf: SF) => sf.baseline.right_cu),
      Col("Baseline Center CU", (sf: SF) => sf.baseline.center_cu)

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

  override def getSopUID(data: SF): String = data.symmetryAndFlatness.SOPInstanceUID

  override def getOutput(data: SF): Output = data.output
}

object CsvSymmetryAndFlatness {

  def main(args: Array[String]): Unit = {
    DbSetup.init
    Trace.trace()
    val history = SymmetryAndFlatness.history(22)

    def show(h: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Unit = {
      val newAS_pct = h.symmetryAndFlatness.axialSymmetry
      val newTS_pct = h.symmetryAndFlatness.transverseSymmetry
      val newFL_pct = h.symmetryAndFlatness.flatness
      val newPC_pct = h.symmetryAndFlatness.profileConstancy(h.baseline)

      val oldAS_pct = h.symmetryAndFlatness.axialSymmetry_pct
      val oldTS_pct = h.symmetryAndFlatness.transverseSymmetry_pct
      val oldFL_pct = h.symmetryAndFlatness.flatness_pct
      val oldPC_pct = h.symmetryAndFlatness.profileConstancy_pct

    }

    history.map(h => show(h))
  }

}
