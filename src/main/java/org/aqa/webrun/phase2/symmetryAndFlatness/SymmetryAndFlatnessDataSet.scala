package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness

/**
 * Represent data needed to generate report for symmetry, flatness, and constancy
 *
 * @param symmetryAndFlatness Readings for a single beam.
 * @param output Output referenced by <code>symmetryAndFLatness</code>
 * @param baseline Baseline for this beam.  May be the same as <code>symmetryAndFLatness</code>
 * @param al Attribute list for this DICOM RTIMAGE.
 */
case class SymmetryAndFlatnessDataSet(symmetryAndFlatness: SymmetryAndFlatness, output: Output, baseline: SymmetryAndFlatness, al: AttributeList, rtplan: AttributeList) {
  val time: Long = Util.getOutputDateTime(Seq(al)).get
}
