
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.DicomFile
import org.aqa.db.SymmetryAndFlatness
import java.sql.Timestamp
import org.aqa.db.DbSetup
import java.util.Date

/**
 * Test the TestCenter_recentHistory method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 */

class TestSymmetryAndFlatness_recentHistory extends FlatSpec with Matchers {

  DbSetup.init

  val machinePK = 22.toLong
  val procedurePK = 4
  val beamName = "J20G0-10X"
  val dateText = "2018-05-30T00:00:00.000"
  val date = Util.standardDateFormat.parse(dateText)
  val dateTime = new Timestamp(date.getTime)

  println("\nStarting. Using date of:\n    " + Util.standardDateFormat.format(date))
  val seq = SymmetryAndFlatness.history(machinePK, procedurePK, beamName)

  println("results:\n    " + seq.map(_.date).mkString("\n    "))
  println("Number of results: " + seq.size)

  seq.size should be(4)
  println("Done")
}
