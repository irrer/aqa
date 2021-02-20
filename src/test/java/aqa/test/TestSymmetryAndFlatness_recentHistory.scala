
package aqa.test

;

import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.SymmetryAndFlatness
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.sql.Timestamp
import java.util.Date

/**
 * Test the TestCenter_recentHistory method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 */

class TestSymmetryAndFlatness_recentHistory extends FlatSpec with Matchers {

  DbSetup.init

  val machinePK: Long = 22.toLong
  val procedurePK = 4
  val beamName = "J20G0-10X"
  val dateText = "2018-05-30T00:00:00.000"
  val date: Date = Util.standardDateFormat.parse(dateText)
  val dateTime = new Timestamp(date.getTime)

  println("\nStarting. Using date of:\n    " + Util.standardDateFormat.format(date))
  val seq: Seq[SymmetryAndFlatness.SymmetryAndFlatnessHistory] = SymmetryAndFlatness.history(machinePK, beamName)

  println("results:\n    " + seq.map(_.timestamp).mkString("\n    "))
  println("Number of results: " + seq.size)

  seq.size should be(4)
  println("Done")
}
