
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
import org.aqa.db.CenterDose
import java.sql.Timestamp
import org.aqa.db.DbSetup
import java.util.Date

/**
 * Test the TestCenter_recentHistory method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 *
 */

class TestCenterDose_recentHistory extends FlatSpec with Matchers {

  DbSetup.init

  val machinePK = 22.toLong
  val procedurePK = 4
  val date = new Timestamp(Util.standardDateFormat.parse("2018-05-30T00:00:00.000").getTime)

  println("Starting")
  val list = CenterDose.history(machinePK, procedurePK)

  println("results:\n    " + list.mkString("\n    "))
  println("Number of results: " + list.size)

  true should be(true)

}
