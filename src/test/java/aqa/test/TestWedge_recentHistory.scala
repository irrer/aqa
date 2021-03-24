
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import org.aqa.db.WedgePoint
import java.sql.Timestamp
import org.aqa.db.DbSetup
import java.util.Date

/**
 * Test the TestCenter_recentHistory method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 */

class TestWedge_recentHistory extends FlatSpec with Matchers {

  DbSetup.init

  val limit = 2
  val machinePK = 22.toLong
  val procedurePK = 4
  val dateText = "2018-05-30T00:00:00.000"
  val date = Util.standardDateFormat.parse(dateText)
  val dateTime = new Timestamp(date.getTime)

  println("\nStarting. Using date of:\n    " + Util.standardDateFormat.format(date))
  val seq = WedgePoint.recentHistory(machinePK)

  println("results:\n    " + seq.map(_.date).mkString("\n    "))
  println("Number of results: " + seq.size)

  seq.size should be(4)
  println("Done")
}
