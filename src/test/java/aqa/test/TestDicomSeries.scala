
package aqa.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.ScalaUtil.Trace
import org.aqa.db.DbSetup
import org.aqa.DicomFile
import org.aqa.db.User
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.DicomSeries
import org.aqa.Util

/**
 * Test InputFiles.
 *
 */

class TestDicomSeries extends FlatSpec with Matchers {

  DbSetup.init

  val dicomDir = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\TestDicomSeries""")

  "insert" should "add entry" in {

    println("Reading DICOM test files ...")

    val alList = dicomDir.listFiles.map(f => (new DicomFile(f)).attributeList.get)
    println("Number of DICOM files in series: " + alList.size)

    val machine = Machine.list.head
    val input = Input.getByMachine(machine.machinePK.get).head
    val userPK = input.userPK.get

    val dicomSeriesBefore = DicomSeries.makeDicomSeries(userPK, input.inputPK, machine.machinePK, alList)

    val roundTrip = dicomSeriesBefore.attributeListList

    val sopBefore = alList.map(al => Util.sopOfAl(al)).mkString("\n")
    val sopAfter = roundTrip.map(al => Util.sopOfAl(al)).mkString("\n")

    (sopAfter) should be(sopBefore)

    val inserted = dicomSeriesBefore.insert

    val retrieved = DicomSeries.get(inserted.dicomSeriesPK.get).get

    val sopRetrieved = retrieved.attributeListList.map(al => Util.sopOfAl(al)).mkString("\n")

    (sopRetrieved) should be(sopBefore)

    DicomSeries.delete(inserted.dicomSeriesPK.get)
  }

}
