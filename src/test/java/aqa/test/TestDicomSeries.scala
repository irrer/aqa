
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
import java.sql.Timestamp
import org.aqa.db.InputFiles
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.FileUtil

/**
 * Test InputFiles.
 */

class TestDicomSeries extends FlatSpec with Matchers {

  DbSetup.init

  //val dicomDir = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\TestDicomSeries""")
  val dicomDir = new File("""src\test\resources\TestDicomSeries""")

  "insert" should "add entry" in {

    println("Reading DICOM test files ...")

    val alPlanImage = dicomDir.
      listFiles.
      toSeq.
      map(f => (new DicomFile(f)).attributeList.get).
      partition(al => Util.modalityOfAl(al).equalsIgnoreCase("RTPLAN"))
    val rtplan = alPlanImage._1.head
    val imageList = alPlanImage._2

    val userPK = User.list.head.userPK.get

    val input = {
      new Input(
        inputPK = None,
        directory = Some(dicomDir.getAbsolutePath),
        uploadDate = new Timestamp(0),
        userPK = None,
        machinePK = None,
        patientId = None,
        dataDate = None).insert
    }

    val inpPK = input.inputPK.get
    println("X inputPK: " + inpPK) // X denotes item to be deleted

    val zippedContent = FileUtil.readFileTreeToZipByteArray(Seq(dicomDir))
    val inputFiles = (new InputFiles(inpPK, inpPK, zippedContent)).insert
    println("X inputFiles.inputFilesPK: " + inputFiles.inputFilesPK) // X denotes item to be deleted

    val dsRtplan = (DicomSeries.makeDicomSeries(userPK, Some(inpPK), None, Seq(rtplan)).get).insert
    val dsImage = (DicomSeries.makeDicomSeries(userPK, Some(inpPK), None, imageList).get).insert
    println("X dsRtplan.dicomSeriesPK: " + dsRtplan.dicomSeriesPK.get) // X denotes item to be deleted
    println("X dsImage.dicomSeriesPK: " + dsImage.dicomSeriesPK.get) // X denotes item to be deleted

    val dsRtplan2 = DicomSeries.get(dsRtplan.dicomSeriesPK.get).get
    val dsImage2 = DicomSeries.get(dsImage.dicomSeriesPK.get).get

    // check to see that the RTPLAN data is as expected
    dsRtplan2.inputPK.isEmpty should be(true)
    val rtplanAlList = dsRtplan2.attributeListList
    rtplanAlList.size should be(1)
    Util.sopOfAl(rtplanAlList.head) should be(Util.sopOfAl(rtplan))

    // check to see that the IMAGE data is as expected
    dsImage2.inputPK should be(inpPK)
    val imageAlList = dsImage.attributeListList
    imageAlList.size should be(imageList.size)
    Util.sopOfAl(imageAlList.head) should be(Util.sopOfAl(imageList.head))

    // remove the input both as clean up and for testing
    Input.delete(inpPK) should be(1)

    // check to see that rows were deleted
    Input.get(inpPK).isEmpty should be(true)
    InputFiles.get(inputFiles.inputFilesPK).isEmpty should be(true)
    DicomSeries.get(dsRtplan.dicomSeriesPK.get).isDefined should be(true) // not deleted
    DicomSeries.get(dsImage.dicomSeriesPK.get).isEmpty should be(true)

    // clean up plan.. This is not done automatically when input is deleted like everything else.
    DicomSeries.delete(dsRtplan.dicomSeriesPK.get) should be(1)

  }

}
