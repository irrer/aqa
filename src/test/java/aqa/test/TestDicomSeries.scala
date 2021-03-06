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

package aqa.test

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.FileUtil
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries
import org.aqa.db.Input
import org.aqa.db.InputFiles
import org.aqa.db.Procedure
import org.aqa.db.User
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import java.sql.Timestamp
import scala.util.Try

/**
  * Test InputFiles.
  */

class TestDicomSeries extends FlatSpec with Matchers {

  DbSetup.init

  //val dicomDir = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\TestDicomSeries""")
  val dicomDir = new File("""src\test\resources\TestDicomSeries""")

  "insert" should "add entry" in {

    println("Reading DICOM test files ...")

    val alRtplanAndImageSeq = dicomDir.listFiles.toSeq.map(f => new DicomFile(f).attributeList.get).partition(al => Util.modalityOfAl(al).equalsIgnoreCase("RTPLAN"))
    val rtplan = alRtplanAndImageSeq._1.head
    val imageList = alRtplanAndImageSeq._2

    println("Number of image slices: " + imageList.size)

    val userPK = User.list.head.userPK.get

    var dsRtplan: Option[DicomSeries] = None
    var dsImage: Option[DicomSeries] = None
    val imageSopClass = imageList.head.get(TagFromName.SOPClassUID).getSingleStringValueOrNull

    val input = {
      Some(new Input(inputPK = None, directory = Some(dicomDir.getAbsolutePath), uploadDate = new Timestamp(0), userPK = Some(userPK), machinePK = None, patientId = None, dataDate = None).insert)
    }

    val inpPK = input.get.inputPK.get
    println("X inputPK: " + inpPK) // X denotes item to be deleted

    /**
      * remove all relevant data from database
      */
    def cleanup = {
      Try {
        val dicomSeries = DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(rtplan)) ++ DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(imageList.head))

        dicomSeries.map(ds => DicomSeries.delete(ds.dicomSeriesPK.get))
        dicomSeries.flatMap(ds => ds.inputPK).distinct.map(inputPK => Input.delete(inputPK))
      }
    }

    cleanup

    try {
      val zippedContent = FileUtil.readFileTreeToZipByteArray(Seq(dicomDir))
      val inputFiles = new InputFiles(inpPK, inpPK, zippedContent)
      inputFiles.insert
      println("X inputFiles.inputFilesPK: " + inputFiles.inputFilesPK) // X denotes item to be deleted

      val procedurePK = Procedure.ProcOfPhase2.get.procedurePK
      dsRtplan = Some(DicomSeries.makeDicomSeries(userPK, Some(inpPK), None, Seq(rtplan), procedurePK).get.insert)
      dsImage = Some(DicomSeries.makeDicomSeries(userPK, Some(inpPK), None, imageList, procedurePK).get.insert)
      println("X dsRtplan.dicomSeriesPK: " + dsRtplan.get.dicomSeriesPK.get) // X denotes item to be deleted
      println("X dsImage.dicomSeriesPK: " + dsImage.get.dicomSeriesPK.get) // X denotes item to be deleted

      val dsRtplan2 = DicomSeries.get(dsRtplan.get.dicomSeriesPK.get).get
      val dsImage2 = DicomSeries.get(dsImage.get.dicomSeriesPK.get).get

      // check to see that the RTPLAN data is as expected by round-tripping the data
      dsRtplan2.inputPK.isEmpty should be(true)
      val rtplanAlList = dsRtplan2.attributeListList
      rtplanAlList.size should be(1)
      Util.sopOfAl(rtplanAlList.head) should be(Util.sopOfAl(rtplan))

      // check to see that the IMAGE data is as expected by round-tripping the data
      dsImage2.inputPK.get should be(inpPK)
      val imageAlList = dsImage.get.attributeListList
      imageAlList.size should be(imageList.size)
      Util.sopOfAl(imageAlList.head) should be(Util.sopOfAl(imageList.head))

      // test the get functions for rtplan
      DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(rtplan)).size should be(1)
      DicomSeries.getBySopInstanceUID(Util.sopOfAl(rtplan)).size should be(1)
      rtplan.get(TagFromName.FrameOfReferenceUID) match {
        case at: Attribute => DicomSeries.getByFrameUIDAndSOPClass(Set(at.getSingleStringValueOrNull), SOPClass.RTPlanStorage).size should be(1)
        case _             => ;
      }

      // test the get functions for an image series
      imageList.map(al => {
        DicomSeries.getBySeriesInstanceUID(Util.serInstOfAl(al)).size should be(1)
        DicomSeries.getBySopInstanceUID(Util.sopOfAl(al)).size should be(1)
        DicomSeries.getByFrameUIDAndSOPClass(Set(Util.getFrameOfRef(al)), imageSopClass).size should be(1)
      })

      // remove the input both as clean up and for testing
      Input.delete(inpPK) should be(1)

      // check to see that rows were deleted
      Input.get(inpPK).isEmpty should be(true)
      InputFiles.get(inputFiles.inputFilesPK).isEmpty should be(true)
      DicomSeries.get(dsRtplan.get.dicomSeriesPK.get).isDefined should be(true) // not deleted
      DicomSeries.get(dsImage.get.dicomSeriesPK.get).isEmpty should be(true)

      // clean up plan.. This is not done automatically when input is deleted like everything else.
      println("Deleting dsRtplan " + dsRtplan.get.dicomSeriesPK.get)
      DicomSeries.delete(dsRtplan.get.dicomSeriesPK.get) should be(1)

    } catch {
      case t: Throwable =>
        println("Unexpected exception: " + t)
        t.printStackTrace()
        if (input.isDefined) {
          println("Cleaning up input " + input.get.inputPK.get)
          Input.delete(inpPK)
        }
        if (dsRtplan.isDefined) {
          val ds = DicomSeries.get(dsRtplan.get.dicomSeriesPK.get)
          if (ds.isDefined) {
            val pk = ds.get.dicomSeriesPK.get
            println("Cleaning up dsRtplan " + pk)
            DicomSeries.delete(pk)
          }
        }
        if (dsImage.isDefined) {
          val ds = DicomSeries.get(dsImage.get.dicomSeriesPK.get)
          if (ds.isDefined) {
            val pk = ds.get.dicomSeriesPK.get
            println("Cleaning up dsImage " + pk)
            DicomSeries.delete(pk)
          }
        }
        cleanup
        true should be(false) // force test to fail
    }
  }

}
