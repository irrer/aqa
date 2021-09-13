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

import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test DicomSeries.insertIfNew.  Verify that it allows series to have slices added to them after the fact.
  */

class TestDicomSeries_insertIfNew extends FlatSpec with Matchers {

  println("Starting...")
  Config.validate
  DbSetup.init

  val dir = new File("""src\test\resources\TestDicomSeries_insertIfNew""")

  private def testRtplan() = {

    val rtplanDir = new File(dir, "RTPLAN")
    rtplanDir.isDirectory should be(true)

    val alList = Util.listDirFiles(rtplanDir).map(f => new DicomFile(f).attributeList.get)
    val al1 = alList.head
    val al2 = alList(1)

    val seriesUid = Util.serInstOfAl(al1)

    // delete DicomSeries from a previous test if they exist
    def cleanup() = {
      val list = DicomSeries.getBySeriesInstanceUID(seriesUid)
      println("Number of DicomSeries to clean up: " + list.size)
      list.map(ds => DicomSeries.delete(ds.dicomSeriesPK.get))
      DicomSeries.getBySeriesInstanceUID(seriesUid).size should be(0)
    }

    cleanup()

    val templateDs = DicomSeries.getRandomForTesting.get

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, Seq(al1))
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(1)
      ds.head.sopUidSeq.head should be(Util.sopOfAl(al1))
      ds.head.content.nonEmpty should be(true)
    }

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, Seq(al1))
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(1)
      ds.head.sopUidSeq.head should be(Util.sopOfAl(al1))
      ds.head.content.nonEmpty should be(true)
    }

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, Seq(al2))
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(2)
      ds.head.sopUidSeq.contains(Util.sopOfAl(al1)) should be(true)
      ds.head.sopUidSeq.contains(Util.sopOfAl(al2)) should be(true)
      ds.head.content.nonEmpty should be(true)
    }

    cleanup()
  }

  private def testCt() = {

    val ctDir = new File(dir, "CT")
    ctDir.isDirectory should be(true)

    val alList = Util.listDirFiles(ctDir).map(f => new DicomFile(f).attributeList.get)
    val alA = alList.take(3)
    val alB = Seq(alList(1), alList(5), alList(6))
    val alC = alList.drop(3)

    val seriesUid = Util.serInstOfAl(alList.head)

    // delete DicomSeries from a previous test if they exist
    def cleanup() = {
      val list = DicomSeries.getBySeriesInstanceUID(seriesUid)
      println("Number of DicomSeries to clean up: " + list.size)
      list.map(ds => DicomSeries.delete(ds.dicomSeriesPK.get))
      DicomSeries.getBySeriesInstanceUID(seriesUid).size should be(0)
    }

    cleanup()

    val templateDs = DicomSeries.getRandomForTesting.get

    // insert the DICOM series in stages, a few slices at a time

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, alA)
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(3)
      val dbSop = ds.head.sopUidSeq.sorted.mkString("  ")
      val expectedSop = alA.map(Util.sopOfAl).distinct.sorted.mkString("  ")
      dbSop should be(expectedSop)
      ds.head.content.isEmpty should be(true)
    }

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, alA)
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(3)
      val dbSop = ds.head.sopUidSeq.sorted.mkString("  ")
      val expectedSop = alA.map(Util.sopOfAl).distinct.sorted.mkString("  ")
      dbSop should be(expectedSop)
      ds.head.content.isEmpty should be(true)
    }

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, alB)
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(5)
      val dbSop = ds.head.sopUidSeq.sorted.mkString("  ")
      val expectedSop = (alA ++ alB).map(Util.sopOfAl).distinct.sorted.mkString("  ")
      dbSop should be(expectedSop)
      ds.head.content.isEmpty should be(true)
    }

    if (true) {
      DicomSeries.insertIfNew(templateDs.userPK, templateDs.inputPK, templateDs.machinePK, alC)
      val ds = DicomSeries.getBySeriesInstanceUID(seriesUid)
      ds.size should be(1)
      ds.head.size should be(10)
      val dbSop = ds.head.sopUidSeq.sorted.mkString("  ")
      val expectedSop = (alA ++ alC).map(Util.sopOfAl).distinct.sorted.mkString("  ")
      dbSop should be(expectedSop)
      ds.head.content.isEmpty should be(true)
    }

    cleanup()
  }

  "insertIfNew" should "be maintaining the superset of DICOM files" in {
    testRtplan()
    testCt()
  }
  println("Done")
}
