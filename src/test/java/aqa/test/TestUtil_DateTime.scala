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


package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import java.text.SimpleDateFormat
import java.util.Date

/**
 * Test the Config.
 *
 */

class TestUtil_DateTime extends FlatSpec with Matchers {

  val dateTimePairList = Seq(
    ("20190124", "110000.123456"),
    ("20190124", "110000.12345"),
    ("20190124", "110000.1234"),
    ("20190124", "110000.123"),
    ("20190124", "110000.12"),
    ("20190124", "110000.1"),
    ("20190124", "110000."),
    ("20190124", "110000"),
    ("20190724", "110000.123456"),
    ("20190724", "110000.12345"),
    ("20190724", "110000.1234"),
    ("20190724", "110000.123"),
    ("20190724", "110000.12"),
    ("20190724", "110000.1"),
    ("20190724", "110000."),
    ("20190724", "110000"),
    ("20200229", "110000"))

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  val dicomFormat = new SimpleDateFormat("yyyyMMdd HHmmss.SSS")

  private def check(dateText: String, timeText: String): Unit = {
    val contentDate = AttributeFactory.newAttribute(TagFromName.ContentDate)
    contentDate.addValue(dateText)

    val contentTime = AttributeFactory.newAttribute(TagFromName.ContentTime)
    contentTime.addValue(timeText)

    val al = new AttributeList
    al.put(contentDate)
    al.put(contentTime)

    val dateTime = Util.extractDateTimeAndPatientIdFromDicomAl(al)

    def toText(t: Date) = {
      val orig = "orig: " + (dateText + " " + timeText).format("%-26s")
      val ms = "ms: " + t.getTime.formatted("%14d")
      val humanReadable = "human readable:" + dateFormat.format(t)
      val out = " out: " + dicomFormat.format(t)

      orig + "   ==> " + ms + "   " + humanReadable + "   " + "\n" + out + "\n"

    }

    def isOk(t: Date) = {
      val before = (dateText + " " + timeText).take(19)
      val after = dicomFormat.format(t).take(before.size)
      before should be(after)
    }

    dateTime._1.map(t => {
      println(toText(t))
      isOk(t)
    })
  }

  dateTimePairList.map(dt => check(dt._1, dt._2))

  true should be(true)

}
