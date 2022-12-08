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

import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import org.aqa.DicomFile

import java.io.File

object RtImageCU {

  def dump(file: File): Unit = {
    val df = new DicomFile(file)
    val al = df.attributeList.get
    val di = new DicomImage(al)
    val mid = di.height / 2
    val slope = al.get(TagFromName.RescaleSlope).getDoubleValues.head
    val intercept = al.get(TagFromName.RescaleIntercept).getDoubleValues.head

    println(
      file.getAbsolutePath +
        "  min: " + di.minPixelValue +
        "  max: " + di.maxPixelValue +
        "  slope: " + slope +
        "  intercept: " + intercept
    )
    val text = (0 until di.width).map(i => di.get(i, mid).round.formatted("%8d")).mkString("")
    println(text)
  }

  def main(args: Array[String]): Unit = {
    val dirList = Seq(
      """D:\tmp\aqa\tmp\BR1dailyQA""",
      """D:\tmp\aqa\tmp\BR1phase2"""
    ).map(n => new File(n))

    dirList.foreach(d => {
      val fileList = d.listFiles()
      fileList.foreach(dump)
    })
  }

}
