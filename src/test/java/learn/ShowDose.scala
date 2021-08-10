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

package learn

import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile

import java.awt.Rectangle
import java.io.File

/**
 * Quick program to show the center raw pixel values from DICOM files.
 */
object ShowDose {

  def main(args: Array[String]): Unit = {

    Trace.trace

    val oldFile = new File("""D:\tmp\aqa\tmp\hyejoo\old\RI.1.2.246.352.62.1.4974503712463805044.16509728268003719349.dcm""")
    val newFile = new File("""D:\tmp\aqa\tmp\hyejoo\new\RI.1.2.246.352.62.1.5705664162718992103.15031502956057050045.dcm""")

    def show(file: File): Unit = {
      val al = new DicomFile(file).attributeList.get
      val di = new DicomImage(al)
      val range = 5
      val middle = di.getSubimage(new Rectangle(di.width / 2 - range, di.height / 2 - range, range * 2, range * 2))

      def getValue(tag: AttributeTag): String = {
        val v = al.get(TagByName.RescaleSlope).getDoubleValues.head
        val name = TagByName.dict.getNameFromTag(tag)
        "    " + name + " " + tag.toString + " : " + v
      }

      val mean = middle.sum / (middle.width * middle.height)

      println(
        "file: " + file.getAbsolutePath + "    mean: " + mean + "\n" +
          getValue(TagByName.RescaleSlope) + getValue(TagByName.RescaleIntercept) +
          "\n" + middle.pixelsToText
      )
    }

    show(oldFile)
    show(newFile)

    System.exit(0)
  }

}
