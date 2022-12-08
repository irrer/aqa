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


import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TagFromName

object PixelRow {

  def main(args: Array[String]): Unit = {
    println("starting")

    val file = new File("""D:\downloads\proffy.dcm""")

    val al = new AttributeList
    al.read(file)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val pixelData = al.get(TagFromName.PixelData).getShortValues

    val bound = 40
    val x = 395
    val y = 718

    val lo = (width * y) + x - bound
    val hi = (width * y) + x + bound

    def show(p: Int) = {
      println(pixelData(p))
    }

    (lo to hi).map(p => show(p))

    println("finished")
  }

}