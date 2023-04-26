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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.DicomFile
import org.aqa.Util

import java.io.File

object MakeCBCT {

  def main(args: Array[String]): Unit = {

    // offsets from center of volume
    val xOffset = 10
    val yOffset = 20
    val zOffset = 4

    val inDir = new File("""src\test\resources\TestCBCTAlign\TX2_CT_1""")
    println("Using input directory: " + inDir.getAbsolutePath)
    val outDir = new File("""src\test\resources\TestCBCTAlign\CBCT_ONE_VOXEL""")
    outDir.mkdirs
    outDir.listFiles.map(f => f.delete)

    def getPosn(al: AttributeList): Double = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

    val attrListSeq = inDir.listFiles.map(f => new DicomFile(f).attributeList.get).toSeq.sortBy(al => getPosn(al))

    val xSize = attrListSeq.head.get(TagFromName.Columns).getIntegerValues()(0)
    val ySize = attrListSeq.head.get(TagFromName.Rows).getIntegerValues()(0)
    val zSize = attrListSeq.size

    val x = (xSize / 2) + xOffset
    val y = (ySize / 2) + yOffset
    val z = (zSize / 2) + zOffset

    def makeFile(index: Int): Unit = {
      val al = attrListSeq(index)

      val pixAttr = al.get(TagFromName.PixelData) //.asInstanceOf[OtherWordAttribute]
      val pixels = pixAttr.getShortValues
      (0 until pixels.size).foreach(i => pixels(i) = 100.toShort)
      //      Trace.trace(pixels.size)
      //      val empty = Array.fill(pixels.size)(1000.toShort)
      if (index == (z - 1)) {
        val columns = al.get(TagFromName.Columns).getIntegerValues.head
        pixels((y * columns) + x) = 10000.toShort
      }
      //      pixAttr.setValues(empty)

      val name = ("CBCT_" + (index + 1).formatted("%3d") + ".dcm").replace(' ', '_')
      val file = new File(outDir, name)
      Util.writeAttributeListToFile(al, file)
      println("wrote: " + file.getAbsolutePath)
    }

    attrListSeq.indices.foreach(index => makeFile(index))

  }

}