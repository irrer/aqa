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
import java.nio.charset.Charset
import java.nio.charset.Charset
import java.io.OutputStreamWriter
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import java.io.InputStreamReader
import java.io.ByteArrayOutputStream
import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.SequenceItem
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.DicomDict.TagByName

//import java.nio.charset.StandardCharsets

object Windows1252Encoding {
  import sun.nio.cs.StandardCharsets

  def main(args: Array[String]): Unit = {
    println("starting")

    if (true) {
      val file = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\main\resources\static\rtplan_hdmlc.dcm""")
      val df = new DicomFile(file)
      val al = df.attributeList.get

      if (true) {
        val attr = al.get(TagByName.BeamSequence)
        Trace.trace(attr.getClass.getName)
        val seq = attr.asInstanceOf[SequenceAttribute]

        val beamSeq = DicomUtil.seqToAttr(al, TagByName.BeamSequence)

        val keep = beamSeq.take(2)
        val newSeq = new SequenceAttribute(TagByName.BeamSequence) //AttributeFactory.newAttribute(TagFromName.BeamSequence)

        keep.map(k => newSeq.addItem(k))

        al.put(newSeq)

        Trace.trace("New BeamSequence:\n" + DicomUtil.attributeListToString(al))

        Trace.trace
        System.exit(99)
      }

      val tag = new AttributeTag(0x3253, 0x1000)
      println("tag: " + tag)

      val ExtendedInterfaceData = al.get(tag).getByteValues

      val eidText = new String(ExtendedInterfaceData)
      println("eidText: " + eidText)
      println("ExtendedInterfaceData size: " + ExtendedInterfaceData.size)
      println("ExtendedInterfaceData: " + ExtendedInterfaceData.toList.mkString(" "))

      val ia = ExtendedInterfaceData.map(e => e.toInt).toList
      println
      println("ia: " + ia.mkString(" "))
      println
      println("ia size: " + ia.size)
      println
      def show(i: Int) = {
        println(" " + i)
      }
      ia.map(i => show(i))
      println
    }
    println("\n----------------")
    println
    //System.exit(0)

    val text2 = "aaaaa 12345>><<"
    val text = """ma�ana"""

    println("text as bytes: " + text.getBytes.toList.map(b => (b & 0xff).formatted("%3x")).mkString("  "))
    val in = new ByteArrayInputStream(text.getBytes)

    val bo = new ByteArrayOutputStream
    bo.write(text.getBytes)

    val out = new OutputStreamWriter(bo, "windows-1252")

    out.close
    val windows1252Bytes = bo.toByteArray

    println(windows1252Bytes.mkString(" "))

    println("finished")
  }

}
