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
import com.pixelmed.dicom.XMLRepresentationOfDicomObjectFactory
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File

object DicomXML {

  val fixSize = 500
  def prnt(t: String): Unit = {
    println(t.take(fixSize) + "\n...\n" + t.takeRight(fixSize))
  }

  def main(args: Array[String]): Unit = {

    try {
      Trace.trace
      val al = new AttributeList
      //val file = new File("""D:\tmp\aqa\CBCT\MQATX3OBIQA2019Q3\CT.1.2.246.352.63.1.4612301178611931584.17686074523525590964.dcm""")
      val file = new File("""D:\tmp\aqa\CBCT\MQATX3OBIQA2019Q3\RI.1.2.246.352.81.3.4034627062.56026.17761.135.155.dcm""")

      al.read(file)

      al.remove(TagFromName.PixelData)
      Util.writeAttributeListToFile(al, new File("""D:\tmp\DicomXML.dcm"""), "DicomXML")

      val baos = new ByteArrayOutputStream

      XMLRepresentationOfDicomObjectFactory.createDocumentAndWriteIt(al, baos)
      baos.close()

      val text = new String(baos.toByteArray)

      prnt(text)
      val factory = new XMLRepresentationOfDicomObjectFactory

      val bais = new ByteArrayInputStream(baos.toByteArray)

      val al2 = factory.getAttributeList(bais)

      prnt(al2.toString)

      //      val text2 = text.replaceAll("<.xml[^>]*>", "").trim
      //      prnt(text2)
      //      Trace.trace
      //
      //      val doc = XML.loadString(text)
      //      val docText = new scala.xml.PrettyPrinter(1024, 2).format(doc)
      //      Trace.trace(docText)

      Trace.trace

      println("done")
    } catch {
      case t: Throwable =>
        println("badness: " + t.toString.take(500))
        t.printStackTrace()
    }
  }

}
