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

object PermuteDICOM {

    val dir = new File("""D:\tmp\velocity_crypt\AAAA\originals""")

    val outDir = new File(dir, "out")

    val tagList = List(
        TagFromName.ReferringPhysicianName,
        TagFromName.PatientName,
        TagFromName.PatientID,
        TagFromName.PatientBirthDate,
        TagFromName.PatientSex)

    def readDicom(dicomFile: File) = {
        val al = new AttributeList
        al.read(dicomFile)
        al
    }

    def alList = {
        dir.listFiles.toSeq.filter(f => f.getName.toLowerCase.endsWith(".dcm")).map(f => readDicom(f))
    }

    val seriesUid: String = {
        alList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrNull
    }

    def setAttr(al: AttributeList, tag: AttributeTag, text: String) = {
        val a = AttributeFactory.newAttribute(tag)
        a.addValue(text)
        al.put(a)
    }

    def modify(al: AttributeList, len: Int): AttributeList = {
        setAttr(al, TagFromName.OtherPatientIDs, len.formatted("%03d"))
        val ReferringPhysicianName = (0 until len).map(i => "A").mkString
        setAttr(al, TagFromName.ReferringPhysicianName, ReferringPhysicianName)
        setAttr(al, TagFromName.SeriesInstanceUID, seriesUid + "." + len)
        val instUid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull + "." + len
        setAttr(al, TagFromName.SOPInstanceUID, instUid)
        setAttr(al, TagFromName.MediaStorageSOPInstanceUID, instUid)
        al
    }

    def write(al: AttributeList) = {
        val transferSyntax = al.get(TagFromName.TransferSyntaxUID).getSingleStringValueOrDefault(TransferSyntax.ExplicitVRLittleEndian)
        val name =
            al.get(TagFromName.OtherPatientIDs).getSingleStringValueOrNull + "_" +
                al.get(TagFromName.InstanceNumber).getSingleStringValueOrNull.toInt.formatted("%03d") +
                ".dcm"
        val file = new File(outDir, name)
        file.delete
        al.write(file, transferSyntax, true, true)
    }

    def process(len: Int) = {
        println("len: " + len)
        val s = alList.map(al => modify(al, len))
        s.map(al => write(al))
    }

    def main(args: Array[String]): Unit = {
        println("starting")
        outDir.mkdirs()

        (1 until 70).map(len => process(len))

        println("finished")
    }

}