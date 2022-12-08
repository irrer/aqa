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
import org.aqa.DicomFile
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ImageUtil.DicomImage

object CollimatorPositionError {

  def main(args: Array[String]): Unit = {
    println("starting")

    val file = new File("""D:\pf\eclipse\workspaceOxygen\aqa\target\CollimatorPositionError.dcm""")
    val df = new DicomFile(file)
    val di = new DicomImage(df.attributeList.get)

    val cols = di.columnSums

    println("\n" + cols.mkString("\n"))
    
    

    println("finished")
  }

}