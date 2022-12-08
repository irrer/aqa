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

import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries

import java.io.File

/**
  * Get a DICOM file from the database.
  */
object SimpleDb {
  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")
    DbSetup.init

    val ds = DicomSeries.getBySeriesInstanceUID("1.3.6.1.4.1.22361.7091168952785.634753698.1569527528282.57")
    if (ds.isEmpty)
      Trace.trace("Could not find DICOM series")
    else {
      val alList = ds.head.attributeListList
      println("Number of attribute lists: " + alList.size)
      val outDir = new File("""D:\tmp\aqa\tmp\ds""")
      alList.foreach(al => {
        val file = new File(outDir, Util.sopOfAl(al) + ".dcm")
        DicomUtil.writeAttributeListToFile(al, file, "SimpleDb")
        Trace.trace("wrote DICOM file " + file.getAbsolutePath)
      })
    }

    Trace.trace("Done")
  }
}
