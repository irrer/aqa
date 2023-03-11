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


package aqa.test

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities
import edu.umro.DicomDict.TagByName
import org.aqa.Util
import org.aqa.webrun.bbByCBCT.BBbyCBCTExecute
import org.aqa.webrun.bbByCBCT.BBbyCBCTRunReq
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import javax.vecmath.Point3d

/**
 * Test the BBbyCBCTExecute.measure function, which does the matrix operations needed to map
 * CBCT coordinates into RTPLAN coordinates.
 *
 */

class TestCbctCalculate extends FlatSpec with Matchers {
  "TestCbctCalculate" should "calculate CBCT BB in RTPLAN space" in {
    /** Main directory containing test data. */
    val mainDir = new File("""src\test\resources\TestCbctCalculate""")

    def readDicomFile(file: File): Option[AttributeList] = {
      try {
        if (DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
          // println("trying DICOM file: " + file.getAbsolutePath)
          val al = new AttributeList
          al.read(file)
          Some(al)
        }
        else
          None
      }
      catch {
        case _: Throwable =>
          println("Bad DICOM file: " + file.getAbsolutePath)
          None
      }
    }

    /**
     * Get the Z position in the attribute list.
     *
     * @param al Attribute list that must have a Z position.
     * @return The Z position in mm in CBCT space.
     */
    def zOf(al: AttributeList) = al.get(TagByName.ImagePositionPatient).getDoubleValues()(2)

    def getRunReq(dir: File): BBbyCBCTRunReq = {
      val allDicom = Util.listDirFiles(dir).flatMap(readDicomFile)
      val rtplan = allDicom.find(Util.isRtplan).get
      val reg = allDicom.find(Util.isReg).get
      val cbctList = allDicom.filter(Util.isCt).sortBy(zOf)
      val runReq = BBbyCBCTRunReq(rtplan, Some(reg), cbctList)
      runReq
    }

    /**
     * Look for a *.txt file containing the XYZ coordinates of the BB in CBCT space.  They
     * should be separated by white space and/or commas.
     *
     * @param dir
     * @return XYZ coordinates.
     */
    def getCoordinates(dir: File) = {
      val file = Util.listDirFiles(dir).find(f => f.getName.toLowerCase().endsWith(".txt")).get

      val text = Util.readTextFile(file).right.get
      val coordinateList = text.split("[ ,\n\r\t]").filter(_.length > 0).map(t => t.toDouble)

      val bbCenter = new Point3d(coordinateList.toArray)
      bbCenter
    }


    def testDir(dir: File): Unit = {
      println("Processing test directory: " + dir.getAbsolutePath)

      def runReq = getRunReq(dir)

      val coordinates = getCoordinates(dir)
      println("Using BB coordinates: " + coordinates)

      val pointInPlan = BBbyCBCTExecute.testCalculate(runReq, coordinates)

      println("Answer: " + pointInPlan)
    }

    println("Starting ...")
    (0 to 10).foreach(_ => println)
    if (mainDir.isDirectory)
      Util.listDirFiles(mainDir).foreach(testDir)
    else {
      println("Failure: no test data directory " + mainDir.getAbsolutePath)
      false should be(true)
    }

    true should be(true)
    println("Done.")
  }
}
