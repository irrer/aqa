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


package aqa.test;

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis
import org.scalactic.TolerantNumerics
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
 * Test Sym+Flat function.
 *
 */

class TestSymmetryAndFlatnessAnalysis extends FlatSpec with Matchers {

  "SymAndFlat" should "show point values" in {

    Trace.trace("Config.validated: " + Config.validated)
    Trace.trace("Starting")

    val dir = new File("""src\test\resources""")
    val file = new File(dir, """SymmetryAndFlatness_J20G0-10F.dcm""")
    Trace.trace("Using file " + file)

    val attributeList = new AttributeList
    attributeList.read(file)
    val dicomImage = new DicomImage(attributeList)

    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    /*  TODO this should be re-implemented
    val pointSet = SymmetryAndFlatnessAnalysis.testMakePointSet(dicomImage, attributeList, RescaleSlope, RescaleIntercept)

    Trace.trace("values: " + pointSet)

    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)

    (pointSet.top === 0.156552) should be(true)
    (pointSet.bottom === 0.155609) should be(true)
    (pointSet.right === 0.156309) should be(true)
    (pointSet.left === 0.156356) should be(true)
    (pointSet.center === 0.224504) should be(true)
     */
    Trace.trace
  }

}
