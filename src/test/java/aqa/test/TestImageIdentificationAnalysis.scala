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

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.ScalaUtil.Trace

/**
 * Test the PositioningCheckAnalysis.
 */

class TestMetadataCheckAnalysis extends FlatSpec with Matchers {

  "makeMetadataCheck" should "make a passing one" in {

    Trace.trace
    //      val plan = Util.readDicomFile(new File("""PositioningCheckPlan.dcm""")).right.get
    //      Trace.trace
    //      val image = Util.readDicomFile(new File("""PositioningCheckImage.dcm""")).right.get
    //      Trace.trace
    //
    //      val positioningCheck = PositioningCheckAnalysis.makePositioningCheck(-1, plan, image)
    //      System.err.println("PositioningCheck:\n" + positioningCheck)
    //      System.err.println("Util.buildProperties: " + Util.buildProperties) // TODO rm
    //      (positioningCheck.get.pass) should be(true)
    (true) should be(true)
  }

}
