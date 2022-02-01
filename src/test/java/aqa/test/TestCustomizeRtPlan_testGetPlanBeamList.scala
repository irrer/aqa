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

import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.db.Machine
import org.aqa.webrun.phase2.customizeRtPlan.CustomizeRtPlan
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
  * Test CustomizeRtPlan.testGetPlanBeamList.  Note that this test is not real portable because it depends on a machine, collimator, and plan being set up as it expects.
  *
  */

class TestCustomizeRtPlan_testGetPlanBeamList extends FlatSpec with Matchers {

  Config.validate
  DbSetup.init

  "CustomizeRtPlan.testGetPlanBeamList" should "get list of plan beams" in {
    println("Starting...")
    val mlcPK = 3.toLong // hard coded 3 for PK makes code brittle
    val machine = new Machine(None, "id", Some("id_real"), 1, None, mlcPK, -1, -1, None, false, false, false, false, false, true, tpsID_real = None, "notes")

    val list = CustomizeRtPlan.testGetPlanBeamList(machine)

    list.size should be(22)
    list.count(_.fff) should be(2)
    true should be(true)
  }
}
