
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqa.db.Machine
import org.aqa.db.DbSetup
import org.aqa.webrun.phase2.customizeRtPlan.CustomizeRtPlan

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
    val machine = new Machine(None, "id", Some("id_real"), 1, None, mlcPK, -1, -1, None, false, false, false, false, false, true, "notes")

    val list = CustomizeRtPlan.testGetPlanBeamList(machine)

    (list.size) should be(22)
    (list.filter(_.fff).size) should be(2)
    true should be(true)
  }
}
