
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.InputFiles
import org.aqa.db.DbSetup
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.db.Output
import org.aqa.db.Input
import org.aqa.db.Db
import org.aqa.run.ProcedureStatus
import slick.driver.PostgresDriver.api._

/**
 * Test InputFiles.
 *
 */

class TestLOCBaselineCurrent extends FlatSpec with Matchers {

  DbSetup.init

  "insert" should "add entry" in {

    def latestLOCBaselineDir(machinePK: Long, webInterface: String): Seq[(Input, Output)] = {

      val search = for {
        machine <- Machine.query.filter(m => m.machinePK === machinePK)
        procedure <- Procedure.query.filter(p => p.webInterface === webInterface)
        output <- Output.query.filter(o => (o.procedurePK === procedure.procedurePK) && (o.status === ProcedureStatus.done.toString))
        input <- Input.query.filter(i => i.inputPK === output.inputPK)
      } yield ((input, output))

      val sorted = search.sortBy(_._2.startDate)

      val list = Db.run(sorted.result)
      list
    }

    Machine.list.map(machine => {
      val list = latestLOCBaselineDir(machine.machinePK.get, "LOCUploadBaseFiles_1")
      println("\n==== machine: " + machine.machinePK.get + " : " + machine.id)
      println(list.map(io => io._2.startDate) mkString ("\n====    ", "\n====    ", "\n====    "))
    })

    true should be(true)
  }

}
