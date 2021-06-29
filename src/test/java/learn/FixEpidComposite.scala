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

import org.aqa.db.Db
import Db.driver.api._
import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.db.BBbyEPIDComposite
import edu.umro.ScalaUtil.Trace
import org.aqa.db.BBbyEPID
import org.aqa.db.BBbyCBCT

object FixEpidComposite {

  def listEpid: Seq[BBbyEPID] = {
    Db.run(BBbyEPID.query.result)
  }

  def listCbct: Seq[BBbyCBCT] = {
    Db.run(BBbyCBCT.query.result)
  }

  def listComposite: Seq[BBbyEPIDComposite] = {
    Db.run(BBbyEPIDComposite.query.result)
  }

  def main(args: Array[String]): Unit = {
    DbSetup.init

    Trace.trace
    val repList = BBbyEPIDComposite.getReportingDataSet(2.toLong)
    Trace.trace("size: " + repList.size)

    if (false) {
      val allComposite = listComposite
      Trace.trace
      Trace.trace("size: " + allComposite.size)
      Trace.trace

      val allEpid = listEpid
      Trace.trace
      Trace.trace("size: " + allEpid.size)
      Trace.trace

      val allCbct = listCbct
      Trace.trace
      Trace.trace("size: " + allCbct.size)
      Trace.trace
    }
  }
}
