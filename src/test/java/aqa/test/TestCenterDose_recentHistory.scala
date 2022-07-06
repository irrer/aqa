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
import org.aqa.db.CenterDose
import org.aqa.db.DbSetup
import org.aqa.db.Procedure
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.sql.Timestamp

/**
 * Test the TestCenter_recentHistory method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 *
 */

class TestCenterDose_recentHistory extends FlatSpec with Matchers {

  DbSetup.init

  val machinePK = 22.toLong
  val procedurePK = 4
  val date = new Timestamp(Util.standardDateFormat.parse("2018-05-30T00:00:00.000").getTime)

  println("Starting")
  val list = CenterDose.history(machinePK, Procedure.ProcOfPhase2.get.procedurePK.get)

  println("results:\n    " + list.mkString("\n    "))
  println("Number of results: " + list.size)

  true should be(true)

}
