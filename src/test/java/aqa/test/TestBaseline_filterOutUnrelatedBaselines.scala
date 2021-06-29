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

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqa.db.DbSetup
import org.aqa.db.Baseline

/**
 * Test the TestBaseline.filterOutUnrelatedBaselines method.
 *
 * This is highly dependent on the contents of the database on Jim's desktop.
 *
 */

class TestBaseline_filterOutUnrelatedBaselines extends FlatSpec with Matchers {

  DbSetup.init

  (0 to 10).map(_ => println)
  println("Starting")
  val maintRecPKset = (1 to 100).toList.map(i => i.toLong).toSet
  val requiredText = Set("Wedge")
  val list = Baseline.filterOutUnrelatedBaselines(maintRecPKset, requiredText)

  println("results:\n    " + list.map(_.maintenanceRecordPK.get).sorted.mkString("\n    "))
  println("Number of results: " + list.size)
  println

  true should be(true)

}
