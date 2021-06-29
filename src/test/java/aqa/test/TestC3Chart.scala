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

import org.aqa.Crypto
import org.aqa.Util
import org.aqa.web.C3Chart
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.awt.Color

/**
  * Test the Config.
  *
  */

class TestC3Chart extends FlatSpec with Matchers {

  "C3Chart" should "makeNiceChart" in {

    val xAxisLabel = "X Axis Label"
    val xDataLabel = "X Data Label"
    val xValueList = (2 to 25).map(i => i * 1.1)
    val xFormat = ".g4"
    val yRange = (1 to 6)
    val yAxisLabels = yRange.map(i => "Y Axis " + i + " Label ").toSeq
    val yDataLabel = "Y Data Label"
    val yValues = yRange.map(yy => yRange.map(y => ((y * 71 * yy) % 13) * 6786.3))
    val yFormat = ".g4"
    val yColorList = Util.colorPallette(new Color(0x4477bb), new Color(0x44aaff), yValues.size)

    val chart = new C3Chart(
      xAxisLabel = xAxisLabel: String,
      xDataLabel = xDataLabel,
      xValueList = xValueList,
      xFormat = xFormat,
      yAxisLabels = yAxisLabels,
      yDataLabel = yDataLabel,
      yValues = yValues,
      yFormat = yFormat,
      yColorList = yColorList
    )

    println(chart.javascript)

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Crypto.randomSecureHash).distinct
    list.size should be(size)
  }
}
