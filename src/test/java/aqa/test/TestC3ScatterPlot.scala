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

import org.aqa.web.C3ScatterPlot
import org.aqa.web.C3ScatterPlotDataPoint
import org.aqa.web.C3ScatterPlotDataSet
import org.aqa.Util

import java.io.File

object TestC3ScatterPlot {

  val chartTag = "TheChart"

  private def makeDoc(ref: String, js: String) =
    s"""
      |<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |    <meta charset="UTF-8">
      |    <meta name="viewport" content="width=device-width, initial-scale=1.0">
      |    <title>C3 Scatter Plot Example</title>
      |    <link href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.min.css" rel="stylesheet">
      |    <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.16.0/d3.min.js"></script>
      |    <script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.min.js"></script>
      |    <style>
      |        body {
      |            font-family: Arial, sans-serif;
      |        }
      |    </style>
      |</head>
      |<body>
      |
      |<div>
      |Before chart.
      |<div>
      |$ref
      |</div>
      |After chart.
      |</div>
      |<script>
      |        $js
      |</script>
      |
      |</body>
      |</html>
      |
      |""".stripMargin

  def main(args: Array[String]): Unit = {

    val data1 = {
      val d = (1 until 10).map(i => C3ScatterPlotDataPoint(i, i))
      C3ScatterPlotDataSet("First Data", d, color = None)
    }

    val data2 = {
      val d = (1 until 10).map(i => C3ScatterPlotDataPoint(-i, -i))
      C3ScatterPlotDataSet("Second Data", d, color = Some("red"))
    }

    val scatter = new C3ScatterPlot(
      dataList = Seq(data1, data2),
      xAxisLabel = "X Label",
      yAxisLabel = "Y Label",
      width = Some(400),
      height = Some(200)
    )

    val text = makeDoc(Util.prettyPrint(scatter.html), scatter.javascript)

    val targetDir = new File("target")
    val dir = new File(targetDir, "TestC3ScatterPlot")
    dir.mkdirs

    val file = new File(dir, "TestC3ScatterPlot.html")
    Util.writeFile(file, text)

    println(s"Use browser to view this file: ${file.getAbsolutePath}")

  }
}
