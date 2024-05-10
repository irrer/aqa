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

package org.aqa.webrun

import org.aqa.webrun.LOC.LOCUtil

import java.io.File
import scala.collection.immutable
import scala.xml.NodeSeq
import scala.xml.XML

/**
  * Extract LOC related values from XML file.
  */
class LOCXml(dir: File) {
  private val file = new File(dir, LOCUtil.locXmlOutputFileName)
  private val elem = XML.loadFile(file)

  private def nodeSeqToDouble(ns: NodeSeq): Seq[Double] = {
    ns.map(n => LOCXml.textToDouble(n.head.text))
  }

  private val constancy = elem \ "LeafOffsetConstancy"
  private val constancyLeafList = constancy \ "LeafList" \ "Leaf"

  val outputPK: Long = (elem \ "@outputPK").head.text.toLong

  val epidCenterCorrection_mm: Double = (elem \ "EPIDCenterCorrection").head.text.toDouble

  val LeafOffsetConstancyValue: immutable.Seq[Seq[Double]] = constancyLeafList.map(n => nodeSeqToDouble(n \ "Value"))
  val LeafOffsetConstancyMean: Seq[Double] = nodeSeqToDouble(constancyLeafList \ "Mean")
  val LeafOffsetConstancyRange: Seq[Double] = nodeSeqToDouble(constancyLeafList \ "Range")
  val LeafOffsetConstancySectionMean: Seq[Double] = nodeSeqToDouble(constancy \ "MeanList" \ "Mean")
  val LeafOffsetConstancySectionSTD: Seq[Double] = nodeSeqToDouble(constancy \ "STDList" \ "STD")
  val LeafOffsetConstancySectionCoeffOfVar: Seq[Double] = nodeSeqToDouble(constancy \ "CoeffOfVarList" \ "CoeffOfVar")
  val LeafOffsetConstancySectionRange: Seq[Double] = nodeSeqToDouble(constancy \ "RangeList" \ "Range")

  private val transmission = elem \ "LeafTransmission"
  private val transmissionLeafList = transmission \ "LeafList" \ "Leaf"
  val LeafOffsetTransmissionValue: immutable.Seq[Seq[Double]] = transmissionLeafList.map(n => nodeSeqToDouble(n \ "Value"))
  val LeafOffsetTransmissionMean: Seq[Double] = nodeSeqToDouble(transmissionLeafList \ "Mean")
  val LeafOffsetTransmissionSectionMean: Seq[Double] = nodeSeqToDouble(transmission \ "MeanList" \ "Mean")
  val LeafOffsetTransmissionSectionSTD: Seq[Double] = nodeSeqToDouble(transmission \ "STDList" \ "STD")
  val LeafOffsetTransmissionSectionCoeffOfVar: Seq[Double] = nodeSeqToDouble(transmission \ "CoeffOfVarList" \ "CoeffOfVar")
  val LeafOffsetTransmissionSectionRange: Seq[Double] = nodeSeqToDouble(transmission \ "RangeList" \ "Range")

  val LOCRSquared: immutable.Seq[Seq[Double]] = (elem \ "LOCRSquared" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

  val LOCDifferenceFromBaselineOpen: immutable.Seq[Seq[Double]] = (elem \ "LOCDifferenceFromBaselineOpen" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

  val LOCDifferenceFromBaselineTrans: immutable.Seq[Seq[Double]] = (elem \ "LOCDifferenceFromBaselineTrans" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

  val leafIndexList: immutable.Seq[Int] = (constancyLeafList \ "leafIndex").map(n => n.head.text.toInt).distinct.sorted
  val sections: Int = LeafOffsetConstancyValue.head.size
}

object LOCXml {

  private def okDbl(d: Double) = { (d < Double.MaxValue) && (d > Double.MinValue) }

  def textToDouble(text: String): Double = {
    try {
      val d = text.trim.toDouble
      if (okDbl(d)) d else Double.NaN
    } catch {
      case t: Throwable => Double.NaN
    }
  }

  def main(args: Array[String]): Unit = {
    // val dir = new File("""D:\AQA_Data\results\TBD_2\CHIC2_12\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-10T14-41-53-929_107\output_2017-05-10T14-41-54-056""")
    val dir = new File("""D:\tmp\aqa\tmp\mario\bad""")
    val locXml = new LOCXml(dir)

    def show(seq: Seq[Double], name: String): Unit = {
      print(name + " : " + seq.size + " : ")
      seq.foreach(d => print(d.formatted("  %10f")))
      println
    }

    def show2(seqSeq: Seq[Seq[Double]], name: String): Unit = {
      println(name + " : " + seqSeq.size + " * " + (if (seqSeq.nonEmpty) seqSeq.head.size else 0))
      for (seq <- seqSeq) {
        seq.foreach(d => print(d.formatted("  %10f")))
        println
      }
    }

    def showInt(seq: Seq[Int], name: String): Unit = {
      print(name + " :")
      seq.foreach(d => print(d.formatted("%5d")))
      println
    }

    def line(): Unit = println("\n-------------------------------------------------------------------------\n")

    show2(locXml.LeafOffsetConstancyValue, "LeafOffsetConstancyValue")
    show(locXml.LeafOffsetConstancyMean, "LeafOffsetConstancyMean")
    show(locXml.LeafOffsetConstancyRange, "LeafOffsetConstancyRange")
    show(locXml.LeafOffsetConstancySectionMean, "LeafOffsetConstancySectionMean")
    show(locXml.LeafOffsetConstancySectionSTD, "LeafOffsetConstancySectionSTD")
    show(locXml.LeafOffsetConstancySectionCoeffOfVar, "LeafOffsetConstancySectionCoeffOfVar")
    show(locXml.LeafOffsetConstancySectionRange, "LeafOffsetConstancySectionRange")
    line()
    show2(locXml.LeafOffsetTransmissionValue, "LeafOffsetTransmissionValue")
    show(locXml.LeafOffsetTransmissionMean, "LeafOffsetTransmissionMean")
    show(locXml.LeafOffsetTransmissionSectionMean, "LeafOffsetTransmissionSectionMean")
    show(locXml.LeafOffsetTransmissionSectionSTD, "LeafOffsetTransmissionSectionSTD")
    show(locXml.LeafOffsetTransmissionSectionCoeffOfVar, "LeafOffsetTransmissionSectionCoeffOfVar")
    show(locXml.LeafOffsetTransmissionSectionRange, "LeafOffsetTransmissionSectionRange")
    line()
    show2(locXml.LOCRSquared, "LOCRSquared")
    line()
    show2(locXml.LOCDifferenceFromBaselineOpen, "LOCDifferenceFromBaselineOpen")
    line()
    show2(locXml.LOCDifferenceFromBaselineTrans, "LOCDifferenceFromBaselineTrans")
    line()
    showInt(locXml.leafIndexList, "leafIndexList")
    line()
    println("Number of sections: " + locXml.sections)
    line()
  }
}
