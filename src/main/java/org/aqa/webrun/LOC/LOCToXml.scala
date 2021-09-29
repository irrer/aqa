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

package org.aqa.webrun.LOC

import org.aqa.webrun.LOCXml

import java.io.File
import scala.collection.immutable
import scala.xml.NodeSeq
import scala.xml.XML

/**
  * Extract LOC related values from XML file.
  *
  * @param file File containing XML.
  */
class LOCToXml(file: File) {
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

  override def toString: String = {

    def show(seq: Seq[Double], name: String): String = {
      name + " : " + seq.size + " : " +
        seq.map(_.formatted("  %10f")).mkString("\n") + "\n"
    }

    def show2(seqSeq: Seq[Seq[Double]], name: String): String = {
      name + " : " + seqSeq.size + " * " + (if (seqSeq.nonEmpty) seqSeq.head.size else 0) +
        seqSeq.map(seq =>
          {
            seq.map(_.formatted("  %10f")).mkString("\n")
          }.mkString("\n")
        )
    }

    def showInt(seq: Seq[Int], name: String): String = {
      (name + " :") +
        seq.map(_.formatted("%5d")).mkString("\n") + "\n"
    }

    val line = "\n-------------------------------------------------------------------------\n"

    show2(this.LeafOffsetConstancyValue, "LeafOffsetConstancyValue") +
      show(this.LeafOffsetConstancyMean, "LeafOffsetConstancyMean") +
      show(this.LeafOffsetConstancyRange, "LeafOffsetConstancyRange") +
      show(this.LeafOffsetConstancySectionMean, "LeafOffsetConstancySectionMean") +
      show(this.LeafOffsetConstancySectionSTD, "LeafOffsetConstancySectionSTD") +
      show(this.LeafOffsetConstancySectionCoeffOfVar, "LeafOffsetConstancySectionCoeffOfVar") +
      show(this.LeafOffsetConstancySectionRange, "LeafOffsetConstancySectionRange") +
      line +
      show2(this.LeafOffsetTransmissionValue, "LeafOffsetTransmissionValue") +
      show(this.LeafOffsetTransmissionMean, "LeafOffsetTransmissionMean") +
      show(this.LeafOffsetTransmissionSectionMean, "LeafOffsetTransmissionSectionMean") +
      show(this.LeafOffsetTransmissionSectionSTD, "LeafOffsetTransmissionSectionSTD") +
      show(this.LeafOffsetTransmissionSectionCoeffOfVar, "LeafOffsetTransmissionSectionCoeffOfVar") +
      show(this.LeafOffsetTransmissionSectionRange, "LeafOffsetTransmissionSectionRange") +
      line +
      show2(this.LOCRSquared, "LOCRSquared") +
      line +
      show2(this.LOCDifferenceFromBaselineOpen, "LOCDifferenceFromBaselineOpen") +
      line +
      show2(this.LOCDifferenceFromBaselineTrans, "LOCDifferenceFromBaselineTrans") +
      line +
      showInt(this.leafIndexList, "leafIndexList") +
      line +
      "Number of sections: " + this.sections +
      line
  }
}
