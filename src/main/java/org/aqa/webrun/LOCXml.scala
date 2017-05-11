package org.aqa.webrun

import scala.xml.XML
import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import org.aqa.procedures.ProcedureOutputUtil

/**
 * Extract LOC related values from XML file.
 */
class LOCXml(dir: File) {
    private val file = new File(dir, ProcedureOutputUtil.outputFileName)
    private val elem = XML.loadFile(file)

    private def nodeSeqToDouble(ns: NodeSeq): Seq[Double] = {
        ns.map(n => n.head.text.toDouble)
    }

    private val constancy = elem \ "LeafOffsetConstancy"
    private val constancyLeafList = constancy \ "LeafList" \ "Leaf"

    val LeafOffsetConstancyValue = constancyLeafList.map(n => nodeSeqToDouble(n \ "Value"))
    val LeafOffsetConstancyMean = nodeSeqToDouble(constancyLeafList \ "Mean")
    val LeafOffsetConstancyRange = nodeSeqToDouble(constancyLeafList \ "Range")
    val LeafOffsetConstancySectionMean = nodeSeqToDouble(constancy \ "MeanList" \ "Mean")
    val LeafOffsetConstancySectionSTD = nodeSeqToDouble(constancy \ "STDList" \ "STD")
    val LeafOffsetConstancySectionCoeffOfVar = nodeSeqToDouble(constancy \ "CoeffOfVarList" \ "CoeffOfVar")
    val LeafOffsetConstancySectionRange = nodeSeqToDouble(constancy \ "RangeList" \ "Range")

    private val transmission = elem \ "LeafTransmission"
    private val transmissionLeafList = transmission \ "LeafList" \ "Leaf"
    val LeafOffsetTransmissionValue = transmissionLeafList.map(n => nodeSeqToDouble(n \ "Value"))
    val LeafOffsetTransmissionMean = nodeSeqToDouble(transmissionLeafList \ "Mean")
    val LeafOffsetTransmissionSectionMean = nodeSeqToDouble(transmission \ "MeanList" \ "Mean")
    val LeafOffsetTransmissionSectionSTD = nodeSeqToDouble(transmission \ "STDList" \ "STD")
    val LeafOffsetTransmissionSectionCoeffOfVar = nodeSeqToDouble(transmission \ "CoeffOfVarList" \ "CoeffOfVar")
    val LeafOffsetTransmissionSectionRange = nodeSeqToDouble(transmission \ "RangeList" \ "Range")

    val LOCRSquared = (elem \ "LOCRSquared" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

    val LOCDifferenceFromBaselineOpen = (elem \ "LOCDifferenceFromBaselineOpen" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

    val LOCDifferenceFromBaselineTrans = (elem \ "LOCDifferenceFromBaselineTrans" \ "Leaf").map(n => nodeSeqToDouble(n \ "Value"))

    val leafIndexList = (constancyLeafList \ "leafIndex").map(n => n.head.text.toInt)
    val sections = LeafOffsetConstancyValue.head.size
}

object LOCXml {
    def main(args: Array[String]): Unit = {
        val dir = new File("""D:\AQA_Data\results\TBD_2\CHIC2_12\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-10T14-41-53-929_107\output_2017-05-10T14-41-54-056""")
        val locXml = new LOCXml(dir)

        def show(seq: Seq[Double], name: String) = {
            print(name + " : " + seq.size + " : ")
            seq.map(d => print(d.formatted("  %10f")))
            println
        }

        def show2(seqSeq: Seq[Seq[Double]], name: String) = {
            println(name + " : " + seqSeq.size + " * " + (if (seqSeq.nonEmpty) seqSeq.head.size else 0))
            for (seq <- seqSeq) {
                seq.map(d => print(d.formatted("  %10f")))
                println
            }
        }

        def showInt(seq: Seq[Int], name: String) = {
            print(name + " :")
            seq.map(d => print(d.formatted("%5d")))
            println
        }
        
        def line = println("\n-------------------------------------------------------------------------\n")

        show2(locXml.LeafOffsetConstancyValue, "LeafOffsetConstancyValue")
        show(locXml.LeafOffsetConstancyMean, "LeafOffsetConstancyMean")
        show(locXml.LeafOffsetConstancyRange, "LeafOffsetConstancyRange")
        show(locXml.LeafOffsetConstancySectionMean, "LeafOffsetConstancySectionMean")
        show(locXml.LeafOffsetConstancySectionSTD, "LeafOffsetConstancySectionSTD")
        show(locXml.LeafOffsetConstancySectionCoeffOfVar, "LeafOffsetConstancySectionCoeffOfVar")
        show(locXml.LeafOffsetConstancySectionRange, "LeafOffsetConstancySectionRange")
        line
        show2(locXml.LeafOffsetTransmissionValue, "LeafOffsetTransmissionValue")
        show(locXml.LeafOffsetTransmissionMean, "LeafOffsetTransmissionMean")
        show(locXml.LeafOffsetTransmissionSectionMean, "LeafOffsetTransmissionSectionMean")
        show(locXml.LeafOffsetTransmissionSectionSTD, "LeafOffsetTransmissionSectionSTD")
        show(locXml.LeafOffsetTransmissionSectionCoeffOfVar, "LeafOffsetTransmissionSectionCoeffOfVar")
        show(locXml.LeafOffsetTransmissionSectionRange, "LeafOffsetTransmissionSectionRange")
        line
        show2(locXml.LOCRSquared, "LOCRSquared")
        line
        show2(locXml.LOCDifferenceFromBaselineOpen, "LOCDifferenceFromBaselineOpen")
        line
        show2(locXml.LOCDifferenceFromBaselineTrans, "LOCDifferenceFromBaselineTrans")
        line
        showInt(locXml.leafIndexList, "leafIndexList")
        line
        println("Number of sections: " + locXml.sections)
        line
    }
}
