package org.aqa.db

import scala.xml.Elem
import java.io.File
import scala.xml.XML
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.Logging._

trait ProcedureOutput {
    /** Identifies the top level XML tag for procedure output. */
    val topXmlLabel: String;

    def contains(elem: Elem): Boolean = {
        (elem \ topXmlLabel).headOption.isDefined
    }

    def insert(elem: Elem, outputPK: Long): Int;
}

object ProcedureOutput {

    val outputFileName = "output.xml"

    val procedureList: List[ProcedureOutput] = List(LeafOffsetCorrection, LeafTransmission)

    private def insert(elem: Elem, outputPK: Long) = procedureList.map(po => po.insert(elem, outputPK))
}

object ProcedureOutputMain {
    private val labelList = ProcedureOutput.procedureList.map(p => p.topXmlLabel)

    def main(args: Array[String]): Unit = {
        try {
            val valid = Config.validate
            DbSetup.init
            println("Inserting data into database")
            val outputPK = System.getenv("outputPK").toLong
            println("Using outputPK: " + outputPK)
            val outputFile = new File(ProcedureOutput.outputFileName)
            println("Using outputFile: " + outputFile.getAbsolutePath)
            val elem = XML.loadFile(outputFile)
            println("Read file " + outputFile.getAbsolutePath)
            val known = (elem \ "_").map(n => n.label)

            val unknown = known.diff(labelList)
            if (unknown.nonEmpty) {
                val unknownText = (unknown.foldLeft("")((t, l) => t + "  " + l)).trim
                val msg = ("Unknown top level values in the file " + outputFile.getAbsolutePath + " were defined: " + unknownText)
                ProcedureStatus.terminate(msg, ProcedureStatus.crash)
            }

            def insert(label: String) = {
                val procedureOutput = ProcedureOutput.procedureList.find(p => p.topXmlLabel.equals(label)).get
                println("Inserting values in database for " + label)
                val count = procedureOutput.insert(elem, outputPK)
                println("Inserted " + count + " values in database for " + label)
            }

            known.map(label => insert(label))
            println("Done inserting data into database")
            System.exit(0)
        }
        catch {
            case t: Throwable => ProcedureStatus.terminate("Unexpected problem while inserting data into the database: " + fmtEx(t), ProcedureStatus.crash)
        }
    }
}