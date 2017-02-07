package org.aqa.procedures

import scala.xml.Elem
import java.io.File
import org.aqa.db.LeafOffsetCorrection
import org.aqa.db.LeafTransmission
import org.aqa.run.ProcedureStatus
import scala.xml.XML
import org.aqa.db.DbSetup
import org.aqa.Config
import org.aqa.Logging

object ProcedureOutputUtil {

    val outputFileName = "output.xml"

    private val procedureList: List[ProcedureOutput] = List(LeafOffsetCorrection, LeafTransmission)

    private def insert(elem: Elem, outputPK: Long) = procedureList.map(po => po.insert(elem, outputPK))

    private val labelList = procedureList.map(p => p.topXmlLabel)

    private def getOutputFile(args: Array[String]): File = {
        if ((args == null) || (args.isEmpty)) new File(outputFileName)
        else new File(args(0))
    }

    def main(args: Array[String]): Unit = {
        try {
            val valid = Config.validate
            DbSetup.init
            println("Inserting data into database")
            val outputPK = System.getenv("outputPK").toLong
            println("Using outputPK: " + outputPK)
            val outputFile = getOutputFile(args)
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
                val procedureOutput = procedureList.find(p => p.topXmlLabel.equals(label)).get
                println("Inserting values in database for " + label)
                val count = procedureOutput.insert(elem, outputPK)
                println("Inserted " + count + " values in database for " + label)
            }

            known.map(label => insert(label))
            println("Done inserting data into database")
            System.exit(0)
        }
        catch {
            case t: Throwable => {
                ProcedureStatus.terminate("Unexpected problem while inserting data into the database: " + Logging.fmtEx(t), ProcedureStatus.crash)
                System.exit(1)
            }
        }
    }

}
