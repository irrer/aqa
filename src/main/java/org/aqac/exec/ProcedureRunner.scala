package org.aqac.exec

import org.aqac.db.Procedure
import org.aqac.db.Input
import edu.umro.ScalaUtil.Trace._
import scala.sys.process.ProcessIO
import scala.sys.process.Process
import java.io.File
import java.io.InputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

class ProcedureRunner(procedure: Procedure, input: Input) {

    val inDir = new File(input.directory)

    "CD /D " + input.directory + """ && .\""" + ProcedureRunner.runCommandName
}

object ProcedureRunner {

    val runCommandName = "run"

    def main(args: Array[String]): Unit = {

        if (true) {
            val procDirName = """D:\tmp\aqac\proc"""
            val procDir = new File(procDirName)
            procDir.mkdirs

            val content = "CD /D " + procDirName + " && " + procDirName + "\\" + runCommandName
            println("content: " + content)
            // Run the Windows CMD program and have it change directory to the target directory, and
            // then run the procedure execution file, which can be any of:
            //    run.cmd
            //    run.bat
            //    run.exe
            val processBuilder = Process(Seq("cmd.exe", "/C", content))

            val process = processBuilder.run

            Thread.sleep(1000)
            System.exit(99)
        }

    }

}