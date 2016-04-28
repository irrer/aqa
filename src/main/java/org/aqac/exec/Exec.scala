package org.aqac.exec

import org.aqac.db.Machine
import scala.xml.Elem
import org.aqac.db.Procedure
import java.io.File
import org.aqac.Config
import org.aqac.db.Institution
import org.aqac.Logging._
import java.text.SimpleDateFormat
import org.aqac.db.Input
import org.aqac.db.User
import org.aqac.web.Session
import java.sql.Date
import org.aqac.web.WebUtil
import java.io.FileOutputStream
import org.restlet.Response
import org.aqac.db.Output
import sys.process.ProcessCreation
import sys.process._
import scala.sys.process._
import edu.umro.ScalaUtil.Trace._

object Exec {

    /** Time formatted to be used as a file name. */
    private val fileTimeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

    private val inputSubdirName = "input"

    private val ouputSubdirName = "output"

    private val parametersFileName = "parameters.xml"

    private def timeFileName = fileTimeFormat.format(System.currentTimeMillis)

    private def institutionFileName(machine: Machine): String = {
        val institution = Institution.get(machine.institutionPK)
        if (institution.isDefined) institution.get.fileName
        else {
            logWarning("Exec.dir Could not find institution for machine " + machine.toString)
            "unknown"
        }
    }

    /**
     * Construct a directory organized by:  procedure / institution / machine / time
     */
    private def makeDir(machine: Machine, session: String, procedure: Procedure): File = {
        def nameHierarchy = List(
            procedure.fileName,
            institutionFileName(machine),
            machine.fileName,
            timeFileName)

        def dir: File = nameHierarchy.foldLeft(Config.procedureDir)((d, name) => new File(d, name))

        // Handle the wildly improbable case where the directory already exists.
        def makeDir: File = {
            val d = dir
            if (d.mkdirs) makeDir
            else d
        }

        val newDir = makeDir

        logInfo("New procedure directory: " + newDir.getAbsolutePath)
        newDir
    }

    /**
     * Move the input files from the temporary directory to the final directory.
     */
    private def moveInputFiles(session: String, dir: File): Unit = {
        val inputDir = new File(dir, inputSubdirName)
        val sessionDir = Session.idToFile(session)
        sessionDir.renameTo(inputDir)
    }

    /**
     *  Create parameter file
     */
    private def makeParametersFile(parameters: Elem, dir: File): Unit = {
        val text = WebUtil.xmlToText(parameters)
        val paramFile = new File(dir, parametersFileName)
        val fos = new FileOutputStream(new File(dir, parametersFileName))
        fos.write(text.getBytes)
        fos.flush
        fos.close
    }

    private def runProcedure(dir: File, procedure: Procedure): Int = {
        ???
    }

    private def setResponse(dir: File, response: Response): Unit = {
        ???
    }

    /**
     * Execute a procedure.
     */
    def exec(procedure: Procedure, machine: Machine, session: String, user: User, parameters: Elem, response: Response) = {

        val dir = makeDir(machine, session, procedure)

        moveInputFiles(session, dir)

        makeParametersFile(parameters, dir)

        val outputDir = new File(dir, ouputSubdirName)
        outputDir.mkdirs // create output dir

        // create DB Input
        val input = new Input(None, dir.getAbsolutePath, new Date(System.currentTimeMillis), user.userPK.get, machine.machinePK)
        input.insert

        // run procedure
        val status = runProcedure(dir, procedure)

        // create DB Output
        val output = new Output(None, new Date(System.currentTimeMillis), input.inputPK.get)
        output.insert

        // show result as web page
        setResponse(dir, response)
    }

    def main(args: Array[String]): Unit = {

        if (false) {
            val contents = Process("date").lineStream
            Thread.sleep(1000)
            contents.map(s => println("++" + s))
        }

        if (true) {
            val env = System.getenv
            env.keySet.toArray.map(k => println(k + " = " + env.get(k)))
        }

        if (false) {
            val processIO: ProcessIO = null
            val processBuilder = Process("cmd.exe /C run.cmd")
            //processBuilder.getDeclaredField("pid")
            val process = processBuilder.run
            
            if (processBuilder.hasExitValue) trace(process.exitValue)
            Thread.sleep(2500)
            if (processBuilder.hasExitValue) trace(process.exitValue)
            process.destroy
            if (processBuilder.hasExitValue) trace(process.exitValue)
            Thread.sleep(250000)
        }
    }

}