package org.aqac.run

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
import edu.umro.ScalaUtil.Trace._
import org.aqac.Util
import org.restlet.Request
import scala.concurrent.Await
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.sys.process._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import resource.managed
import edu.umro.util.Utility
import org.restlet.data.MediaType
import org.aqac.web.WebServer
import java.io.FileWriter
import java.io.Closeable
import java.io.Flushable
import scala.collection.mutable.ArrayBuffer
import org.aqac.web.ViewOutput
import java.sql.Timestamp

/**
 * Run a procedure.
 */
object Run {

    private val ouputSubdirNamePrefix = "output_"

    /** Procedure must supply a cmd, bat, or exe file with this name. */
    private val runCommandName = "run"

    private def appendPK(text: String, pk: Long): String = text + "_" + pk.toString

    private def institutionFileName(machine: Machine): String = {
        val institution = Institution.get(machine.institutionPK)
        if (institution.isDefined) appendPK(institution.get.fileName, institution.get.institutionPK.get)
        else {
            logWarning("Run.dir Could not find institution for machine " + machine.toString)
            "unknown_institution_" + machine.fileName
        }
    }

    /**
     * Construct a directory organized by:  institution / machine / procedure / time
     */
    private def makeInputDir(machine: Machine, procedure: Procedure, inputPK: Long): File = {
        def nameHierarchy = List(
            institutionFileName(machine),
            appendPK(machine.fileName, machine.machinePK.get),
            appendPK(procedure.fileName, procedure.procedurePK.get),
            appendPK(Util.currentTimeAsFileName, inputPK))

        val inputDir: File = nameHierarchy.foldLeft(WebServer.DATA_DIR)((d, name) => new File(d, name))

        logInfo("New input directory: " + inputDir.getAbsolutePath)
        inputDir
    }

    private def runInThread(func: () => Unit): Unit = {
        class Spinoff extends Runnable {
            def run: Unit = func()
        }
        (new Thread(new Spinoff)).start
    }

    /**
     * Start a thread to monitor the process.  When the process terminates, its status and
     * finish time will be recorded in the database.  If the process fails to finish before
     * the procedure's timeout, then the process will be killed and assigned the status of
     * <code>timeout</code>.
     */
    private def startProcessMonitor(actProc: ActiveProcess, msTimeout: Long): Unit = {
        def run(): Unit = {
            managed(actProc.logger) acquireAndGet {
                lgr =>
                    {
                        try {
                            val future = Future(blocking(actProc.process.exitValue)) // wrap in Future
                            val res = try {
                                val timeout = Duration(msTimeout, TimeUnit.MILLISECONDS)
                                Await.result(future, timeout)
                            }
                            catch {
                                case _: TimeoutException => {
                                    actProc.process.destroy
                                    ProcedureStatus.writeProcedureStatus(new File(actProc.output.directory), ProcedureStatus.timeout)
                                }
                            }
                            val fileStatus = ProcedureStatus.dirToProcedureStatus(new File(actProc.output.directory))
                            val status = if (fileStatus.isDefined) fileStatus.get else ProcedureStatus.crash
                            // update DB Output
                            actProc.output.updateStatusAndFinishDate(status.toString, now)
                            ActiveProcess.remove(actProc.output.outputPK.get)
                        }
                        catch {
                            case t: Throwable => logWarning("Unexpected error running procedure.  Output: " + actProc.output + " : " + t.getMessage)
                        }
                    }
            }
        }
        runInThread(run)
    }

    /**
     * Run the Windows CMD program and have it change directory to the target directory, and
     * then run the procedure execution file, which can be any of:
     *    run.cmd
     *    run.bat
     *    run.exe
     */
    private def startProcess(procedure: Procedure, output: Output): ActiveProcess = {
        val runCommand = procedure.execDir.getAbsolutePath + File.separator + runCommandName

        val content = "CD /D " + output.directory + " && " + procedure.execDir + File.separator + runCommandName

        val processBuilder = Process(Seq("cmd.exe", "/C", content))

        val logger = new StdLogger(output)
        val process = processBuilder.run(logger)
        new ActiveProcess(output, process, logger)
    }

    private def now: Date = new Date(System.currentTimeMillis)

    private def setResponse(output: Output, response: Response): Unit = {

    }

    /**
     * Run a procedure.
     */
    def run(procedure: Procedure, machine: Machine, sessionDir: File, request: Request, response: Response, patientId: Option[String], acquisitionDate: Option[Long]) = {

        val user = WebUtil.getUser(request)
        val userPK = if (user.isDefined) user.get.userPK else None

        // create DB Input
        val acq = if (acquisitionDate.isDefined) Some(new Timestamp(acquisitionDate.get)) else None
        val input = (new Input(None, None, new Timestamp(now.getTime), userPK, machine.machinePK, patientId, acq)).insert

        // The input PK is needed to make the input directory, which creates a circular definition when making an
        // input row, but this is part of the compromise of creating a file hierarchy that has a consistent (as
        // practical) link to the database.
        val inputDir = makeInputDir(machine, procedure, input.inputPK.get)
        input.updateDirectory(inputDir.getAbsolutePath)

        // move input files to their final resting place
        inputDir.getParentFile.mkdirs
        val dirRenamed = sessionDir.renameTo(inputDir)
        if (!dirRenamed) throw new RuntimeException("Unable to rename temporary directory " + sessionDir.getAbsolutePath + " to " + inputDir.getAbsolutePath)

        val startDate = new Date(System.currentTimeMillis)

        val outputDir = new File(inputDir, ouputSubdirNamePrefix + Util.timeAsFileName(startDate))
        outputDir.mkdirs // create output directory

        val output = {
            val tempOutput = new Output(
                outputPK = None,
                inputPK = input.inputPK.get,
                directory = outputDir.getAbsolutePath,
                procedurePK = procedure.procedurePK.get,
                userPK,
                new Timestamp(startDate.getTime),
                finishDate = None,
                status = ProcedureStatus.running.toString)
            tempOutput.insert
        }

        // Start a process that runs the procedure
        val actProc = startProcess(procedure, output)

        ActiveProcess.add(actProc)

        startProcessMonitor(actProc, procedure.timeoutInMs)

        // show default summary as web page
        val clientId = OutputPending.add(request, output)
        val suffix = "?" + ViewOutput.outputPKTag + "=" + output.outputPK.get
        response.redirectSeeOther(ViewOutput.path + suffix)
    }

    def main(args: Array[String]): Unit = {

        /*
        if (true) {
            val logger = ProcessLogger(
                (o: String) => println("out " + o),
                (e: String) => println("err " + e))

            "ls -ld aaa s*" ! logger
        }
        */

        if (false) {
            val contents = Process("date").lineStream
            Thread.sleep(1000)
            contents.map(s => println("++" + s))
        }

        if (false) {
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