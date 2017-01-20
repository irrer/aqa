package org.aqa.run

import org.aqa.db.Machine
import scala.xml.Elem
import org.aqa.db.Procedure
import java.io.File
import org.aqa.Config
import org.aqa.db.Institution
import org.aqa.Logging._
import java.text.SimpleDateFormat
import org.aqa.db.Input
import org.aqa.db.User
import org.aqa.web.Session
import java.sql.Date
import org.aqa.web.WebUtil
import java.io.FileOutputStream
import org.restlet.Response
import org.aqa.db.Output
import sys.process._
import edu.umro.ScalaUtil.Trace._
import org.aqa.Util
import org.restlet.Request
//import scala.concurrent.Await
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.sys.process._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import resource.managed
import edu.umro.util.Utility
import org.restlet.data.MediaType
import org.aqa.web.WebServer
import java.io.FileWriter
import java.io.Closeable
import java.io.Flushable
import scala.collection.mutable.ArrayBuffer
import org.aqa.web.ViewOutput
import java.sql.Timestamp
import java.io.RandomAccessFile
import java.io.FileInputStream
import java.io.FileOutputStream
import org.aqa.db.DataValidity

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

        val inputDir: File = nameHierarchy.foldLeft(WebServer.dataDir)((d, name) => new File(d, name))

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
                                    ProcedureStatus.writeProcedureStatus(actProc.output.dir, ProcedureStatus.timeout)
                                }
                            }
                            val fileStatus = ProcedureStatus.dirToProcedureStatus(actProc.output.dir)
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
        val cd = "CD /D " + output.dir.getAbsolutePath

        val setDir = "SET PROCEDURE_DIR=" + procedure.execDir

        val setInputPk = "SET inputPK=" + output.inputPK

        val setOutputPk = "SET outputPK=" + output.outputPK.get

        val setDbCommand = "SET DatabaseCommand=" + Config.DatabaseCommand

        val execute = procedure.execDir + File.separator + runCommandName

        val cmdList = List(cd, setDir, setInputPk, setOutputPk, setDbCommand, execute)
        val inputString = cmdList.foldLeft("")((t, c) => t + c + System.lineSeparator)
        println("inputString: " + inputString) // TODO rm
        val inputStream = new java.io.ByteArrayInputStream(inputString.getBytes("UTF-8"))

        val pb = Process(Seq("cmd.exe")) #< inputStream
        val logger = new StdLogger(output)
        val process = pb.run(logger, true)
        new ActiveProcess(output, process, logger)
    }

    private def now: Date = new Date(System.currentTimeMillis)

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
        input.updateDirectory(WebServer.fileToDataPath(inputDir))

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
                directory = WebServer.fileToDataPath(outputDir),
                procedurePK = procedure.procedurePK.get,
                userPK,
                new Timestamp(startDate.getTime),
                finishDate = None,
                status = ProcedureStatus.running.toString,
                dataValidity = DataValidity.valid.toString)
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

    def handleRunning(output: Output, procedure: Procedure) = {

        try {
            // Get the most recent modification date.
            def latestFileChange: Long = (output.dir.listFiles :+ output.dir).map(f => f.lastModified).max

            def timeoutTime: Long = output.startDate.getTime + procedure.timeoutInMs

            def procedureHasTimedOut = timeoutTime > System.currentTimeMillis

            def updateDb(status: ProcedureStatus.Value): Unit = {
                val updatedOutput = new Output(
                    outputPK = output.outputPK,
                    inputPK = output.inputPK,
                    directory = output.directory,
                    procedurePK = output.procedurePK,
                    userPK = output.userPK,
                    startDate = output.startDate,
                    finishDate = Some(new Timestamp(latestFileChange)),
                    status = status.toString,
                    dataValidity = output.dataValidity)
                updatedOutput.insertOrUpdate
            }

            def updateFile(status: ProcedureStatus.Value): Unit = ProcedureStatus.writeProcedureStatus(output.dir, status)

            def updateBoth(status: ProcedureStatus.Value): Unit = {
                updateFile(status)
                updateDb(status)
            }

            val statusFromFile = ProcedureStatus.dirToProcedureStatus(WebServer.fileOfDataPath(output.directory))

            def killProcess = logWarning("Need to implement killProcess") // TODO

            def startMonitor = logWarning("Need to implement startMonitor") // TODO

            if (statusFromFile.isDefined) {
                // procedure finished while server was down, which should be the usual case
                updateDb(statusFromFile.get)
            }
            else {
                val procedureIsRunning = {
                    logWarning("Need to implement procedureIsRunning") // TODO  note that rename or any of the file locking on log.txt do not work.
                    false
                }

                0 match {
                    case _ if procedureHasTimedOut && procedureIsRunning => { killProcess; updateBoth(ProcedureStatus.timeout) } // out of time.  Kill it.
                    case _ if procedureIsRunning => startMonitor // wait for the allotted timeout and the re-evaluate
                    case _ => updateBoth(ProcedureStatus.crash) // it stopped without giving a status
                }
            }
        }
        catch {
            case e: Throwable => {
                logSevere("Unexpected exeception while wrapping up unterminated procedure after service restart: " +
                    e.getMessage + "\n    Output: " + output + "\n    Procedure: " + procedure)
            }
        }
    }

    /**
     * Look for any Output's that were in running state when the server was shut down and handle them.
     */
    def handleRunningProcedureList = Output.listWithStatus(ProcedureStatus.running).map(or => handleRunning(or._1, or._2))

    def main(args: Array[String]): Unit = {

        if (true) {
            val procDir = new File("""D:\AQA_Data\data\University of Michigan Radiation Oncology_1\TB5_2\WinstonLutz_1.0_1""")

            def newest(dir: File): File = dir.listFiles.sortWith((a, b) => a.lastModified > b.lastModified()).head

            val inDir = newest(procDir)

            val outDir = inDir.listFiles.filter(f => f.getName.startsWith("output")).head

            val logFile = outDir.listFiles.filter(f => f.getName.equalsIgnoreCase("log.txt")).head

            println("logFile: " + logFile.getAbsolutePath)

            if (false) {
                val dest = new File(logFile.getAbsolutePath + "X.txt")
                println("dest: " + dest.getAbsolutePath)
                println("rename: " + logFile.renameTo(dest))
                System.exit(99)
            }

            while (true) {

                println("\n" + (new java.util.Date).toString)

                if (false) {
                    println("logFile canWrite: " + logFile.canWrite)
                }

                if (false) {
                    val raf = new RandomAccessFile(logFile, "rw");
                    val channel = raf.getChannel();
                    val lock = channel.tryLock()
                    println("lock is null: " + (lock == null).toString)
                    if (lock != null) println("lock.isValid: " + lock.isValid)
                    if (lock != null && lock.isValid) lock.release
                    channel.close
                    raf.close
                }

                if (false) {
                    val raf = new RandomAccessFile(logFile, "rw");
                    val channel = raf.getChannel();
                    val sharedLock = channel.tryLock(0, Long.MaxValue, true)
                    println("sharedLock is null: " + (sharedLock == null).toString)
                    println("sharedLock: " + sharedLock.isValid())
                    if (sharedLock.isValid) sharedLock.release
                    channel.close
                    raf.close
                }

                if (false) {
                    val raf = new RandomAccessFile(logFile, "rw");
                    val channel = raf.getChannel();
                    val notSharedLock = channel.tryLock(0, Long.MaxValue, false)
                    println("notSharedLock is null: " + (notSharedLock == null).toString)
                    println("notSharedLock: " + notSharedLock.isValid())
                    if (notSharedLock.isValid) notSharedLock.release
                    channel.close
                    raf.close
                }

                if (false) {
                    try {
                        val in = new FileInputStream(logFile)
                        val isLocked = in != null
                        println("isLocked read: " + isLocked)
                        if (in != null) in.close();
                    }
                    catch {
                        case e: Throwable => println("badness: " + e.getMessage)
                    }
                }

                if (false) {
                    try {
                        val out = new FileOutputStream(logFile)
                        val isLocked = out != null
                        println("isLocked write: " + isLocked)
                        if (out != null) out.close();
                    }
                    catch {
                        case e: Throwable => println("badness: " + e.getMessage)
                    }
                }

                if (true) {
                    try {
                        val dest = new File(logFile.getAbsolutePath + "X.txt")
                        println("dest: " + dest.getAbsolutePath)
                        println("rename: " + logFile.renameTo(dest))
                    }
                    catch {
                        case e: Throwable => println("badness: " + e.getMessage)
                    }
                }

                Thread.sleep(1000)
            }
        }

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