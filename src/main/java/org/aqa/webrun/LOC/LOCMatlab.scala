package org.aqa.webrun.LOC

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.run.StdLogger
import org.aqa.webrun.ExtendedData

import java.io.File
import java.io.FileInputStream
import scala.sys.process.Process

object LOCMatlab extends Logging {

  /**
    * Create a 'run.cmd' file in the output directory and execute it.   Making the file (as opposed to
    * doing everything dynamically in memory) provides a diagnostic tool (run the file manually) if
    * there is a problem.
    *
    * @param extendedData Metadata for data being created.
    */
  private def execute(extendedData: ExtendedData): Unit = {

    def setEnv(name: String, value: String): String = {
      s"SET $name=$value"
    }

    val matlabExecutableFile = {
      val locDir = new File(Config.ProcedureDir, "LOC")
      val exeList = Util.listDirFiles(locDir).filter(_.canExecute).filter(_.getName.endsWith(".exe"))
      if (exeList.isEmpty) {
        throw new RuntimeException("Can not either find or execute LOC MATLAB file in dir " + locDir.getAbsolutePath)
      }
      exeList.head
    }

    val commandFileName = "run.cmd"

    val commandFile = new File(extendedData.output.dir, commandFileName)

    val commandList = Seq(
      "CD /D " + extendedData.output.dir.getAbsolutePath,
      """copy /Y ..\*.dcm .""",
      setEnv("institution_id", extendedData.institution.name),
      setEnv("machine_configDir", extendedData.output.dir.getAbsolutePath),
      setEnv("machine_id", extendedData.machine.id),
      setEnv("mlc_model", extendedData.multileafCollimator.model),
      setEnv("outputPK", extendedData.output.outputPK.get.toString),
      matlabExecutableFile.getAbsolutePath
    )

    val commandFileContent = commandList.mkString("", System.lineSeparator(), System.lineSeparator())
    Util.writeFile(commandFile, commandFileContent)

    val fileInputStream = new FileInputStream(commandFile)

    val pb = Process(Seq("cmd.exe")) #< fileInputStream
    val processLogger = new StdLogger(extendedData.output)
    logger.info("Running LOC Matlab executable...")
    val start = System.currentTimeMillis()
    pb.run(processLogger, connectInput = true)
    val timeout_ms = System.currentTimeMillis() + extendedData.procedure.timeoutInMs
    logger.info("Waiting for LOC Matlab executable to finish.  Timeout in ms: " + timeout_ms)

    val statusFile = new File(extendedData.output.dir, "status.txt")
    while ((System.currentTimeMillis() < timeout_ms) && (!statusFile.canRead)) {
      Thread.sleep(500)
    }
    // pb.wait(extendedData.procedure.timeoutInMs)
    val elapsed = System.currentTimeMillis() - start
    logger.info("LOC Matlab executable finished in " + elapsed + " ms.")
  }

  /**
    * Put the contents of the Matlab log file into the system log.
    *
    * @param outputDir Directory that contains Matlab log file.
    */
  private def logMatlabLog(outputDir: File): Unit = {
    val file = new File(outputDir, StdLogger.LOG_TEXT_FILE_NAME)
    try {
      logger.info("Matlab log:\n" + Util.readTextFile(file).right.get)
    } catch {
      case t: Throwable =>
        logger.warn("Unable to read Matlab log file " + file.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
    * After the Matlab program has run, it should produce a 'status.txt' file containing a text version of
    * the status.  If the program was successful, this should be 'done'.
    * @param outputDir Status file should be here.
    * @return Matlab status.
    */
  private def getMatlabProgramStatus(outputDir: File): ProcedureStatus.Value = {

    val status = {
      try {
        val statusText = {
          val result = Util.readTextFile(new File(outputDir, "status.txt"))
          result.right.get
        }
        val sts = ProcedureStatus.stringToProcedureStatus(statusText)
        sts.get
      } catch {
        case t: Throwable =>
          logger.error("Unexpected exception while running LOC Matlab: " + fmtEx(t))
          ProcedureStatus.crash
      }
    }
    status
  }

  /**
    * Execute the MATLAB code as a separate process.  Log the output to the AQA log, and return the status.
    * @param extendedData Meta data of input DICOM files.
    * @return Procedure status
    */
  def executeMatlab(extendedData: ExtendedData): ProcedureStatus.Value = {
    execute(extendedData)
    val outputDir = extendedData.output.dir
    logMatlabLog(outputDir)
    getMatlabProgramStatus(outputDir)

  }

}
