package org.aqa.webrun.LOCBaseline

import edu.umro.util.Utility
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.procedures.UploadTransAndOpen
import org.aqa.Util
import org.aqa.db.Input
import org.aqa.Logging

import java.io.File

object LOCBaselineUtil extends Logging {

  /**
    * Given a machine PK, make sure that the baseline files are available, getting them from the
    * database if necessary.  If it is not possible to get the files, then return false.
    */
  def ensureBaseline(machinePK: Long): Boolean = {

    try {
      val machine = Machine.get(machinePK).get

      def valid(dir: File): Boolean = {
        val ok = dir.isDirectory &&
          new File(dir, UploadTransAndOpen.openName).canRead &&
          new File(dir, UploadTransAndOpen.transName).canRead
        ok
      }

      val machConfValid: Boolean = {
        try {
          val v = valid(machine.configDir.get)
          v
        } catch {
          case _: Throwable => false
        }
      }

      /**
        * Copy the LOC baseline files from the output directory to the machine configuration directory.
        */
      def copyOutput(output: Output) = {
        val machConfigDir = machine.configDir.get
        Util.mkdirs(machConfigDir)

        def copy(name: String): Unit = {
          val buf = Utility.readBinFile(new File(output.dir, name))
          Utility.writeFile(new File(machConfigDir, name), buf)
        }

        copy(UploadTransAndOpen.openName)
        copy(UploadTransAndOpen.transName)
        true
      }

      if (machConfValid) true
      else {
        logger.info("LOC machine configuration not available for " + machine.id)
        Output.getLatestLOCBaselineDir(machinePK, "LOCUploadBaseFiles_1") match {
          // The baseline files exist in the output directory.  Copy them to the machine configuration directory
          case Some((_, output)) if valid(output.dir) => copyOutput(output)

          // The base files exist only in the database.  Re-instate the input and output directories from the
          // database and then copy them to machine configuration directory
          case Some((input, output)) =>
            Input.getFilesFromDatabase(input.inputPK.get, input.dir.getParentFile)
            Output.getFilesFromDatabase(output.outputPK.get, output.dir.getParentFile)
            copyOutput(output)
          case _ => false
        }
      }

    } catch {
      // if anything goes wrong, then the files are not available
      case _: Throwable => false
    }
  }
}
