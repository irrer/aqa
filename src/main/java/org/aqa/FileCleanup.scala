package org.aqa

import org.aqa.db.DbSetup
import org.aqa.db.Machine
import org.aqa.db.Output

import java.io.File

/**
  * Remove old results files.  If the user wants to view them, they can be reinstantiated from the database.
  */
object FileCleanup extends Logging {

  private def resultsCleanup(): Unit = {

    /**
     * Given an output, use the output to identify the directory of a given machine
     * and procedure.  Then clean up all old input directories.
     *
     * @param output Output specifying machine and procedure.
     * @return Number of directories removed.
     */
    def cleanupProcedure(output: Output): Int = {

      // procedure dir
      val dir: Option[File] = {
        if (output.dir.getParentFile.getParentFile.isDirectory)
          Some(output.dir.getParentFile.getParentFile)
        else
          None
      }

      if (dir.isEmpty) {
        0
      } else {
        val old = System.currentTimeMillis() - Config.MinimumDataSetRetentionAge_ms
        val deleteList = {
          val all = Util.listDirFiles(dir.get).sortBy(_.lastModified())
          // Trace.trace("FileCleanup dir: " + dir.get.getAbsolutePath + " : " + all.size)
          all.dropRight(Config.MaximumDataSetRetentionCount).filter(_.lastModified() < old)
        }

        if (deleteList.nonEmpty) {
          logger.info("FileCleanup Dir: " + dir.get.getAbsolutePath + "    Number of dirs to clean up: " + deleteList.size + " \n    " + deleteList.map(_.getAbsolutePath).mkString("\n    "))
        }
        deleteList.foreach(Util.deleteFileTreeSafely)

        deleteList.size
      }

    }

    /**
     * Clean up old results for this machine.
     * @param machinePK Machine of interest.
     * @return Number of directories removed.
     */
    def machineCleanup(machinePK: Long): Int = {
      val list = Output.machineProcedureList(machinePK)
      val count = list.map(cleanupProcedure).sum
      count
    }

    // Only do this if the configuration says so.
    if (Config.MaximumDataSetRetentionCount > 0) {
      val machinePKList = Machine.list.map(_.machinePK.get)
      val count = machinePKList.map(machineCleanup).sum
      logger.info(s"Number of results directories cleaned up: $count")
    }
  }

  /**
    * Remove old and unneeded files.
    */
  private def fileCleanup(): Unit = {
    val start = System.currentTimeMillis()
    logger.info("Cleaning up (removing) oldest results files.")
    resultsCleanup()
    val elapsed = System.currentTimeMillis() - start
    logger.info("Done cleaning up (removing) oldest results files.  Elapsed time: " + Util.elapsedTimeHumanFriendly(elapsed))
  }

  /**
    * Run the cleanup process in another thread.  Delay before starting to permit other startup processes to
    * complete first.  This is a lower priority task so it makes sense to de-prioritize it.
    *
    * @param delay_ms Wait this long (in ms) before starting.
    */
  def cleanupThread(delay_ms: Long): Unit = {
    class CleanFiles extends Runnable {
      override def run(): Unit = {
        Thread.sleep(delay_ms)
        TmpCleanup.cleanup() // clean up old temporary files
        fileCleanup() // clean up old results files
      }
    }

    new Thread(new CleanFiles).start()
  }

  def main(args: Array[String]): Unit = {

    println("validate: " + Config.validate) // loading forces configuration to be read
    DbSetup.init
    println("-----------------------------------------------------------------------")

    resultsCleanup()
  }

}
