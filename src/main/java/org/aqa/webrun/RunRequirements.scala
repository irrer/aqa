package org.aqa.webrun

import java.io.File

/**
 * RunRequirements are objects that encapsulate data that has been verified but not yet run (procedure).
 */
trait RunRequirements[T] {
  /**
   * The list of the session files that this procedure is using.
   */
  val fileList: IndexedSeq[File]

  /**
   * Create a copy of this with the files referencing to the given directory.
   */
  def reDir(dir: File): T;
}