package org.aqa.webrun

import java.io.File

/**
 * RunRequirements are objects that encapsulate data that has been verified but not yet run (procedure).
 */
trait RunRequirements {
  /**
   * The list of the session files that this procedure is using.
   */
  val fileList: IndexedSeq[File]
}