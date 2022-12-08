package org.aqa.simpleRtPlan

/**
  * Data made by modifying RTPLAN template and the supporting files.
  *
  * @param rtplanText For display to the user.
  * @param zippedContent For download.
  */
case class ModifiedPlan(rtplanText: String, rtplanUID: String, zippedContent: Array[Byte]) {}
