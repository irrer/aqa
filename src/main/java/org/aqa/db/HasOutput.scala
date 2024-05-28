package org.aqa.db

/**
 * Allow classes to indicate that they contain an Output.
 *
 * This is used to associate Output rows with history values, which is in turn used for CSV downloads.
 */
abstract class HasOutput {
  def getOutput: Output
}
