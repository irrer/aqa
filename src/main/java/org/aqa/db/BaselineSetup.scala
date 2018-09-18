package org.aqa.db

/**
 * How the baseline was created.
 */
object BaselineSetup extends Enumeration {

  /** When a baseline value is needed, but none has been established, a procedure may default to the values from the given data set.*/
  val byDefault = Value

  /** When a user explicitly decides that a data value is to be used as a baseline. */
  val chosen = Value

  /**
   * Given a string get the corresponding <code>BaselineSetup</code>.
   */
  def stringToBaselineSetup(name: String): Option[BaselineSetup.Value] = {
    val matches = BaselineSetup.values.filter(s => name.equalsIgnoreCase(s.toString)).toList
    if (matches.isEmpty) None else Some(matches.head)
  }
}