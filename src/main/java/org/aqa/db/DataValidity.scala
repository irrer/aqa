package org.aqa.db

/**
 * Define whether data should be considered valid or otherwise.
 */
object DataValidity extends Enumeration {

  val valid = Value // good value
  val invalid = Value // bad value.  Possible error in setup or calculation.

  /**
   * Convert text to a Validity.  Is case insensitive.
   */
  def stringToDataValidity(text: String): Option[DataValidity.Value] = DataValidity.values.find(ur => ur.toString.equalsIgnoreCase(text))
}