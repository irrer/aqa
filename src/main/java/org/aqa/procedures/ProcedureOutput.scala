package org.aqa.procedures

import scala.xml.Elem

/**
 * Describe a database table that represents data output by a procedure.
 */
trait ProcedureOutput {
  /** Identifies the top level XML tag for procedure output. */
  val topXmlLabel: String;

  def contains(elem: Elem): Boolean = {
    (elem \ topXmlLabel).headOption.isDefined
  }

  def insert(elem: Elem, outputPK: Long): Int;
}
