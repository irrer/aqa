/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
