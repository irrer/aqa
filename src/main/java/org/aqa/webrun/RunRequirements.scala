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
