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

package org.aqa.db

import org.aqa.Config

case class MaintenanceCategory(Name: String, Color: String, Description: String) {
  override def toString: String = "Name: " + Name + "  Color: " + Color + "    Description: " + Description
}

/**
  * Define roles of users, each of which authorizes them to do do various things.
  */
object MaintenanceCategory {

  // standard categories
  val maintenance = "Maintenance"
  val setBaseline = "Set Baseline"
  val firmwareUpdate = "Firmware Update"
  val limit = "Limit"

  /**
    * Find the closest matching maintenance category.
    *
    * TODO : add Smith-Waterman implementation to get best match.  Java code in ImportToUMPlan -> DiffSW.
    */
  def findMaintenanceCategoryMatch(name: String): MaintenanceCategory = {
    Config.MaintenanceCategoryList.find(mc => mc.Name.equalsIgnoreCase(name)) match {
      case Some(mc) => mc
      case _        => Config.MaintenanceCategoryList.head
    }
  }

}
