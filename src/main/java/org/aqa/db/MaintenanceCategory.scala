package org.aqa.db

import org.aqa.Config

case class MaintenanceCategory(Name: String, Color: String, Description: String) {
  override def toString = "Name: " + Name + "  Color: " + Color + "    Description: " + Description
}

/**
 * Define roles of users, each of which authorizes them to do do various things.
 */
object MaintenanceCategory {

  // standard categories
  val maintenance = "Maintenance"
  val setBaseline = "Set Baseline"
  val firmwareUpdate = "Firmware Update"

  /**
   * Find the closest matching maintenance category.
   *
   * TODO : add Smith-Waterman implementation to get best match.  Java code in ImportToUMPlan -> DiffSW.
   */
  def findMaintenanceCategoryMatch(name: String): MaintenanceCategory = {
    Config.MaintenanceCategoryList.find(mc => mc.Name.equalsIgnoreCase(name)) match {
      case Some(mc) => mc
      case _ => Config.MaintenanceCategoryList.head
    }
  }

}