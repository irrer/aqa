package org.aqa.db

/**
 * Define roles of users, each of which authorizes them to do do various things.
 */
object MaintenanceType extends Enumeration {

  val epid = Value // involved EPID
  val obi = Value // involved onboard imager
  val collimator = Value // involved collimator
  val jaws = Value // involved jaws
  val gantry = Value // involved jaws
  val couch = Value // involved jaws
  val software = Value // software modification or update
  val calibration = Value // tuning machine for improved performance
  val other = Value // what ever does not fit into another category

  /**
   * Convert text to a MaintenanceType.  Is case insensitive.
   */
  def stringToMaintenanceType(text: String): Option[MaintenanceType.Value] = MaintenanceType.values.find(ur => ur.toString.equalsIgnoreCase(text))

  /**
   * For testing only.
   */
  def main(args: Array[String]): Unit = {

    def foo(ur: MaintenanceType.Value): Unit = {
      println("foo ur: " + ur)
    }

    println("equality ==     : " + (stringToMaintenanceType("mechanical").get == MaintenanceType.collimator))
    println("equality eq     : " + (stringToMaintenanceType("mechanical").get.eq(MaintenanceType.collimator)))
    println("equality equals : " + (stringToMaintenanceType("mechanical").get.equals(MaintenanceType.collimator)))

    values.toArray.toList.map(ur => println("ur: " + ur))

    foo(MaintenanceType.calibration)

    println("stringToMaintenanceType AdMin : " + stringToMaintenanceType("oTHer"))

    println("stringToMaintenanceType foo : " + stringToMaintenanceType("foo"))
  }

}