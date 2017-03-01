package org.aqa.db

/**
 * Define roles of users, each of which authorizes them to do do various things.
 */
object MaintenanceType extends Enumeration {

    val software = Value // software modification
    val mechanical = Value // physical modifications, including replacing of parts
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

        println("equality ==     : " + (stringToMaintenanceType("mechanical").get == MaintenanceType.mechanical))
        println("equality eq     : " + (stringToMaintenanceType("mechanical").get.eq(MaintenanceType.mechanical)))
        println("equality equals : " + (stringToMaintenanceType("mechanical").get.equals(MaintenanceType.mechanical)))

        values.toArray.toList.map(ur => println("ur: " + ur))

        foo(MaintenanceType.calibration)

        println("stringToMaintenanceType AdMin : " + stringToMaintenanceType("oTHer"))

        println("stringToMaintenanceType foo : " + stringToMaintenanceType("foo"))
    }

}