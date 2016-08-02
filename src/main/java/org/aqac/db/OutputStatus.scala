package org.aqac.db

/**
 * Define roles of users, each of which authorizes them to do do various things.
 */
object OutputStatus extends Enumeration {

    val valid = Value // good value
    val invalid = Value // bad value.  Possible error in setup or calculation. 

    /**
     * Convert text to a OutputStatus.  Is case insensitive.
     */
    def stringToOutputStatus(text: String): Option[OutputStatus.Value] = OutputStatus.values.find(ur => ur.toString.equalsIgnoreCase(text))

}