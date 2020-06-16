package learn.dbConnect

import java.util.Properties
import resource.managed
import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML
import java.sql.DriverManager

object AQAdb2 {

  //val dllPath = """D:\pf\eclipse\workspaceOxygen\aqa\src\main\dll\x64"""
  val dllPath = """D:\pf\eclipse\workspaceOxygen\VarianDB_2.12\src\main\resources"""
  //val dllPath = """src\main\resources"""
  val dllFileName = "sqljdbc_auth.dll"
  val dllFullPath = dllPath + "\\" + dllFileName

  val jlpName = "java.library.path"
  def jlpToString = System.getProperty(jlpName).split(";").mkString("\n    ", "\n    ", "")

  def findAuthDllJlp = {
    System.getProperty(jlpName).split(";").map(dn => new File(dn)).map(d => new File(d, dllFileName)).filter(f => f.canRead).map(f => println("   jlp  found dll: " + f.getAbsolutePath))
  }

  def findAuthDllPath = {
    System.getenv("path").split(";").map(dn => new File(dn)).map(d => new File(d, dllFileName)).filter(f => f.canRead).map(f => println("   path found dll: " + f.getAbsolutePath))
  }

  def loadDll = {
    println("starting loadDll")
    System.load(dllFullPath)
    println("Loaded " + dllFullPath)
  }

  /**
   * Self-test, not for production use.
   */
  def main(args: Array[String]): Unit = {

    println("starting main")
    println("The tricky part: must be on path and java.library.path")

    findAuthDllJlp
    findAuthDllPath

    if (false) {
      println("path: " + System.getenv("path"))
    }

    if (false) {
      System.getProperties.entrySet.toArray.toSeq.map(prop => println("    " + prop))
    }

    val properties = new Properties
    properties.setProperty("databaseName", "AQAmsDV")
    properties.setProperty("user", "UMHS\\irrer")
    properties.setProperty("password", "45eetslp")
    properties.setProperty("applicationIntent", "ReadOnly")
    properties.setProperty("integratedSecurity", "true")

    val loc = "jdbc:sqlserver://ntsrodbsdv1.umhs.med.umich.edu:1433"
    println("Connecting to database " + this.toString)
    val connection = DriverManager.getConnection(loc, properties)
    println("Made it connection")

    def dbResourceTest: Unit = {
      println("Entered dbResourceTest")

      println("Made connection")
      val query = {
        if (true) {
          val tableName = "FOO"
          val user = "irrer"
          val query = "select * from " + tableName
          query
        } else {
          val tableName = "AppUser"
          val user = """umhs\irrer"""
          val query = "select * from " + tableName + " where ((UserCUID = '" + user + " COLLATE SQL_Latin1_General_CP1_CI_AS') OR (UserId = '" + user + "' COLLATE SQL_Latin1_General_CP1_CI_AS))"
          query
        }
      }

      println("Using query: " + query)
      val statement = connection.createStatement

      val resultSet = statement.executeQuery(query)

      while (resultSet.next) {
        println(resultSet.getString(1) + " : " + resultSet.getString(2))
        println("basic test success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      }

    }

    try {
      dbResourceTest
      //isAriaUserTest
      System.exit(99)
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

}
