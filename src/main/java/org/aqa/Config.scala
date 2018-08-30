package org.aqa

import java.io.File
import edu.umro.util.Log
import scala.xml.Elem
import scala.xml.XML
import java.awt.Color
import scala.collection.mutable.ResizableArray
import scala.collection.mutable.ArrayBuffer
import edu.umro.util.OpSys
import scala.xml.Node
import java.text.SimpleDateFormat
import org.restlet.data.Warning
import java.text.ParseException
import java.util.Date
import scala.xml.NodeSeq
import java.net.InetAddress
import java.util.Properties
import edu.umro.util.Utility
import org.aqa.webrun.phase2.SymmetryAndFlatnessPoint

/**
 * This class extracts configuration information from the configuration file.  Refer
 * to <code>Config.configFileName</code> for details indicating what the different
 * configuration values are used for.
 */
object Config extends Logging {

  private val configFileName = "AQAConfig.xml";
  private val DEFAULT_RESTART_TIME = "1:20"
  private val PenumbraThresholdPercentDefault = 80.0

  logger.info("Starting configuration.  File name: " + configFileName)

  /** Root directory name for static directory. */
  val staticDirName = "static"

  /** Root directory name for test results. */
  val resultsDirName = "results"

  /** Root directory name for temporary files. */
  val tmpDirName = "tmp"

  /** Root directory name for machine configuration files. */
  val machineConfigurationDirName = "MachineConfiguration"

  private def fail(msg: String) {
    logger.error(msg)
    throw new RuntimeException(msg)
  }

  def makeDataDir(dirName: String): File = {
    val dir = new File(DataDir, dirName)
    dir.mkdirs
    dir
  }

  lazy val resultsDirFile = makeDataDir(resultsDirName)

  /** Directory for files shared by multiple machines, possibly multiple institutions. */
  lazy val sharedDir = {
    val f = new File(resultsDirFile, "shared")
    f.mkdirs
    f
  }

  lazy val tmpDirFile = makeDataDir(tmpDirName)

  lazy val machineConfigurationDirFile = makeDataDir(machineConfigurationDirName)

  private var configFile: File = null

  def getConfigFile = configFile

  // Search these directories in the order given to find the configuration file.
  private lazy val directoryList: List[File] = {

    val resourceDirName = """src\main\resources"""
    List(
      Util.thisJarFile.getParentFile, // same directory as jar
      new File(System.getProperty("user.dir")), // current directory
      new File(Util.thisJarFile.getParentFile.getParentFile, resourceDirName), // for development
      new File(resourceDirName) // for development
    )
  }

  /**
   * Read the configuration file.
   *
   * @param dir: Directory from which to read configuration file.
   *
   * @return DOM of configuration, or nothing on failure
   */
  private def readFile(dir: File, name: String): Option[Elem] = {
    val file = new File(dir, name)
    logger.info("Trying config file " + file.getAbsolutePath + " ...")
    if (file.canRead) {
      try {
        val content = Some(XML.loadFile(file))
        logger.info("Using config file " + file.getAbsolutePath)
        configFile = file
        content
      } catch {
        case e: Exception => {
          logger.info("Failed to use config file " + file.getAbsolutePath + "    file exists: " + file.exists + "    can read file: " + file.canRead + "  Exception: " + e)
          None
        }
      }
    } else {
      if (!file.exists) logger.info("Config file " + file.getAbsoluteFile + " does not exist")
      else logger.info("Config file " + file.getAbsoluteFile + " is not readable")
      None
    }
  }

  /**
   * If a fatal error occurs during the reading of the configuration file, then the application
   * is toast, so log an error and exit with a failed status.
   */
  private def epicFail(name: String) = {
    val tried = directoryList.foldLeft("")((l, f) => l + "\n    " + f.getAbsolutePath)

    logger.error("Could not find a usable configuration file.  Using file name " + name + " , tried directories: " + tried + "\nShutting down...")
    System.exit(1)
  }

  private def getDoc(dirList: List[File], name: String): Option[Elem] = {
    if (dirList.length < 1) None
    val elem = readFile(dirList.head, name)
    elem match {
      case Some(el) => elem
      case _ => getDoc(dirList.tail, name)
    }
  }

  private def getJavaKeyStorePassword: String = {
    val name = "JavaKeyStorePassword"
    try {
      val value = getMainText(name)
      logText(name, "[redacted]")
      value
    } catch {
      case _: Throwable => {
        logText(name, "[not configured]")
        ""
      }
    }
  }

  private def getJavaKeyStoreFileList: List[File] = {
    val name = "JavaKeyStoreFileList"
    try {
      val list = (document \ name \ "JavaKeyStoreFile").toList.map(node => new File(node.head.text))
      list.map(jksf => logText("JavaKeyStoreFile", jksf.getAbsolutePath))
      list
    } catch {
      case _: Throwable => {
        logText(name, "[not configured]")
        List[File]()
      }
    }
  }

  private val document: Elem = {
    val doc = getDoc(directoryList, configFileName)
    doc match {
      case Some(d) => d
      case _ => {
        epicFail(configFileName)
        null
      }
    }

  }

  // Commented out because of the potential security risk of exposing passwords.
  //logger.trace("Using configuration:\n" + edu.umro.ScalaUtil.Util.xmlToText(document) + "\n")

  private val valueText = new ArrayBuffer[String]

  private def logText(name: String, value: String) = valueText += (name + ": " + value)

  private def logTextNotSpecified(name: String) = logText(name, "[not specified]")

  private def getMainText(name: String): String = {
    val list = document \ name
    if (list.isEmpty) fail("No such XML node " + name)

    def forThisHost(node: Node) = {
      val attr = (node \ "@HostIp").headOption
      attr.isDefined && attr.get.text.equalsIgnoreCase(OpSys.getHostIPAddress)
    }

    def noHostGiven(node: Node) = (node \ "@HostIp").headOption.isEmpty

    val thisHost = list.filter(n => forThisHost(n)).headOption

    val noHost = list.filter(n => noHostGiven(n)).headOption

    (thisHost, noHost) match {
      case (Some(th), _) => th.text
      case (_, Some(nh)) => nh.text
      case _ => {
        val msg = "No such XML node " + name
        fail(msg)
        msg
      }
    }
  }

  private def getMainTextOption(name: String): Option[String] = {
    try {
      Some(getMainText(name))
    } catch {
      case t: Throwable => None
    }
  }

  private def logMainText(name: String): String = {
    println("Name: " + name) // TODO rm
    val value = getMainText(name)
    logText(name, value)
    value
  }

  private def getDir(name: String): File = {
    val dir = new File(getMainText(name))
    logText(name, dir.getAbsolutePath)
    dir.mkdirs
    dir
  }

  private def getThisJarFile: File = {
    // Assume we are in the development environment and get the latest jar
    def getDevJar: File = {
      def cmprFileTime(a: File, b: File): Boolean = a.lastModified > b.lastModified
      val timeSortedList = (new File("target")).listFiles.sortWith((a, b) => cmprFileTime(a, b))
      timeSortedList.filter(f => f.getName.matches("^AQA.*jar-with-dependencies.jar$")).headOption match {
        case Some(f) => f
        case _ => {
          val f = new File("target/AQA-0.0.1-jar-with-dependencies.jar")
          logger.warn("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath)
          f
        }
      }
    }
    val jf: File = Util.thisJarFile match {
      case f if f.isDirectory => getDevJar
      case f if (f.isFile && f.canRead) => f
      case f => { logger.warn("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath); f }
    }
    val fileDate = if (jf.canRead) " : " + (new Date(jf.lastModified)) else "  can not read file"
    logText("jarFile", jf.getAbsolutePath + fileDate)
    jf
  }

  val HTTPSPort: Option[Int] = {
    val name = "HTTPSPort"
    try {
      val securePort = getMainText(name).toInt
      logText(name, securePort.toString)
      Some(securePort)
    } catch {
      case _: Throwable => {
        logTextNotSpecified(name)
        None
      }
    }

  }

  val JavaKeyStorePassword = getJavaKeyStorePassword
  val JavaKeyStoreFileList = getJavaKeyStoreFileList

  val ProgramDir = getDir("ProgramDir")
  val ProcedureDir = getDir("ProcedureDir")
  val DataDir = getDir("DataDir")
  val machineConfigurationDir = new File(DataDir, machineConfigurationDirName)
  machineConfigurationDir.mkdirs

  val RestletLogLevel = logMainText("RestletLogLevel")
  val AuthenticationTimeout = logMainText("AuthenticationTimeout").toDouble
  val AuthenticationTimeoutInMs = (AuthenticationTimeout * 1000).toLong

  val jarFile = getThisJarFile

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime: Long = {
    val dateFormat = new SimpleDateFormat("HH:mm")
    val millisec = try {
      dateFormat.parse(logMainText("RestartTime")).getTime
    } catch {
      case e: ParseException => {
        Log.get.warning("Badly formatted RestartTime in configuration file: " + logMainText("RestartTime") + " .  Should be HH:MM, as in 1:23 .  Assuming default of " + DEFAULT_RESTART_TIME)
        dateFormat.parse(DEFAULT_RESTART_TIME).getTime
      }
    }

    millisec
  }

  private def getDbUser: Option[String] = {
    val name = "SlickDbsDefaultDbUser"
    getMainTextOption(name) match {
      case Some(user) => {
        logText(name, user)
        Some(user)
      }
      case _ => {
        logTextNotSpecified(name)
        None
      }
    }
  }

  /**
   * Get the database password if it exists.
   */
  private def getDbPassword: Option[String] = {
    val name = "SlickDbsDefaultDbPassword"
    try {
      val password = getMainText(name)
      logText(name, "[redacted]")
      Some(password)
    } catch {
      case t: Throwable => {
        logTextNotSpecified(name)
        None
      }
    }
  }

  private def getMetadataCheckBeamNameList = {
    val list = (document \ "MetadataCheckBeamNameList" \ "BeamName").map(n => n.head.text.toString).toList
    logText("MetadataCheckBeamNameList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  private def getCenterDoseBeamNameList = {
    val list = (document \ "CenterDoseBeamNameList" \ "BeamName").map(n => n.head.text.toString).toList
    logText("CenterDoseBeamNameList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  private def getWedgeBeamList = {
    val list = (document \ "WedgeBeamList" \ "BeamName").map(n => n.head.text.toString).toList
    logText("WedgeBeamList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  private def getSymmetryAndFlatnessBeamList = {
    val list = (document \ "SymmetryAndFlatnessBeamList" \ "BeamName").map(n => n.head.text.toString).toList
    logText("SymmetryAndFlatnessBeamList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  private def getSymmetryAndFlatnessPointList: Seq[SymmetryAndFlatnessPoint] = {
    def makePoint(node: Node): SymmetryAndFlatnessPoint = {
      val name = (node \ "@name").head.text.toString
      val x_mm = (node \ "@x_mm").head.text.toString.toDouble
      val y_mm = (node \ "@y_mm").head.text.toString.toDouble
      new SymmetryAndFlatnessPoint(name, x_mm, y_mm)
    }

    val list = (document \ "SymmetryAndFlatnessPointList" \ "SymmetryAndFlatnessPoint").map(n => makePoint(n)).toList.toSeq
    logText("SymmetryAndFlatnessPointList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  case class CollimatorPositionBeamConfig(beamName: String, FloodCompensation: Boolean) {
    override def toString = "Beam name: " + beamName + "  FloodCompensation: " + FloodCompensation
  }

  private def getCollimatorPositionBeamList = {
    def nodeToCollimatorPositionBeam(node: Node) = {
      val beamName = node.head.text.toString
      val FloodCompensation = {
        try {
          (node \ "@FloodCompensation").head.text.toString.toLowerCase.toBoolean
        } catch {
          case t: Throwable => false
        }
      }
      new CollimatorPositionBeamConfig(beamName, FloodCompensation)
    }
    val list = (document \ "CollimatorPositionBeamList" \ "BeamName").map(node => nodeToCollimatorPositionBeam(node)).toList
    logText("CollimatorPositionBeamNameList", list.mkString("\n        ", "\n        ", "\n"))
    list
  }

  /**
   * Get the PenumbraThresholdPercent.  If it is not valid in the configuration, then assume the default.
   */
  private def getPenumbraThresholdPercent = {
    val name = "PenumbraThresholdPercent"
    val ptp = logMainText(name).toDouble
    if ((ptp > 0) && (ptp < 100)) ptp
    else {
      logText(name, "Invalid value, must be greater than 0 and less than 100.  Assuming default value of " + PenumbraThresholdPercentDefault)
      PenumbraThresholdPercentDefault
    }
  }

  val SlickDbsDefaultDbUrl = logMainText("SlickDbsDefaultDbUrl")
  val SlickDbsDefaultDbUser = getDbUser
  val SlickDbsDefaultDbPassword = getDbPassword
  val SlickDbsDefaultDriver = logMainText("SlickDbsDefaultDriver")
  val SlickDbsDefaultDbDriver = logMainText("SlickDbsDefaultDbDriver")

  val UserWhiteList: List[String] = (document \ "UserWhiteList" \ "User").toList.map(node => node.head.text.trim.toLowerCase)

  /** Symbols for pass and fail. */
  val passImageUrl = "/static/images/pass.png"
  val failImageUrl = "/static/images/fail.png"

  val MetadataCheckBeamNameList: List[String] = getMetadataCheckBeamNameList

  val FloodFieldBeamName = logMainText("FloodFieldBeamName")

  val CollimatorCenteringTolerence_mm = logMainText("CollimatorCenteringTolerence_mm").toDouble
  val CollimatorCentering090BeamName = logMainText("CollimatorCentering090BeamName")
  val CollimatorCentering270BeamName = logMainText("CollimatorCentering270BeamName")
  val CollimatorCenteringCoarseBandWidth_mm = logMainText("CollimatorCenteringCoarseBandWidth_mm").toDouble
  val PenumbraThickness_mm = logMainText("PenumbraThickness_mm").toDouble
  val PenumbraPlateauPixelsPerMillion = logMainText("PenumbraPlateauPixelsPerMillion").toInt
  val PenumbraThresholdPercent = getPenumbraThresholdPercent

  val MaxEstimatedBadPixelPerMillion = logMainText("MaxEstimatedBadPixelPerMillion").toInt
  val BadPixelSamplePerMillion = logMainText("BadPixelSamplePerMillion").toInt
  val BadPixelStdDev = logMainText("BadPixelStdDev").toDouble
  val MaxAllowedBadPixelsPerMillion = logMainText("MaxAllowedBadPixelsPerMillion").toInt

  val MaxProcedureDuration = logMainText("MaxProcedureDuration").toDouble

  val TermsOfUse = logMainText("TermsOfUse")

  private def requireReadableDirectory(name: String, dir: File) = {
    if (!dir.canRead) fail("Directory " + name + " is not readable: " + dir)
    if (!dir.isDirectory) fail("Directory " + name + " is required but is not a directory: " + dir)
  }

  // force each of these directories to be usable.  It would be catastrophic if this fails.
  requireReadableDirectory("resultsDirFile", resultsDirFile)
  requireReadableDirectory("tmpDirFile", tmpDirFile)
  requireReadableDirectory("machineConfigurationDirFile", machineConfigurationDirFile)

  val CenterDoseRadius_mm = logMainText("CenterDoseRadius_mm").toDouble
  val CenterDoseReportedHistoryLimit = logMainText("CenterDoseReportedHistoryLimit").toInt
  val CenterDoseBeamNameList = getCenterDoseBeamNameList

  val CollimatorPositionTolerance_mm = logMainText("CollimatorPositionTolerance_mm").toDouble
  val CollimatorPositionBeamList = getCollimatorPositionBeamList

  val WedgeBeamList = getWedgeBeamList

  val SymmetryAndFlatnessDiameter_mm = logMainText("SymmetryAndFlatnessDiameter_mm").toDouble

  val SymmetryAndFlatnessBeamList = getSymmetryAndFlatnessBeamList

  val SymmetryAndFlatnessPointList = getSymmetryAndFlatnessPointList

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = {
    validated
  }

  override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

  def toHtml = {
    { valueText.map(line => <br>{ line } </br>) }.toSeq
  }

  logger.info("Configuration has been validated.")
  logger.info(toString)

  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis
    println("validate: " + validate) // loading forces configuration to be read
    val elapsed = System.currentTimeMillis - startTime
    println("Elapsed time in ms: " + elapsed)
    System.exit(99)
  }
}

