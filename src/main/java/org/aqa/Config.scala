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

package org.aqa

import com.pixelmed.dicom.AttributeTag
import edu.umro.ImageUtil.Watermark
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.OpSys
import edu.umro.ScalaUtil.Util.daysToMs
import org.aqa.db.MaintenanceCategory
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessPoint
import org.aqa.webrun.wl.WLTreatmentMachine

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.File
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Semaphore
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

/**
  * This class extracts configuration information from the configuration file.  Refer
  * to <code>Config.configFileName</code> for details indicating what the different
  * configuration values are used for.
  */
object Config extends Logging {

  private val configFileName = "AQAConfig.xml"
  private val DEFAULT_RESTART_TIME = "1:15"

  logger.info("Starting configuration.  File name: " + configFileName)

  /** Root directory name for static directory. */
  val staticDirName = "static"

  /** Root directory name for test results. */
  val resultsDirName = "results"

  /** Root directory name for temporary files. */
  val tmpDirName = "tmp"

  /** Root directory name for cached files. */
  val cacheDirName = "cache"

  /** For indenting sub-content. */
  private val indent1 = "\n                  "

  private def indentList[T](list: Seq[T]): String = list.mkString(indent1, indent1, "\n")

  /** Root directory name for machine configuration files. */
  val machineConfigurationDirName = "MachineConfiguration"

  private def fail(msg: String): Unit = {
    logger.error(msg)
    throw new RuntimeException(msg)
  }

  private def makeDataDir(dirName: String): File = {
    val dir = new File(DataDir, dirName)
    Util.mkdirs(dir)
    dir
  }

  lazy val resultsDirFile: File = makeDataDir(resultsDirName)

  /** Directory for files shared by multiple machines, possibly multiple institutions. */
  lazy val sharedDir: File = {
    val f = new File(resultsDirFile, "shared")
    Util.mkdirs(f)
    f
  }

  lazy val tmpDirFile: File = makeDataDir(tmpDirName)

  lazy val cacheDirFile: File = makeDataDir(cacheDirName)

  lazy val machineConfigurationDirFile: File = makeDataDir(machineConfigurationDirName)

  private var configFile: Option[File] = None

  def getConfigFile: File = configFile.get

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
    * @param dir : Directory from which to read configuration file.
    * @return DOM of configuration, or nothing on failure
    */
  private def readFile(dir: File, name: String): Option[Elem] = {
    val file = new File(dir, name)
    logger.info("Trying config file " + file.getAbsolutePath + " ...")
    if (file.canRead) {
      try {
        val content = Some(XML.loadFile(file))
        logger.info("Using config file " + file.getAbsolutePath)
        configFile = Some(file)
        content
      } catch {
        case e: Exception =>
          logger.info("Failed to use config file " + file.getAbsolutePath + "    file exists: " + file.exists + "    can read file: " + file.canRead + "  Exception: " + e)
          None
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
  private def epicFail(name: String): Unit = {
    val tried = indentList(directoryList.map(d => d.getAbsolutePath))

    logger.error("Could not find a usable configuration file.  Using file name " + name + " , tried directories: " + tried + "\nShutting down...")
    System.exit(1)
  }

  @tailrec
  private def getDoc(dirList: List[File], name: String): Option[Elem] = {
    val elem = readFile(dirList.head, name)
    elem match {
      case Some(_) => elem
      case _       => getDoc(dirList.tail, name)
    }
  }

  /**
    * Get the list of possible passwords to use for the java key store for serving web pages via HTTPS.  If not configured, then return an empty list.
    */
  private def getJavaKeyStorePasswordList: List[String] = {
    val name = "JavaKeyStorePassword"
    try {
      val passwordList = (document \ name).map(n => n.head.text)
      logText(name, "[redacted]")
      (passwordList ++ passwordList.map(pw => pw.trim)).distinct.toList
    } catch {
      case _: Throwable =>
        logText(name, "[not configured]")
        List[String]()
    }
  }

  private def getJavaKeyStoreFileList: List[File] = {
    val name = "JavaKeyStoreFileList"
    try {
      val list = (document \ name \ "JavaKeyStoreFile").toList.map(node => new File(node.head.text))
      list.map(jk => logText("JavaKeyStoreFile", jk.getAbsolutePath))
      list
    } catch {
      case _: Throwable =>
        logText(name, "[not configured]")
        List[File]()
    }
  }

  private val document: Elem = {
    val doc = getDoc(directoryList, configFileName)
    doc match {
      case Some(d) => d
      case _ =>
        epicFail(configFileName)
        null
    }

  }

  // Commented out because of the potential security risk of exposing passwords.
  //logger.trace("Using configuration:\n" + edu.umro.ScalaUtil.Util.xmlToText(document) + "\n")

  private val valueText = {
    val vt = new ArrayBuffer[String]
    vt +=
      "Many configuration values have a default value that is used if the value is not specified in the configuration file.  Example log messages of configuration values: \n" +
        "        AuthenticationTimeout (same as default): 7200.0             <== means that there was a default value and the value was given in the configuration file and they were the same.\n" +
        "        BBbyEPIDSearchDistance_mm (default: 10.0 overridden): 4.0   <== means that there was a default value and the value was given in the configuration file and they were different.\n" +
        "        BBbyCBCTHistoryRange (defaulted): 1000000                   <== means that there was a default value but the value was not given in the configuration file so the default value was used.\n\n"
    vt
  }

  private def logText(name: String, value: String) = valueText += (name + ": " + value)

  private def logTextNotSpecified(name: String) = logText(name, "[not specified]")

  private def getMainText(name: String): String = {
    logger.info("Getting attribute: " + name)
    val list = document \ name
    if (list.isEmpty) fail("No such XML node " + name)

    def forThisHost(node: Node) = {
      val attr = (node \ "@HostIp").headOption
      attr.isDefined && attr.get.text.equalsIgnoreCase(OpSys.getHostIPAddress)
    }

    def noHostGiven(node: Node) = (node \ "@HostIp").headOption.isEmpty

    val thisHost = list.find(n => forThisHost(n))

    val noHost = list.find(n => noHostGiven(n))

    (thisHost, noHost) match {
      case (Some(th), _) => th.text
      case (_, Some(nh)) => nh.text
      case _ =>
        val msg = "No such XML node " + name
        fail(msg)
        msg
    }
  }

  private def getMainTextOption(name: String): Option[String] = {
    try {
      Some(getMainText(name))
    } catch {
      case _: Throwable => None
    }
  }

  //  private def logMainText(name: String): String = {
  //    val value = getMainText(name)
  //    logText(name, value)
  //    value
  //  }

  /**
    * Get the value matching the given name.  If it does not exist or there is
    * some sort of other problem then return the default.
    */
  private def logMainText(name: String, default: String): String = {
    getMainTextOption(name) match {
      case Some(value: String) =>
        val compare =
          if (default.equals(value))
            " (same as default)"
          else
            " (default: " + default + " overridden)"
        logText(name + compare, value)
        value
      case _ =>
        logText(name + " (defaulted)", default)
        default
    }
  }

  private def getDir(name: String): File = {
    val dir = new File(getMainText(name))
    logText(name, dir.getAbsolutePath)
    Util.mkdirs(dir)
    dir
  }

  private def getThisJarFile: File = {
    // Assume we are in the development environment and get the latest jar
    def getDevJar: File = {
      def compareFileTime(a: File, b: File): Boolean = a.lastModified > b.lastModified

      val timeSortedList = new File("target").listFiles.sortWith((a, b) => compareFileTime(a, b))
      timeSortedList.find(f => f.getName.matches("^AQA.*jar-with-dependencies.jar$")) match {
        case Some(f) => f
        case _ =>
          val f = new File("target/AQA-0.0.1-jar-with-dependencies.jar")
          logger.warn("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath)
          f
      }
    }

    val jf: File = Util.thisJarFile match {
      case f if f.isDirectory         => getDevJar
      case f if f.isFile && f.canRead => f
      case f                          => logger.warn("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath); f
    }
    val fileDate = if (jf.canRead) " : " + new Date(jf.lastModified) else "  can not read file"
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
      case _: Throwable =>
        logTextNotSpecified(name)
        None
    }
  }

  val HTTPPort: Int = logMainText("HTTPPort", "80").toInt

  /**
    * Get the list of allowed IP addresses.  If empty, allow everything.
    */
  private def getAllowedHttpIpList: List[String] = {
    val nodeList = document \ "AllowedHttpIpList" \ "AllowedHttpIp"
    val allowedList = nodeList.toList.map(n => n.head.text.trim)
    logText("AllowedHttpIpList", allowedList.mkString("\n    ", "\n    ", "\n    "))
    allowedList
  }

  /**
    * Get the URL of the LDAP service to use.  If this is not given in the
    * configuration file, then disable LDAP.
    */
  private def getLdapUrl: Option[String] = {
    val tag = "LdapUrl"
    try {
      val url = (document \ tag).head.text.trim
      logText(tag, url)
      Some(url)
    } catch {
      case _: Throwable => None
    }
  }

  private def getLdapGroupList: Seq[String] = {
    val groupTag = "LdapGroupList"
    val tag = "LdapGroup"
    try {
      val groupList = (document \ groupTag \ tag).map(n => n.text.trim)
      logText(groupTag, groupList.mkString("\n        ", "\n        ", ""))
      groupList
    } catch {
      case _: Throwable => Seq[String]()
    }
  }

  /**
    * List of IP addresses allowed to access this server.
    */
  val AllowedHttpIpList: List[String] = getAllowedHttpIpList

  val MinPasswordSize: Int = logMainText("MinPasswordSize", "8").toInt

  val JavaKeyStorePasswordList: List[String] = getJavaKeyStorePasswordList
  val JavaKeyStoreFileList: List[File] = getJavaKeyStoreFileList

  val LdapUrl: Option[String] = getLdapUrl

  val RestletMinThreads: Int = logMainText("RestletMinThreads", "5").toInt
  val RestletMaxThreads: Int = logMainText("RestletMaxThreads", "20").toInt
  private val MonitorThreadCountInterval_sec: Double = logMainText("MonitorThreadCountInterval_sec", "0.0").toDouble
  val MonitorThreadCountInterval_ms: Long = (MonitorThreadCountInterval_sec * 1000).round

  val LdapInstitutionName: String = logMainText("LdapInstitutionName", "not specified")
  val LdapRole: String = logMainText("LdapRole", "not specified")
  val LdapGroupList: Seq[String] = getLdapGroupList

  val MaxProcedureCount: Int = logMainText("MaxProcedureCount", "1").toInt
  private val MaxProcedureWaitTime_min: Double = logMainText("MaxProcedureWaitTime_min", "10.0").toDouble
  val MaxProcedureWaitTime_ms: Long = (MaxProcedureWaitTime_min * 60 * 1000).round

  /** used to synchronize the running of procedures.  Should only process a maximum number of jobs at a time so as not to overload the hardware. */
  val procedureLock = new Semaphore(MaxProcedureCount)

  val ProgramDir: File = getDir("ProgramDir")
  val ProcedureDir: File = getDir("ProcedureDir")
  val DataDir: File = getDir("DataDir")
  val machineConfigurationDir = new File(DataDir, machineConfigurationDirName)
  Util.mkdirs(machineConfigurationDir)

  val MaximumDataSetRetentionCount: Int = logMainText("MaximumDataSetRetentionCount", "12").toInt
  private val MinimumDataSetRetentionAge_day: Double = logMainText("MinimumDataSetRetentionAge_day", "14.0").toDouble
  val MinimumDataSetRetentionAge_ms: Long = daysToMs(MinimumDataSetRetentionAge_day)

  private val AuthenticationTimeout: Double = logMainText("AuthenticationTimeout", "7200.0").toDouble
  val AuthenticationTimeoutInMs: Long = (AuthenticationTimeout * 1000).toLong

  val PasswordPrompt: String = logMainText("PasswordPrompt", "Please enter your password")

  val jarFile: File = getThisJarFile

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime: Long = {
    val dateFormat = new SimpleDateFormat("HH:mm")
    val milliseconds =
      try {
        dateFormat.parse(logMainText("RestartTime", "3:10")).getTime
      } catch {
        case _: ParseException =>
          logger.warn("Badly formatted RestartTime in configuration file: " + logMainText("RestartTime", "3:10") + " .  Should be HH:MM, as in 1:23 .  Assuming default of " + DEFAULT_RESTART_TIME)
          dateFormat.parse(DEFAULT_RESTART_TIME).getTime
      }
    milliseconds
  }

  val TestMode: Boolean = logMainText("TestMode", "false").toLowerCase.trim.toBoolean

  /** True if in production (not test) mode. */
  val ProductionMode: Boolean = !TestMode

  private def getSlickDb: com.typesafe.config.Config = {
    import com.typesafe.config.ConfigFactory
    val name = "SlickDb"
    val configText = {
      val text = getMainText(name)
      if (text.contains("{")) {
        logger.warn("Should remove slick configuration from config file.")
        text
      } else {
        val t = Util.readTextFile(new File(text)).right.get
        t
      }
    }
    logger.info("Constructing database config")
    val dbConfig = ConfigFactory.parseString(configText)
    logger.info("Constructed database config")
    val content = dbConfig.entrySet.toArray.map(cs => cs.toString).filterNot(cs => cs.toLowerCase.contains("pass")).mkString("\n                  ", "\n                  ", "")
    logger.info("Database configuration (password redacted): " + content)

    logText(name, "Constructed database config:" + content)
    dbConfig
  }

  private def getBeamNameList(category: String): List[String] = {
    val list = (document \ category \ "BeamName").map(n => n.head.text.trim).toList
    logText(category, indentList(list))
    list.distinct
  }

  private def getSymmetryAndFlatnessPointList: Seq[SymmetryAndFlatnessPoint] = {
    def makePoint(node: Node): SymmetryAndFlatnessPoint = {
      val name = (node \ "@name").head.text
      val x_mm = (node \ "@x_mm").head.text.toDouble
      val y_mm = (node \ "@y_mm").head.text.toDouble
      SymmetryAndFlatnessPoint(name, x_mm, y_mm)
    }

    val list = (document \ "SymmetryAndFlatnessPointList" \ "SymmetryAndFlatnessPoint").map(n => makePoint(n)).toList
    logText("SymmetryAndFlatnessPointList", indentList(list))
    list
  }

  private def getSymmetryAndFlatnessPointPairList = {

    val radius = SymmetryAndFlatnessDiameter_mm / 2

    def isPair(a: SymmetryAndFlatnessPoint, b: SymmetryAndFlatnessPoint) = {
      val bReflected = new Point2D.Double(-b.x_mm, -b.y_mm)
      val closeTo = a.asPoint.distance(bReflected) < radius
      val aLower = (a.x_mm < b.x_mm) || (a.y_mm < b.y_mm)
      closeTo && aLower
    }

    val list = for (a <- SymmetryAndFlatnessPointList; b <- SymmetryAndFlatnessPointList; if isPair(a, b)) yield {
      (a, b)
    }
    val listText = indentList(list.map(pair => pair._1.name + "   " + pair._2.name))
    logText("SymmetryAndFlatnessPointList", listText)
    list
  }

  private def getSymPoint(nodeName: String) = {
    val name = (document \ nodeName \ "@name").head.text
    val point = SymmetryAndFlatnessPointList.find(p => p.name.equals(name)).get
    logText(nodeName, point.toString)
    point
  }

  case class CollimatorPositionBeamConfig(beamName: String, FloodCompensation: Boolean) {
    override def toString: String = "Beam name: " + beamName + "  FloodCompensation: " + FloodCompensation
  }

  private def getCollimatorPositionBeamList = {
    def nodeToCollimatorPositionBeam(node: Node) = {
      val beamName = node.head.text.trim
      val FloodCompensation = {
        try {
          (node \ "@FloodCompensation").head.text.toLowerCase.toBoolean
        } catch {
          case _: Throwable => false
        }
      }
      CollimatorPositionBeamConfig(beamName, FloodCompensation)
    }

    val list = (document \ "CollimatorPositionBeamList" \ "BeamName").map(node => nodeToCollimatorPositionBeam(node)).toList
    logText("CollimatorPositionBeamNameList", indentList(list))
    list
  }

  /**
    * Give the name of a wedge beam and a list of beams that are compatible for being the background of the wedge beam.
    *
    * wedge: Beam name for wedge
    *
    * background: List of beam names for background beam for comparison (percent calculation).  The beams are searched in
    * the order given, and the first one found (is present in the input data) is used as the background.
    */
  case class WedgeBeam(wedge: String, backgroundList: Seq[String]) {
    override def toString: String = "Wedge: " + wedge + "    background: " + backgroundList.mkString("    ")
  }

  private def getWedgeBeamList = {

    def nodeToWedgeBeamSet(node: Node) = {
      val beamName = (node \ "@BeamName").head.text.trim
      val background = {
        try {
          (node \ "BackgroundBeamName").map(n => n.head.text)
        } catch {
          case _: Throwable =>
            Seq("not found")
        }
      }
      WedgeBeam(beamName, background)
    }

    val list = (document \ "WedgeBeamList" \ "WedgeBeam").map(node => nodeToWedgeBeamSet(node)).toList
    logText("WedgeBeamList", indentList(list))
    list
  }

  /**
    * Encapsulate the configuration for pair of VMAT beams.
    */
  case class VMATBeamPair(name: String, MLC: String, OPEN: String, IsolationBorder_mm: Double) {
    override def toString: String = name + "    MLC: " + MLC.format("%-14s") + "    OPEN: " + OPEN.format("%-14s") + "    IsolationBorder_mm: " + IsolationBorder_mm.formatted("%6.3f")
  }

  /**
    * Convert VMAT beam pair list from XML to list of <code>VMATBeamPair</code>s.
    */
  private def getVMATBeamPairList: Seq[VMATBeamPair] = {
    def nodeToVMATBeamPair(node: Node) = {
      VMATBeamPair((node \ "@Name").head.text, (node \ "@MLC").head.text, (node \ "@OPEN").head.text, (node \ "@IsolationBorder_mm").head.text.toDouble)
    }

    val list = (document \ "VMATBeamPairList" \ "VMATBeamPair").map(node => nodeToVMATBeamPair(node)).toList
    val asText = list.mkString("\n        ", "\n        ", "")
    logText("VMATBeamPairList", asText)
    list
  }

  private def getMaintenanceCategoryList: List[MaintenanceCategory] = {
    def nodeToMaintenanceCat(node: Node) = {
      new MaintenanceCategory((node \ "@Name").head.text, (node \ "@Color").head.text, node.head.text)
    }

    val list = (document \ "MaintenanceCategoryList" \ "MaintenanceCategory").map(node => nodeToMaintenanceCat(node)).toList
    logText("MaintenanceCategoryList", indentList(list))
    list
  }

  /**
    * Directory containing the definitive static files.
    */
  val staticDirFile: File = {
    val locations = List(""".\""", """src\main\resources\""").map(name => new File(name + Config.staticDirName))

    val dirList = locations.filter(f => f.isDirectory)

    if (dirList.isEmpty) {
      val fileNameList = locations.foldLeft("")((t, f) => t + "    " + f.getAbsolutePath)
      val msg = "Unable to find static directory in " + fileNameList
      logger.error(msg)
      throw new RuntimeException(msg)
    }
    logger.info("Using static directory " + dirList.head.getAbsolutePath)
    dirList.head
  }

  val imageDirFile = new File(staticDirFile, "images")
  private val rtplanDirFile = new File(staticDirFile, "rtplan")

  /**
    * Get the directory containing the DICOM file used as a template to make a simple RTPLAN.
    *
    * @return Directory, if it exists.
    */
  private def getSimpleRtplanTemplateDir: Option[File] = {
    val tagName = "SimpleRtplanTemplateDir"
    val rtplanFileName = getMainTextOption(tagName)
    val directory = if (rtplanFileName.isDefined) {
      val dir = new File(rtplanDirFile, rtplanFileName.get)
      if (dir.isDirectory)
        Some(dir)
      else
        None
    } else {
      None
    }
    if (directory.isDefined)
      logText(tagName, "Directory: " + directory.get.getAbsolutePath)
    else
      logText(tagName, "Could not initialize Simple RTPLAN directory.  The generate simple RTPLAN feature will not be supported.")
    directory
  }

  private def getWatermark: Option[Watermark] = {
    val tag = "Watermark"
    try {
      document \ tag match {
        case wm if wm.isEmpty =>
          logText("Watermark", "No watermark tag found in configuration.")
          None
        case wm =>
          val imageName = (wm \ "@image").head.text
          val top = (wm \ "@top").head.text.toBoolean
          val left = (wm \ "@left").head.text.toBoolean
          val percentWidth = (wm \ "@percentWidth").head.text.toDouble
          val percentChange = (wm \ "@percentChange").head.text.toDouble
          val watermarkImageFile = new File(imageDirFile, imageName)
          val watermarkImage = ImageIO.read(watermarkImageFile)
          val watermark = new Watermark(watermarkImage, top, left, percentWidth, percentChange)
          logText(
            "Watermark",
            "image:  " + watermarkImageFile.getAbsolutePath +
              "    top: " + top +
              "    left: " + left + "    percentWidth: " + percentWidth +
              "    percentChange: " + percentChange
          )
          Some(watermark)
      }
    } catch {
      case t: Throwable =>
        logText("Watermark", "Unable to create watermark: " + t)
        None

    }
  }

  private object ToBeAnonymized {
    private def fmtTag(tag: AttributeTag) = {
      tag.getGroup.formatted("%04x") + "," + tag.getElement.formatted("%04x")
    }

    def tagFromName(name: String): AttributeTag = {
      try {
        DicomUtil.dictionary.getTagFromName(name)
      } catch {
        case _: java.lang.Throwable =>
          val msg = "Unable to find ToBeAnonymized name in Pixelmed library: " + name
          logger.error(msg)
          throw new RuntimeException(msg)
      }
    }
  }

  /**
    * A DICOM tag that should be anonymized.
    */
  case class ToBeAnonymized(Name: String, AttrTag: AttributeTag, Value: Option[String]) {
    override def toString: String = {
      val v = if (Value.isDefined) " : " + Value.get else ""
      ToBeAnonymized.fmtTag(AttrTag) + " : " + Name + v
    }
  }

  /**
    * Construct lookup table for finding DICOM attributes to be anonymized and how to anonymize them.
    */
  private def getToBeAnonymizedList: Map[AttributeTag, ToBeAnonymized] = {
    def makeToBeAnon(node: Node): ToBeAnonymized = {
      val Name: Option[String] = {
        val ns = node \ "@Name"
        if (ns.nonEmpty) Some(ns.head.text.trim) else None
      }

      val Tag: Option[AttributeTag] = {
        val ns = node \ "@Tag"
        if (ns.isEmpty) None
        else {
          try {
            val text = ns.head.text
            val intList = text.trim.toLowerCase.replace("x", "").split(",").map(t => Integer.parseInt(t, 16))
            Some(new AttributeTag(intList(0), intList(1)))
          } catch {
            case _: Throwable =>
              logger.warn("Ignoring unparsable ToBeAnonymized configuration tag (should be pair of hex values): " + ns)
              None
          }
        }
      }

      val Value: Option[String] = {
        val ns = node \ "@Value"
        if (ns.nonEmpty) Some(ns.head.text.trim) else None
      }

      (Name, Tag) match {
        case (Some(n), Some(t)) => new ToBeAnonymized(n, t, Value)
        case (Some(n), _)       => new ToBeAnonymized(n, ToBeAnonymized.tagFromName(n), Value)
        case (_, Some(t)) =>
          val nm = DicomUtil.dictionary.getNameFromTag(t) // try to get the name from the dictionary
          if (nm == null)
            new ToBeAnonymized(DicomUtil.formatAttrTag(t), t, Value)
          else
            new ToBeAnonymized(nm, t, Value)
        case _ => throw new RuntimeException("Ignoring unparsable ToBeAnonymized configuration value: " + node)
      }
    }

    val configTag = "ToBeAnonymizedList"
    logger.info("Getting " + configTag + "  ...")
    val list = (document \ configTag \ "ToBeAnonymized").toList.map(node => makeToBeAnon(node))
    logText(configTag, "\n            " + indentList(list))
    list.map(tba => (tba.AttrTag, tba)).toMap
  }

  /**
    * A standard plan used as a basis for a customized plan.
    */
  case class PlanFileConfig(procedure: String, manufacturer: String, collimatorModel: String, file: File) {
    val dicomFile = new DicomFile(file)
    val fileIsValid: Boolean = dicomFile.attributeList.isDefined

    override def toString: String = "manufacturer: " + manufacturer + "  collimator model: " + collimatorModel.format("%-12s") + "  file: " + file.getName
  }

  private def getPlanFileList: Seq[PlanFileConfig] = {
    def makePlanFileConfig(node: Node): PlanFileConfig = {
      val procedure = (node \ "@procedure").head.text
      val manufacturer = (node \ "@manufacturer").head.text
      val model = (node \ "@collimatorModel").head.text
      val fileName = node.head.text

      val file = new File(rtplanDirFile, fileName.trim)
      val pfc = PlanFileConfig(procedure, manufacturer, model, file)

      // check to see if the file is readable DICOM.  If not, flag an error.
      if (pfc.dicomFile.attributeList.isEmpty)
        logger.warn(
          "Config problem.  Unable to read DICOM RTPLAN file used for generating custom plans: " + pfc +
            "\nFile: " + pfc.dicomFile.file.getAbsolutePath
        )

      // Return the plan config even if its file does not exist.  The user interface for
      // creating a custom plan will check it and tell the user if it does not exist.
      // It is handled this way because:
      //
      //     The installation might not require this rtplan anyway, so there is no point in
      //         having the configuration fail for something the user does not need.
      //
      //     If the user needs and it is not defined, then this makes the problem clear to
      //         the user and allows them to do something about it.
      pfc
    }

    val configTag = "PlanFileList"
    val list = (document \ configTag \ "PlanFile").toList.map(node => makePlanFileConfig(node))
    logText(configTag, indentList(list))
    list
  }

  val SlickDb: com.typesafe.config.Config = getSlickDb

  val UserWhiteList: List[String] = (document \ "UserWhiteList" \ "User").toList.map(node => node.head.text.trim.toLowerCase)

  /** Symbols for pass and fail. */
  val passImageUrl = "/static/images/pass.png"
  val failImageUrl = "/static/images/fail.png"

  val RootUrl: String = logMainText("RootUrl", "https://automatedqualityassurance.org")

  val FloodFieldBeamName: String = logMainText("FloodFieldBeamName", "Flood 6X")

  val PrototypeCustomBeamName: String = logMainText("PrototypeCustomBeamName", "J18G0-6X")
  val PrefixForMachineDependentBeamName: String = logMainText("PrefixForMachineDependentBeamName", "J18G0-")

  val CollimatorCenteringTolerence_mm: Double = logMainText("CollimatorCenteringTolerence_mm", "2.0").toDouble
  private val CollimatorCentering090BeamName: String = logMainText("CollimatorCentering090BeamName", "J10G0C90-6X")
  private val CollimatorCentering270BeamName: String = logMainText("CollimatorCentering270BeamName", "J10G0C270-6X")

  private val CollimatorCenteringBeamNameG000C090: String = logMainText(name = "CollimatorCenteringBeamNameG000C090", default = "M10G0C90")
  private val CollimatorCenteringBeamNameG000C270: String = logMainText(name = "CollimatorCenteringBeamNameG000C270", default = "M10G0C270")
  private val CollimatorCenteringBeamNameG180C090: String = logMainText(name = "CollimatorCenteringBeamNameG180C090", default = "M10G180C90")
  private val CollimatorCenteringBeamNameG080C270: String = logMainText(name = "CollimatorCenteringBeamNameG080C270", default = "M10G180C270")
  private val CollimatorCenteringBeamNameG270C090: String = logMainText(name = "CollimatorCenteringBeamNameG270C090", default = "M10G270C90")
  private val CollimatorCenteringBeamNameG270C270: String = logMainText(name = "CollimatorCenteringBeamNameG270C270", default = "M10G270C270")
  private val CollimatorCenteringBeamNameG090C090: String = logMainText(name = "CollimatorCenteringBeamNameG090C090", default = "M10G90C90")
  private val CollimatorCenteringBeamNameG090C270: String = logMainText(name = "CollimatorCenteringBeamNameG090C270", default = "M10G90C270")

  val collimatorCenteringPhase2List: Seq[String] = Seq(CollimatorCentering090BeamName, CollimatorCentering270BeamName)

  val collimatorCenteringPhase3List: Seq[String] = Seq(
    CollimatorCenteringBeamNameG000C090,
    CollimatorCenteringBeamNameG000C270,
    CollimatorCenteringBeamNameG180C090,
    CollimatorCenteringBeamNameG080C270,
    CollimatorCenteringBeamNameG270C090,
    CollimatorCenteringBeamNameG270C270,
    CollimatorCenteringBeamNameG090C090,
    CollimatorCenteringBeamNameG090C270
  )
  val CollimatorCenteringCoarseBandWidth_mm: Double = logMainText("CollimatorCenteringCoarseBandWidth_mm", "5.0").toDouble
  val PenumbraThickness_mm: Double = logMainText("PenumbraThickness_mm", "20.0").toDouble
  val PenumbraPlateauPixelsPerMillion: Int = logMainText("PenumbraPlateauPixelsPerMillion", "500").toInt
  val PenumbraThresholdPercent: Double = logMainText("PenumbraThresholdPercent", "50.0").toDouble

  val MaxEstimatedBadPixelPerMillion: Int = logMainText("MaxEstimatedBadPixelPerMillion", "20").toInt
  val BadPixelSamplePerMillion: Int = logMainText("BadPixelSamplePerMillion", "250").toInt
  val BadPixelStdDev: Double = logMainText("BadPixelStdDev", "3.0").toDouble
  val BadPixelRadius_mm: Double = logMainText("BadPixelRadius_mm", "3.5").toDouble
  val BadPixelMinimumDeviation_CU: Double = logMainText("BadPixelMinimumDeviation_CU", "10.0").toDouble
  val BadPixelMaximumPercentChange: Double = logMainText("BadPixelMaximumPercentChange", "10.0").toDouble
  val MaxAllowedBadPixelsPerMillion: Int = logMainText("MaxAllowedBadPixelsPerMillion", "50").toInt
  val DeepColorPercentDrop: Double = logMainText("DeepColorPercentDrop", "0.2").toDouble

  val MaxProcedureDuration: Double = logMainText("MaxProcedureDuration", "120.0").toDouble

  /** Lookup table for finding DICOM attributes to be anonymized and how to anonymize them. */
  val ToBeAnonymizedList: Map[AttributeTag, ToBeAnonymized] = getToBeAnonymizedList

  val PlanFileList: Seq[PlanFileConfig] = getPlanFileList

  val TermsOfUse: String = logMainText("TermsOfUse", "not specified")

  private val watermark = getWatermark

  /** If a watermark has been configured, then apply it to the given image> */
  def applyWatermark(image: BufferedImage): Unit = if (watermark.isDefined) watermark.get.mark(image)

  private def requireReadableDirectory(name: String, dir: File): Unit = {
    if (!dir.canRead) fail("Directory " + name + " is not readable: " + dir)
    if (!dir.isDirectory) fail("Directory " + name + " is required but is not a directory: " + dir)
  }

  // force each of these directories to be usable.  It would be catastrophic if this fails.
  requireReadableDirectory("resultsDirFile", resultsDirFile)
  requireReadableDirectory("tmpDirFile", tmpDirFile)
  requireReadableDirectory("machineConfigurationDirFile", machineConfigurationDirFile)

  val CenterDoseRadius_mm: Double = logMainText("CenterDoseRadius_mm", "5.0").toDouble
  val CenterDoseHistoryRange: Int = logMainText("CenterDoseHistoryRange", "1000000").toInt

  val CollimatorPositionTolerance_mm: Double = logMainText("CollimatorPositionTolerance_mm", "2.0").toDouble
  val CollimatorPositionBeamList: Seq[CollimatorPositionBeamConfig] = getCollimatorPositionBeamList

  val MaintenanceCategoryList: Seq[MaintenanceCategory] = getMaintenanceCategoryList

  val WedgeProfileThickness_mm: Double = logMainText("WedgeProfileThickness_mm", "5.0").toDouble
  val WedgeBeamList: Seq[WedgeBeam] = getWedgeBeamList
  val WedgeTolerance_pct: Double = logMainText("WedgeTolerance_pct", "2.0").toDouble
  val WedgeHistoryRange: Int = logMainText("WedgeHistoryRange", "1000000").toInt

  val SymmetryAndFlatnessDiameter_mm: Double = logMainText("SymmetryAndFlatnessDiameter_mm", "5.0").toDouble

  val SymmetryPercentLimit: Double = logMainText("SymmetryPercentLimit", "2.0").toDouble
  val FlatnessPercentLimit: Double = logMainText("FlatnessPercentLimit", "2.0").toDouble
  val SymFlatConstHistoryRange: Int = logMainText("SymFlatConstHistoryRange", "1000000").toInt
  val ProfileConstancyPercentLimit: Double = logMainText("ProfileConstancyPercentLimit", "2.0").toDouble

  val SymmetryAndFlatnessPointList: Seq[SymmetryAndFlatnessPoint] = getSymmetryAndFlatnessPointList

  val SymmetryAndFlatnessPointPairList: Seq[(SymmetryAndFlatnessPoint, SymmetryAndFlatnessPoint)] = getSymmetryAndFlatnessPointPairList

  val SymmetryPointTop: SymmetryAndFlatnessPoint = getSymPoint("SymmetryPointTop")
  val SymmetryPointBottom: SymmetryAndFlatnessPoint = getSymPoint("SymmetryPointBottom")
  val SymmetryPointLeft: SymmetryAndFlatnessPoint = getSymPoint("SymmetryPointLeft")
  val SymmetryPointRight: SymmetryAndFlatnessPoint = getSymPoint("SymmetryPointRight")
  val SymmetryPointCenter: SymmetryAndFlatnessPoint = getSymPoint("SymmetryPointCenter")

  val LeafPositionMaxError_mm: Double = logMainText("LeafPositionMaxError_mm", "1.0").toDouble
  val LeafPositionIsolationDistance_mm: Double = logMainText("LeafPositionIsolationDistance_mm", "0.5").toDouble
  val LeafPositionBeamNameList: Seq[String] = getBeamNameList("LeafPositionBeamNameList")

  val VMATDeviationThreshold_pct: Double = logMainText("VMATDeviationThreshold_pct", "3.0").toDouble
  val VMATAverageOfAbsoluteDeviationThreshold_pct: Double = logMainText("VMATAverageOfAbsoluteDeviationThreshold_pct", "1.5").toDouble
  val VMATBeamPairList: Seq[VMATBeamPair] = getVMATBeamPairList
  val VMATHistoryRange: Int = logMainText("VMATHistoryRange", "1000000").toInt

  private val TrueBeamSourceToXJawDistance_cm: Double = logMainText("TrueBeamSourceToXJawDistance_cm", "40.6").toDouble
  private val TrueBeamSourceToYJawDistance_cm: Double = logMainText("TrueBeamSourceToYJawDistance_cm", "31.9").toDouble
  private val TrueBeamSourceToMLCDistance_cm: Double = logMainText("TrueBeamSourceToMLCDistance_cm", "49.0").toDouble

  val TrueBeamSourceToXJawDistance_mm: Double = TrueBeamSourceToXJawDistance_cm * 10
  val TrueBeamSourceToYJawDistance_mm: Double = TrueBeamSourceToYJawDistance_cm * 10
  val TrueBeamSourceToMLCDistance_mm: Double = TrueBeamSourceToMLCDistance_cm * 10

  val FocalSpotBeamNameList: Seq[String] = getBeamNameList("FocalSpotBeamNameList")

  private val DailyQAInProgressInterval_min: Double = logMainText("DailyQAInProgressInterval_min", "30.0").toDouble
  val DailyQAInProgressInterval_ms: Long = (DailyQAInProgressInterval_min * 60 * 1000).round
  val DailyQACBCTLimit_mm: Double = logMainText("DailyQACBCTLimit_mm", "1.0").toDouble
  val DailyQAPassLimit_mm: Double = logMainText("DailyQAPassLimit_mm", "1.0").toDouble
  val DailyQAWarningLimit_mm: Double = logMainText("DailyQAWarningLimit_mm", "1.5").toDouble
  val CBCTBBMinimumStandardDeviation: Double = logMainText("CBCTBBMinimumStandardDeviation", "1.0").toDouble
  val DailyQACBCTDarkPixelValueLevels: Int = logMainText("DailyQACBCTDarkPixelValueLevels", "50").toInt

  val DailyPhantomSearchDistance_mm: Double = logMainText("DailyPhantomSearchDistance_mm", "50.0").toDouble
  val DailyQAPhantomCubeSize_mm: Double = logMainText("DailyQAPhantomCubeSize_mm", "50.0").toDouble
  val DailyQACBCTVoxPercentTolerance: Double = logMainText("DailyQACBCTVoxPercentTolerance", "15.0").toDouble
  val DailyQACBCTCubeSizePercentTolerance: Double = logMainText("DailyQACBCTCubeSizePercentTolerance", "15.0").toDouble
  val DailyQACBCTCubeCrossSectionalAreaPercentTolerance: Double = logMainText("DailyQACBCTCubeCrossSectionalAreaPercentTolerance", "25.0").toDouble
  val DailyQAMaxChartHistory: Int = logMainText("DailyQAMaxChartHistory", "300").toInt
  val CBCTBBPenumbra_mm: Double = logMainText("CBCTBBPenumbra_mm", "2.5").toDouble
  val CBCTZoomSize_mm: Double = logMainText("CBCTZoomSize_mm", "40.0").toDouble
  val CBCTImageColor: Color = Util.hexToColor(logMainText("CBCTImageColor", "FFFFFF"))
  val BBbyCBCTHistoryRange: Int = logMainText("BBbyCBCTHistoryRange", "1000000").toInt

  val BBbyEPIDSearchDistance_mm: Double = logMainText("BBbyEPIDSearchDistance_mm", "10.0").toDouble
  val EPIDBBPenumbra_mm: Double = logMainText("EPIDBBPenumbra_mm", "2.0").toDouble
  val EPIDBBMinimumStandardDeviation: Double = logMainText("EPIDBBMinimumStandardDeviation", "2.25").toDouble
  val EPIDImageColor: Color = Util.hexToColor(logMainText("EPIDImageColor", "FFFFFF"))
  val EPIDZoomSize_mm: Double = logMainText("EPIDZoomSize_mm", "90.0").toDouble
  val BBbyEPIDHistoryRange: Int = logMainText("BBbyEPIDHistoryRange", "1000000").toInt
  val BBbyCBCTChartTolerance_mm: Double = logMainText("BBbyCBCTChartTolerance_mm", "1.0").toDouble.abs
  val BBbyCBCTChartYRange_mm: Double = logMainText("BBbyCBCTChartYRange_mm", "3.0").toDouble.abs
  val BBbyEPIDChartTolerance_mm: Double = logMainText("BBbyEPIDChartTolerance_mm", "1.0").toDouble.abs
  val BBbyEPIDChartYRange_mm: Double = logMainText("BBbyEPIDChartYRange_mm", "3.0").toDouble.abs
  val EPIDBBMaxBackgroundCoefficientOfVariation: Double = logMainText("EPIDBBMaxBackgroundCoefficientOfVariation", "0.03").toDouble.abs
  val BBbyCBCTMaximumSliceThickness_mm: Double = logMainText("BBbyCBCTMaximumSliceThickness_mm", "1.0").toDouble.abs

  val GapSkewBeamNameList: List[String] = getBeamNameList("GapSkewBeamNameList")
  val GapSkewLeafEndPenumbra_mm: Double = logMainText("GapSkewLeafEndPenumbra_mm", "20.0").toDouble.abs
  val GapSkewAngleWarn_deg: Double = logMainText("GapSkewAngleWarn_deg", "0.1").toDouble.abs
  val GapSkewAngleFail_deg: Double = logMainText("GapSkewAngleFail_deg", "0.2").toDouble.abs
  val GapSkewExpiration_day: Double = logMainText("GapSkewExpiration_day", "45").toDouble
  val GapSkewExpiration_ms: Double = daysToMs(GapSkewExpiration_day)
  val GapSkewPenumbraThickness_mm: Double = logMainText("GapSkewPenumbraThickness_mm", "8.0").toDouble
  val GapSkewMinimumFieldWidth_mm: Double = logMainText("GapSkewMinimumFieldWidth_mm", "170.0").toDouble
  val GapSkewMinimumMeasurementLength_mm: Double = logMainText("GapSkewMinimumMeasurementLength_mm", "20.0").toDouble

  val SimpleRtplanTemplateDir: Option[File] = getSimpleRtplanTemplateDir

  val ResultsMaxEntries: Int = logMainText("ResultsMaxEntries", "1000").toInt

  // =================================================================================

  // Winston Lutz values
  def toColor(s: String) = new Color(Integer.parseInt(s, 16))

  val WLBoxSize: Int = logMainText("WLBoxSize", "29").toInt
  val WLAveragePixelsForBrightness: Int = logMainText("WLAveragePixelsForBrightness", "10").toInt
  val WLImageColor: Color = toColor(logMainText("WLImageColor", default = "000001"))
  val WLMinimumDistinctPixelValues: Int = logMainText("WLMinimumDistinctPixelValues", "40").toInt
  val WLMarginalPixelGapLimit: Int = logMainText("WLMarginalPixelGapLimit", "500").toInt
  val WLBadPixelGapLimit: Int = logMainText("WLBadPixelGapLimit", "1000").toInt

  val WLPassLimit: Double = logMainText("WLPassLimit", "1.5").toDouble
  val WLBoxSizePercent: Double = logMainText("WLBoxSizePercent", "30.0").toDouble
  val WLBoxPositionFactor: Double = logMainText("WLBoxPositionFactor", "2.0").toDouble
  val WLLaserCorrectionLimit: Double = logMainText("WLLaserCorrectionLimit", "0.5").toDouble
  val WLRowsPerPageDefault: Int = logMainText("WLRowsPerPageDefault", "50").toInt
  val WLMaxChartHistory: Int = logMainText("WLMaxChartHistory", "200").toInt

  val WLMaxAllowedBrightnessRangePercentDifference: Double = logMainText("WLMaxAllowedBrightnessRangePercentDifference", "20.0").toDouble
  val WLBoxEdgeTolerance_mm: Double = logMainText("WLBoxEdgeTolerance_mm", "5.5").toDouble
  val WLBadPixelCorrectionRadius: Int = logMainText("WLBadPixelCorrectionRadius", "2").toInt
  val WLScale: Int = logMainText("WLScale", "16").toInt
  val WLNumBackgroundPixels: Int = logMainText("WLNumBackgroundPixels", "5").toInt
  val WLBallDiameter: Double = logMainText("WLBallDiameter", "5.0").toDouble
  val WLFieldWidth: Double = logMainText("WLFieldWidth", "20.0").toDouble
  val WLBoxEdgeTolerance: Double = logMainText("WLBoxEdgeTolerance", "5.5").toDouble
  val WLBallAreaFlatnessRatioLowerLimit: Double = logMainText("WLBallAreaFlatnessRatioLowerLimit", "5.0").toDouble
  val WLBallHeightPercentForeground: Double = logMainText("WLBallHeightPercentForeground", "50.0").toDouble
  val WLWLImageColor: Color = toColor(logMainText("WLWLImageColor", "000001"))
  val WLBoxColor: Color = toColor(logMainText("WLBoxColor", "00ff00"))
  val WLBoxColorCorrected: Color = toColor(logMainText("WLBoxColorCorrected", "d0d0d0"))
  val WLBallColor: Color = toColor(logMainText("WLBallColor", "ffff00"))
  val WLOffsetColor: Color = toColor(logMainText("WLOffsetColor", "ff0000"))
  val WLSplineColor: Color = toColor(logMainText("WLSplineColor", "ffff00"))
  val WLTextColor: Color = toColor(logMainText("WLTextColor", "000000"))
  val WLPassColor: Color = toColor(logMainText("WLPassColor", "1dc32b"))
  val WLFailColor: Color = toColor(logMainText("WLFailColor", "e00034"))
  val WLTextFont: String = logMainText("WLTextFont", "SansSerif")
  val WLTextPointSize: Int = logMainText("WLTextPointSize", "30").toInt
  val WLNumberOfCircles: Int = logMainText("WLNumberOfCircles", "2").toInt
  val WLSummarySize: Int = logMainText("WLSummarySize", "350").toInt

  val WLTreatmentMachineList: List[WLTreatmentMachine] = {
    val list = (document \ "WLTreatmentMachineList" \ "WLTreatmentMachine").toList.map(e => new WLTreatmentMachine(e))
    list
  }

  // =================================================================================

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate: Boolean = validated

  override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

  def toHtml: Elem = {
    <pre>
      {valueText}
    </pre>
  }

  logger.info("Configuration has been validated.")
  logger.info(toString)

  /**
    * Self test.
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis
    println("validate: " + validate) // loading forces configuration to be read
    val elapsed = System.currentTimeMillis - startTime
    println("Elapsed time in ms: " + elapsed)
    System.exit(99)
  }
}
