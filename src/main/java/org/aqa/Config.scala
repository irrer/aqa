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
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessPoint
import java.awt.geom.Point2D
import org.aqa.db.MaintenanceCategory
import edu.umro.ImageUtil.Watermark
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.ValueRepresentation
import com.typesafe.config.ConfigFactory

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

  /** For indenting sub-content. */
  private val indent1 = "\n                  "

  /** For indenting sub-sub-content. */
  private val indent2 = indent1 + "    "

  private def indentList[T](list: Seq[T]): String = list.mkString(indent1, indent1, "\n")

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
    val tried = indentList(directoryList.map(d => d.getAbsolutePath))

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
      case _: Throwable => {
        logText(name, "[not configured]")
        List[String]()
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
    logger.info("Getting attribute: " + name)
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
    val value = getMainText(name)
    logText(name, value)
    value
  }

  /**
   * Get the value matching the given name.  If it does not exist or there is
   * some sort of other problem then return the default.
   */
  private def logMainText(name: String, default: String): String = {
    getMainTextOption(name) match {
      case Some(value: String) => {
        logText(name, value)
        value
      }
      case _ => {
        logText(name, default)
        default
      }
    }
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

  val HTTPPort = logMainText("HTTPPort", "80").toInt

  /**
   * Get the list of allowed IP addresses.  If empty, allow everything.
   */
  private def getAllowedHttpIpList: List[String] = {
    val nodeList = document \ "AllowedHttpIpList" \ "AllowedHttpIp"
    val allowedList = nodeList.toList.map(n => n.head.text.trim)
    logText("AllowedHttpIpList", allowedList.mkString("\n    ", "\n    ", "\n    "))
    allowedList
  }

  private def getLdapUrl: Option[String] = {
    val tag = "LdapUrl"
    try {
      val url = (document \ tag).head.text.toString.trim
      logText(tag, url)
      Some(url)
    } catch {
      case t: Throwable => None
    }
  }

  private def getLdapGroupList: Seq[String] = {
    val groupTag = "LdapGroupList"
    val tag = "LdapGroup"
    try {
      val groupList = (document \ groupTag \ tag).map(n => n.text.toString.trim)
      logText(groupTag, groupList.mkString("\n        ", "\n        ", ""))
      groupList
    } catch {
      case t: Throwable => Seq[String]()
    }
  }

  /**
   * List of IP addresses allowed to access this server.
   */
  val AllowedHttpIpList = getAllowedHttpIpList

  val JavaKeyStorePasswordList = getJavaKeyStorePasswordList
  val JavaKeyStoreFileList = getJavaKeyStoreFileList

  val LdapUrl = getLdapUrl
  val LdapInstitutionName = logMainText("LdapInstitutionName", "not specified")
  val LdapRole = logMainText("LdapRole", "not specified")
  val LdapGroupList = getLdapGroupList

  val ProgramDir = getDir("ProgramDir")
  val ProcedureDir = getDir("ProcedureDir")
  val DataDir = getDir("DataDir")
  val machineConfigurationDir = new File(DataDir, machineConfigurationDirName)
  machineConfigurationDir.mkdirs

  val AuthenticationTimeout = logMainText("AuthenticationTimeout").toDouble
  val AuthenticationTimeoutInMs = (AuthenticationTimeout * 1000).toLong

  val PasswordPrompt = logMainText("PasswordPrompt", "Please enter your password")

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

  private def getSlickDb = {
    val name = "SlickDb"
    val configText = getMainText(name)
    logger.info("Constructing database config")
    val dbConfig = ConfigFactory.parseString(configText)
    logger.info("Constructed database config")
    val content = dbConfig.entrySet.toArray.map(cs => cs.toString).filterNot(cs => cs.toLowerCase.contains("pass")).mkString("\n                  ", "\n                  ", "")
    logger.info("Database configuration (password redacted): " + content)

    logText(name, "Constructed database config:" + content)
    dbConfig
  }

  private def getCenterDoseBeamNameList = {
    val list = (document \ "CenterDoseBeamNameList" \ "BeamName").map(n => n.head.text.toString.trim).toList
    logText("CenterDoseBeamNameList", indentList(list))
    list.distinct
  }

  private def getSymmetryAndFlatnessBeamList = {
    val list = (document \ "SymmetryAndFlatnessBeamList" \ "BeamName").map(n => n.head.text.toString.trim)
    logText("SymmetryAndFlatnessBeamList", indentList(list))
    list.distinct
  }

  private def getLeafPositionBeamNameList = {
    val list = (document \ "LeafPositionBeamNameList" \ "BeamName").map(n => n.head.text.toString.trim).toList
    logText("LeafPositionBeamNameList", indentList(list))
    list.distinct
  }

  private def getSymmetryAndFlatnessPointList: Seq[SymmetryAndFlatnessPoint] = {
    def makePoint(node: Node): SymmetryAndFlatnessPoint = {
      val name = (node \ "@name").head.text.toString
      val x_mm = (node \ "@x_mm").head.text.toString.toDouble
      val y_mm = (node \ "@y_mm").head.text.toString.toDouble
      new SymmetryAndFlatnessPoint(name, x_mm, y_mm)
    }

    val list = (document \ "SymmetryAndFlatnessPointList" \ "SymmetryAndFlatnessPoint").map(n => makePoint(n)).toList.toSeq
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

    val list = for (a <- SymmetryAndFlatnessPointList; b <- SymmetryAndFlatnessPointList; if isPair(a, b)) yield { (a, b) }
    val listText = indentList(list.map(pair => pair._1.name + "   " + pair._2.name))
    logText("SymmetryAndFlatnessPointList", listText)
    list
  }

  private def getSymPoint(nodeName: String) = {
    val name = (document \ nodeName \ "@name").head.text.toString
    val point = SymmetryAndFlatnessPointList.find(p => p.name.equals(name)).get
    logText(nodeName, point.toString)
    point
  }

  case class CollimatorPositionBeamConfig(beamName: String, FloodCompensation: Boolean) {
    override def toString = "Beam name: " + beamName + "  FloodCompensation: " + FloodCompensation
  }

  private def getCollimatorPositionBeamList = {
    def nodeToCollimatorPositionBeam(node: Node) = {
      val beamName = node.head.text.toString.trim
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
    override def toString = "Wedge: " + wedge + "    background: " + backgroundList.mkString("    ")
  }

  private def getWedgeBeamList = {

    def nodeToWedgeBeamSet(node: Node) = {
      val beamName = (node \ "@BeamName").head.text.toString.trim
      val background = {
        try {
          (node \ "BackgroundBeamName").map(n => n.head.text.toString)
        } catch {
          case t: Throwable => {
            Seq("not found")
          }
        }
      }
      new WedgeBeam(beamName, background)
    }
    val list = (document \ "WedgeBeamList" \ "WedgeBeam").map(node => nodeToWedgeBeamSet(node)).toList
    logText("WedgeBeamList", indentList(list))
    list
  }

  /**
   * Encapsulate the configuration for pair of VMAT beams.
   */
  case class VMATBeamPair(name: String, MLC: String, OPEN: String, IsolationBorder_mm: Double) {
    override def toString = name + "    MLC: " + MLC.formatted("%-14s") + "    OPEN: " + OPEN.formatted("%-14s") + "    IsolationBorder_mm: " + IsolationBorder_mm.formatted("%6.3f")
  }

  /**
   * Convert VMAT beam pair list from XML to list of <code>VMATBeamPair</code>s.
   */
  private def getVMATBeamPairList: Seq[VMATBeamPair] = {
    def nodeToVMATBeamPair(node: Node) = {
      new VMATBeamPair(
        (node \ "@Name").head.text,
        (node \ "@MLC").head.text,
        (node \ "@OPEN").head.text,
        (node \ "@IsolationBorder_mm").head.text.toDouble)
    }
    val list = (document \ "VMATBeamPairList" \ "VMATBeamPair").map(node => nodeToVMATBeamPair(node)).toList
    val asText = list.mkString("\n        ", "\n        ", "")
    logText("VMATBeamPairList", asText)
    list
  }

  private def getMaintenanceCategoryList: List[MaintenanceCategory] = {
    def nodeToMaintCat(node: Node) = {
      new MaintenanceCategory(
        (node \ "@Name").head.text.toString,
        (node \ "@Color").head.text.toString,
        node.head.text.toString)
    }

    val list = (document \ "MaintenanceCategoryList" \ "MaintenanceCategory").map(node => nodeToMaintCat(node)).toList
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

  private def getWatermark: Option[Watermark] = {
    val tag = "Watermark"
    try {
      (document \ tag) match {
        case wm if (wm.isEmpty) => {
          logText("Watermark", "No watermark tag found in configuration.")
          None
        }
        case wm => {
          val imageName = (wm \ "@image").head.text.toString
          val top = (wm \ "@top").head.text.toString.toBoolean
          val left = (wm \ "@left").head.text.toString.toBoolean
          val percentWidth = (wm \ "@percentWidth").head.text.toString.toDouble
          val percentChange = (wm \ "@percentChange").head.text.toString.toDouble
          val watermarkImageFile = new File(imageDirFile, imageName)
          val watermarkImage = ImageIO.read(watermarkImageFile)
          val watermark = new Watermark(watermarkImage, top, left, percentWidth, percentChange)
          logText(
            "Watermark",
            "image:  " + watermarkImageFile.getAbsolutePath +
              "    top: " + top +
              "    left: " + left + "    percentWidth: " + percentWidth +
              "    percentChange: " + percentChange)
          Some(watermark)
        }
      }
    } catch {
      case t: Throwable => {
        logText("Watermark", "Unable to create watermark: " + t)
        None
      }

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
        case t: java.lang.Throwable => {
          val msg = "Unable to find ToBeAnonymized name in Pixelmed library: " + name
          logger.error(msg)
          throw new RuntimeException(msg)
        }
      }
    }
  }

  /**
   * A DICOM tag that should be anonymized.
   */
  case class ToBeAnonymized(Name: String, AttrTag: AttributeTag, Value: Option[String]) {
    override def toString = {
      val v = if (Value.isDefined) " : " + Value.get else ""
      ToBeAnonymized.fmtTag(AttrTag) + " : " + Name + v
    }
  }

  /**
   * Construct lookup table for finding DICOM attributes to be anonymized and how to anonymize them.
   */
  def getToBeAnonymizedList: Map[AttributeTag, ToBeAnonymized] = {
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
            case t: Throwable =>
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
        case (Some(n), _) => new ToBeAnonymized(n, ToBeAnonymized.tagFromName(n), Value)
        case (_, Some(t)) => {
          val nm = DicomUtil.dictionary.getNameFromTag(t) // try to get the name from the dictionary
          if (nm == null)
            new ToBeAnonymized(DicomUtil.formatAttrTag(t), t, Value)
          else
            new ToBeAnonymized(nm, t, Value)
        }
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
   * A standard Phase 2 plan used as a basis for a customized plan.
   */
  case class Phase2PlanFileConfig(manufacturer: String, model: String, file: File) {
    val dicomFile = new DicomFile(file)
    override def toString = "manufacturer: " + manufacturer + "  model: " + model.formatted("%-12s") + "  file: " + file.getName
  }

  private def getPhase2PlanFileList = {
    def makePhase2PlanFileConfig(node: Node): Phase2PlanFileConfig = {
      val manufacturer = (node \ "@manufacturer").head.text
      val model = (node \ "@model").head.text
      val fileName = node.head.text

      val file = new File(staticDirFile, fileName)
      new Phase2PlanFileConfig(manufacturer, model, file)
    }

    val configTag = "Phase2PlanFileList"
    val list = (document \ configTag \ "Phase2PlanFile").toList.map(node => makePhase2PlanFileConfig(node))
    logText(configTag, indentList(list))
    list
  }

  /**
   * When customizing a plan, override this DICOM attribute with the given value.
   */
  case class PlanAttributeOverride(tag: AttributeTag, value: String) {
    override def toString = DicomUtil.dictionary.getNameFromTag(tag) + " : " + value
  }

  private def getPhase2PlanAttributeOverrideList: Seq[PlanAttributeOverride] = {
    def makeOverride(node: Node): PlanAttributeOverride = {
      val name = (node \ "@Name").head.text
      val value = (node \ "@Value").head.text

      new PlanAttributeOverride(DicomUtil.dictionary.getTagFromName(name), value)
    }
    val configTag = "Phase2PlanAttributeOverrideList"
    val list = (document \ configTag \ "Phase2PlanAttributeOverride").toList.map(node => makeOverride(node))
    logText(configTag, indentList(list))
    list
  }

  val SlickDb = getSlickDb

  val UserWhiteList: List[String] = (document \ "UserWhiteList" \ "User").toList.map(node => node.head.text.trim.toLowerCase)

  /** Symbols for pass and fail. */
  val passImageUrl = "/static/images/pass.png"
  val failImageUrl = "/static/images/fail.png"

  val FloodFieldBeamName = logMainText("FloodFieldBeamName")

  val PrototypeCustomBeamName = logMainText("PrototypeCustomBeamName")
  val PrefixForMachineDependentBeamName = logMainText("PrefixForMachineDependentBeamName")

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
  val BadPixelRadius_mm = logMainText("BadPixelRadius_mm").toDouble
  val BadPixelMinimumDeviation_CU = logMainText("BadPixelMinimumDeviation_CU").toDouble
  val BadPixelMaximumPercentChange = logMainText("BadPixelMaximumPercentChange").toDouble
  val MaxAllowedBadPixelsPerMillion = logMainText("MaxAllowedBadPixelsPerMillion").toInt
  val DeepColorPercentDrop = logMainText("DeepColorPercentDrop").toDouble

  val MaxProcedureDuration = logMainText("MaxProcedureDuration").toDouble

  /** Lookup table for finding DICOM attributes to be anonymized and how to anonymize them.  */

  val ToBeAnonymizedList = getToBeAnonymizedList

  val Phase2PlanFileList = getPhase2PlanFileList
  val Phase2PlanAttributeOverrideList = getPhase2PlanAttributeOverrideList

  val TermsOfUse = logMainText("TermsOfUse")

  private val watermark = getWatermark
  /** If a watermark has been configured, then apply it to the given image> */
  def applyWatermark(image: BufferedImage) = if (watermark.isDefined) watermark.get.mark(image)

  private def requireReadableDirectory(name: String, dir: File) = {
    if (!dir.canRead) fail("Directory " + name + " is not readable: " + dir)
    if (!dir.isDirectory) fail("Directory " + name + " is required but is not a directory: " + dir)
  }

  // force each of these directories to be usable.  It would be catastrophic if this fails.
  requireReadableDirectory("resultsDirFile", resultsDirFile)
  requireReadableDirectory("tmpDirFile", tmpDirFile)
  requireReadableDirectory("machineConfigurationDirFile", machineConfigurationDirFile)

  val CenterDoseRadius_mm = logMainText("CenterDoseRadius_mm").toDouble
  val CenterDoseHistoryRange = logMainText("CenterDoseHistoryRange", "25").toInt
  val CenterDoseBeamNameList = getCenterDoseBeamNameList

  val CollimatorPositionTolerance_mm = logMainText("CollimatorPositionTolerance_mm").toDouble
  val CollimatorPositionBeamList = getCollimatorPositionBeamList

  val MaintenanceCategoryList = getMaintenanceCategoryList

  val WedgeProfileThickness_mm = logMainText("WedgeProfileThickness_mm").toDouble
  val WedgeBeamList = getWedgeBeamList
  val WedgeTolerance_pct = logMainText("WedgeTolerance_pct").toDouble
  val WedgeHistoryRange = logMainText("WedgeHistoryRange", "25").toInt

  val SymmetryAndFlatnessDiameter_mm = logMainText("SymmetryAndFlatnessDiameter_mm").toDouble

  val SymmetryPercentLimit = logMainText("SymmetryPercentLimit").toDouble
  val FlatnessPercentLimit = logMainText("FlatnessPercentLimit").toDouble
  val SymFlatConstHistoryRange = logMainText("SymFlatConstHistoryRange", "25").toInt
  val ProfileConstancyPercentLimit = logMainText("ProfileConstancyPercentLimit").toDouble

  val SymmetryAndFlatnessBeamList = getSymmetryAndFlatnessBeamList

  val SymmetryAndFlatnessPointList = getSymmetryAndFlatnessPointList

  val SymmetryAndFlatnessPointPairList: Seq[(SymmetryAndFlatnessPoint, SymmetryAndFlatnessPoint)] = getSymmetryAndFlatnessPointPairList

  val SymmetryPointTop = getSymPoint("SymmetryPointTop")
  val SymmetryPointBottom = getSymPoint("SymmetryPointBottom")
  val SymmetryPointLeft = getSymPoint("SymmetryPointLeft")
  val SymmetryPointRight = getSymPoint("SymmetryPointRight")
  val SymmetryPointCenter = getSymPoint("SymmetryPointCenter")

  val LeafPositionMaxError_mm = logMainText("LeafPositionMaxError_mm").toDouble
  val LeafPositionIsolationDistance_mm = logMainText("LeafPositionIsolationDistance_mm").toDouble
  val LeafPositionBeamNameList = getLeafPositionBeamNameList

  val VMATDeviationThreshold_pct = logMainText("VMATDeviationThreshold_pct", "3.0").toDouble
  val VMATAverageOfAbsoluteDeviationThreshold_pct = logMainText("VMATAverageOfAbsoluteDeviationThreshold_pct", "1.5").toDouble
  val VMATBeamPairList = getVMATBeamPairList
  val VMATHistoryRange = logMainText("VMATHistoryRange", "25").toInt

  val DailyQAPassLimit_mm = logMainText("DailyQAPassLimit_mm", "1.0").toDouble
  val DailyQAWarningLimit_mm = logMainText("DailyQAWarningLimit_mm", "1.5").toDouble
  val CBCTBBMinimumStandardDeviation = logMainText("CBCTBBMinimumStandardDeviation", "15.0").toDouble
  val DailyPhantomSearchDistance_mm = logMainText("DailyPhantomSearchDistance_mm", "50.0").toDouble
  val DailyQAPhantomCubeSize_mm = logMainText("DailyQAPhantomCubeSize_mm", "50.0").toDouble
  val DailyQACBCTVoxPercentTolerance = logMainText("DailyQACBCTVoxPercentTolerance", "15.0").toDouble
  val CBCTBBPenumbra_mm = logMainText("CBCTBBPenumbra_mm", "2.5").toDouble
  val CBCTZoomSize_mm = logMainText("CBCTZoomSize_mm", "30.0").toDouble
  val CBCTImageColor = Util.hexToColor(logMainText("CBCTImageColor", "FFFFFF"))
  val BBbyCBCTHistoryRange = logMainText("BBbyCBCTHistoryRange", "25").toInt

  val BBbyEPIDSearchDistance_mm = logMainText("BBbyEPIDSearchDistance_mm", "10.0").toDouble
  val EPIDBBPenumbra_mm = logMainText("EPIDBBPenumbra_mm", "2.0").toDouble
  val EPIDBBMinimumStandardDeviation = logMainText("EPIDBBMinimumStandardDeviation", "1.25").toDouble
  val EPIDImageColor = Util.hexToColor(logMainText("EPIDImageColor", "FFFFFF"))
  val EPIDZoomSize_mm = logMainText("EPIDZoomSize_mm", "90.0").toDouble
  val BBbyEPIDHistoryRange = logMainText("BBbyEPIDHistoryRange", "25").toInt
  val BBbyCBCTChartTolerance_mm = logMainText("BBbyCBCTChartTolerance_mm", "1.0").toDouble.abs
  val BBbyCBCTChartYRange_mm = logMainText("BBbyCBCTChartYRange_mm", "3.0").toDouble.abs
  val BBbyEPIDChartTolerance_mm = logMainText("BBbyEPIDChartTolerance_mm", "1.0").toDouble.abs
  val BBbyEPIDChartYRange_mm = logMainText("BBbyEPIDChartYRange_mm", "3.0").toDouble.abs

  // =================================================================================

  object Fix extends Enumeration { // TODO temporary for transition
    val ignore = Value
    val check = Value
    val fix = Value
  }

  private def getFixState(name: String): Fix.Value = {
    val list = document \ name
    val fixState = if (list.isEmpty) {
      Fix.ignore
    } else {
      val state = list.head.text match {
        case text if text.equalsIgnoreCase("check") => Fix.check
        case text if text.equalsIgnoreCase("fix") => Fix.fix
        case _ => Fix.ignore
      }
      state
    }
    logText(name, fixState.toString)
    fixState
  }

  val DicomSeriesDeleteOrphans = getFixState("DicomSeriesDeleteOrphans") // TODO temporary for transition
  val DicomSeriesPopulateFromInput = getFixState("DicomSeriesPopulateFromInput") // TODO temporary for transition
  val DicomSeriesTrim = getFixState("DicomSeriesTrim") // TODO temporary for transition
  val DicomSeriesOrphanOutputs = getFixState("DicomSeriesOrphanOutputs") // TODO temporary for transition
  val DicomSeriesUnlinkInputPK = getFixState("DicomSeriesUnlinkInputPK") // TODO temporary for transition
  val DicomSeriesFindBadRtplans = getFixState("DicomSeriesFindBadRtplans") // TODO temporary for transition
  val DicomSeriesShared = getFixState("DicomSeriesShared") // TODO temporary for transition
  val DicomSeriesInput = getFixState("DicomSeriesInput") // TODO temporary for transition

  // =================================================================================

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

  override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

  def toHtml = {
    <pre>{ valueText }</pre>
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

