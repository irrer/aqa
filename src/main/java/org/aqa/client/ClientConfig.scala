package org.aqa.client

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
import org.aqa.Logging
import org.aqa.Util
import edu.umro.ScalaUtil.PACS

/**
 * This class extracts configuration information from the configuration file.  Refer
 * to <code>ClientConfig.configFileName</code> for details indicating what the different
 * configuration values are used for.
 */
object ClientConfig extends Logging {

  private val configFileName = "AQAClientConfig.xml";
  private val DEFAULT_RESTART_TIME = "1:30"

  logger.info("Starting client configuration.  File name: " + configFileName)

  /** Root directory name for static directory. */
  val staticDirName = "static"

  /** Directory name for test results. */
  val doneDirName = "done"

  /** Directory name for temporary files. */
  val tmpDirName = "tmp"

  /** For indenting sub-content. */
  private val indent1 = "\n                  "

  /** For indenting sub-sub-content. */
  private val indent2 = indent1 + "    "

  private def indentList[T](list: Seq[T]): String = list.mkString(indent1, indent1, "\n")

  private def fail(msg: String) {
    logger.error(msg)
    throw new RuntimeException(msg)
  }

  private def getDir(name: String): File = {
    val dir = new File(getMainText(name))
    logText(name, dir.getAbsolutePath)
    dir.mkdirs
    dir
  }

  def makeDataDir(dirName: String): File = {
    val dir = new File(DataDir, dirName)
    dir.mkdirs
    dir
  }

  lazy val doneDir = makeDataDir(doneDirName)

  lazy val tmpDir = makeDataDir(tmpDirName)

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
    list.head.text
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

  val JavaKeyStorePassword = getJavaKeyStorePassword
  val JavaKeyStoreFileList = getJavaKeyStoreFileList

  val RestletLogLevel = logMainText("RestletLogLevel")
  val AuthenticationTimeout = logMainText("AuthenticationTimeout").toDouble
  val AuthenticationTimeoutInMs = (AuthenticationTimeout * 1000).toLong

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

  /**
   * Directory containing the definitive static files.
   */
  val staticDirFile: File = {
    val locations = List(""".\""", """src\main\resources\""").map(name => new File(name + ClientConfig.staticDirName))

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

  private def getPacs(tag: String): PACS = {
    new PACS((document \ tag).head)
  }

  private def getAMQPBroker = None // TODO

  private def getPatientIDList = {
    val list = (document \ "PatientIDList" \ "PatientID").map(node => node.head.text.toString.trim)
    list
  }

  private def requireReadableDirectory(name: String, dir: File) = {
    if (!dir.canRead) fail("Directory " + name + " is not readable: " + dir)
    if (!dir.isDirectory) fail("Directory " + name + " is required but is not a directory: " + dir)
  }

  override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

  def toHtml = {
    <pre>{ valueText }</pre>
  }

  val DataDir = getDir("DataDir")
  val PatientIDList = getPatientIDList
  val DICOMClient = getPacs("DICOMClient")
  val DICOMSource = getPacs("DICOMSource")
  val AQAURL = getMainText("AQAURL")
  val HTTPSPort = logMainText("HTTPSPort", "443").toInt
  val AMQPBroker = getAMQPBroker

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

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

