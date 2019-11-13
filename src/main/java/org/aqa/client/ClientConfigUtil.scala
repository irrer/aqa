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
 * Utilities to support configuration.
 *
 * @param configFileName: Name of configuration file
 *
 * @param directoryList: directoryList Search these directories in the order given to find the configuration file.
 */
class ClientConfigUtil(configFileName: String, directoryList: Seq[File]) extends Logging {

  private val DEFAULT_RESTART_TIME = "1:30"

  /** Root directory name for static directory. */
  protected val staticDirName = "static"

  /** Directory name for test results. */
  protected val doneDirName = "done"

  /** Directory name for temporary files. */
  protected val tmpDirName = "tmp"

  private def indentList[T](list: Seq[T]): String = {
    val indent1 = "\n                  "
    val indent2 = indent1 + "    "
    list.mkString(indent1, indent1, "\n")
  }

  private def fail(msg: String) {
    logger.error(msg)
    throw new RuntimeException(msg)
  }

  protected def makeDir(attrName: String): File = {
    val dir = new File(getMainText(attrName))
    logText(attrName, dir.getAbsolutePath)
    dir.mkdirs
    dir
  }

  protected def makeChildDir(parentDir: File, childName: String): File = {
    val childDir = new File(parentDir, childName)
    childDir.mkdirs
    logText("Created child dir: ", childDir.getAbsolutePath)
    childDir
  }

  //  protected def getChildFile(parentDir: File, fileName: String): File = {
  //    val file = new File(parentDir, fileName)
  //    logText("Using file: ", file.getAbsolutePath)
  //    file
  //  }

  private var configFile: File = null

  protected def getConfigFile = configFile

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

  private def getDoc(dirList: Seq[File], name: String): Option[Elem] = {
    if (dirList.length < 1) None
    val elem = readFile(dirList.head, name)
    elem match {
      case Some(el) => elem
      case _ => getDoc(dirList.tail, name)
    }
  }

  protected def getPassword(name: String): String = {
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

  protected def getJavaKeyStoreFileList: List[File] = {
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

  protected def getMainText(name: String): String = {
    val list = document \ name
    if (list.isEmpty) fail("No such XML node " + name)
    list.head.text
  }

  private def getMainTextOption(name: String): Option[String] = {
    try {
      val list = document \ name
      if (list.isEmpty)
        None
      else
        Some(list.head.text)
    } catch {
      case t: Throwable => None
    }
  }

  protected def logMainText(name: String): String = {
    println("Name: " + name) // TODO rm
    val value = getMainText(name)
    logText(name, value)
    value
  }

  /**
   * Get the value matching the given name.  If it does not exist or there is
   * some sort of other problem then return the default.
   */
  protected def logMainText(name: String, default: String): String = {
    getMainTextOption(name) match {
      case Some(value: String) => {
        logText(name, value)
        value
      }
      case _ => {
        logText("using default value for " + name, default)
        default
      }
    }
  }

  /** Get time as hours:minutes. */
  protected def getHourMinuteTime(name: String, default: String): Long = {
    val dateFormat = new SimpleDateFormat("HH:mm")
    val millisec = try {
      dateFormat.parse(logMainText(name)).getTime
    } catch {
      case e: ParseException => {
        Log.get.warning("Badly formatted HH:MM time in configuration file: " + logMainText(name) + " .  Should be HH:MM, as in 1:23 .  Assuming default of " + default)
        dateFormat.parse(default).getTime
      }
    }

    millisec
  }

  /**
   * Search for the given directory (which should already exist) in the given list of parent directories..
   */
  protected def getExistingDir(dirName: String, parentPathList: Seq[String]): File = {
    def toDir(parentName: String): File = {
      val parentDir = new File(parentName)
      val dir = new File(parentDir, dirName)
      dir
    }
    val dirList = parentPathList.map(parentName => toDir(parentName))

    val existingDirList = dirList.filter(dir => dir.isDirectory)

    if (existingDirList.isEmpty) {
      val msg = "Unable to find directory in\n    " + existingDirList.map(d => d.getAbsolutePath).mkString("\n    ")
      logger.error(msg)
      throw new RuntimeException(msg)
    }

    val dir = existingDirList.head
    logText(dirName, dir.getAbsolutePath)
    dir
  }

  protected def getPacs(tag: String): PACS = {
    val pacs = new PACS((document \ tag).head)
    logText(tag, pacs.toString)
    pacs
  }

  protected def getAMQPBroker = None // TODO

  protected def getPatientIDList = {
    val list = (document \ "PatientIDList" \ "PatientID").map(node => node.head.text.toString.trim)
    list
  }

  private def requireReadableDirectory(name: String, dir: File) = {
    if (!dir.canRead) fail("Directory " + name + " is not readable: " + dir)
    if (!dir.isDirectory) fail("Directory " + name + " is required but is not a directory: " + dir)
  }

  override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

  protected def toHtml = {
    <pre>{ valueText }</pre>
  }

}

