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
import org.aqa.Logging._
import edu.umro.util.Utility

/**
 * This class extracts configuration information from the configuration file.  Refer
 * to <code>Config.configFileName</code> for details indicating what the different
 * configuration values are used for.
 */
object Config {

    private val configFileName = "AQAConfig.xml";
    private val DEFAULT_RESTART_TIME = "1:20"

    /** Root directory name for static directory. */
    val staticDirName = "static"

    /** Root directory name for test results. */
    val resultsDirName = "results"

    /** Root directory name for temporary files. */
    val tmpDirName = "tmp"

    /** Root directory name for machine configuration files. */
    val machineConfigurationDirName = "MachineConfiguration"

    def makeDataDir(dirName: String): File = {
        val dir = new File(DataDir, dirName)
        dir.mkdirs
        dir
    }

    lazy val resultsDirFile = makeDataDir(resultsDirName)

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
        logInfo("Trying config file " + file.getAbsolutePath + " ...")
        if (file.canRead) {
            try {
                val content = Some(XML.loadFile(file))
                logInfo("Using config file " + file.getAbsolutePath)
                configFile = file
                content
            }
            catch {
                case e: Exception => {
                    logInfo("Failed to use config file " + file.getAbsolutePath + "    file exists: " + file.exists + "    can read file: " + file.canRead + "  Exception: " + e)
                    None
                }
            }
        }
        else {
            if (!file.exists) logInfo("Config file " + file.getAbsoluteFile + " does not exist")
            else logInfo("Config file " + file.getAbsoluteFile + " is not readable")
            None
        }
    }

    /**
     * If a fatal error occurs during the reading of the configuration file, then the application
     * is toast, so log an error and exit with a failed status.
     */
    private def epicFail(name: String) = {
        val tried = directoryList.foldLeft("")((l, f) => l + "\n    " + f.getAbsolutePath)

        logSevere("Could not find a usable configuration file.  Using file name " + name + " , tried directories: " + tried + "\nShutting down...")
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
        }
        catch {
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
        }
        catch {
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

    logFinest("Using configuration:\n" + edu.umro.ScalaUtil.Util.xmlToText(document) + "\n")

    private val valueText = new ArrayBuffer[String]

    private def logText(name: String, value: String) = valueText += (name + ": " + value)

    private def getMainText(name: String): String = {
        if ((document \ name).size == 0) throw new RuntimeException("No such XML node " + name)
        (document \ name).head.text
    }

    private def mainText(name: String): String = {
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

    private def setProperty(node: Node): Unit = {
        val name = (node \ "@Name").head.text
        val value = node.head.text
        System.setProperty(name, value)
        logText("Property " + name, if (name.toLowerCase.contains("password")) "[redacted]" else value)
    }

    private def initProperties: Unit = {
        (document \ "PropertyList" \ "Property").toList.map(node => setProperty(node))

        // force each of these directories to be created.  It would be catastrophic if this fails.
        if (!(Seq(resultsDirFile, tmpDirFile, machineConfigurationDirFile).map(d => (d.isDirectory && d.canRead)).reduce(_ && _)))
            throw new RuntimeException("can not read all necessary directories")
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
                    Logging.logWarning("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath)
                    f
                }
            }
        }
        val jf: File = Util.thisJarFile match {
            case f if f.isDirectory => getDevJar
            case f if (f.isFile && f.canRead) => f
            case f => { Logging.logWarning("Unable to find the jar file being used.  Assuming " + f.getAbsolutePath); f }
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
        }
        catch {
            case _: Throwable => {
                logText(name, "Not specified")
                None
            }
        }

    }

    val JavaKeyStorePassword = getJavaKeyStorePassword
    val JavaKeyStoreFileList = getJavaKeyStoreFileList

    val ProgramDir = getDir("ProgramDir")
    val ProcedureDir = getDir("ProcedureDir")
    val DataDir = getDir("DataDir")

    val RestletLogLevel = mainText("RestletLogLevel")
    val AuthenticationTimeout = mainText("AuthenticationTimeout").toDouble
    val AuthenticationTimeoutInMs = (AuthenticationTimeout * 1000).toLong

    val jarFile = getThisJarFile

    /** Number of minutes into a 24 hour day at which time service should be restarted. */
    val RestartTime: Long = {
        val dateFormat = new SimpleDateFormat("HH:mm")
        val millisec = try {
            dateFormat.parse(mainText("RestartTime")).getTime
        }
        catch {
            case e: ParseException => {
                Log.get.warning("Badly formatted RestartTime in configuration file: " + mainText("RestartTime") + " .  Should be HH:MM, as in 1:23 .  Assuming default of " + DEFAULT_RESTART_TIME)
                dateFormat.parse(DEFAULT_RESTART_TIME).getTime
            }
        }

        millisec
    }

    initProperties

    val UserWhiteList: List[String] = (document \ "UserWhiteList" \ "User").toList.map(node => node.head.text.trim.toLowerCase)

    val TermsOfUse = mainText("TermsOfUse")

    /** If this is defined, then the configuration was successfully initialized. */
    val validated = true
    
    def validate = validated

    override def toString: String = valueText.foldLeft("Configuration values:")((b, t) => b + "\n    " + t)

    def toHtml = {
        { valueText.map(line => <br>{ line } </br>) }.toSeq
    }

    logInfo(toString)

    def main(args: Array[String]): Unit = {
        val startTime = System.currentTimeMillis
        println("validate: " + validate) // loading forces configuration to be read
        val elapsed = System.currentTimeMillis - startTime
        println("Elapsed time in ms: " + elapsed)
        System.exit(99)
    }
}


