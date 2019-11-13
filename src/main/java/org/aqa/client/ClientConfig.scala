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
 * to <code>ClientConfig.xml</code> for details indicating what the different
 * configuration values are used for.
 */
object ClientConfig extends ClientConfigUtil with Logging {

  val JavaKeyStorePassword = getJavaKeyStorePassword
  val JavaKeyStoreFileList = getJavaKeyStoreFileList

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime = getHourMinuteTime("RestartTime", "3:45")

  val DataDir = makeDir("DataDir")

  lazy val doneDir = makeChildDir(DataDir, doneDirName)

  lazy val tmpDir = makeChildDir(DataDir, tmpDirName)

  val staticDirFile = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

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
}

