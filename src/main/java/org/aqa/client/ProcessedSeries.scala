package org.aqa.client

import java.util.Date
import org.aqa.Util
import scala.xml.Node
import java.io.File
import scala.xml.XML
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import java.io.FileWriter
import scala.xml.PrettyPrinter

case class ProcessedSeries(SeriesInstanceUID: String, PatientID: String, Modality: String, dataDate: Date, ProcessDate: Date) extends Logging {

  def this(node: Node) = this(
    Util.getAttr(node, "SeriesInstanceUID"),
    Util.getAttr(node, "PatientID"),
    Util.getAttr(node, "Modality"),
    Util.standardDateFormat.parse(Util.getAttr(node, "dataDate")),
    Util.standardDateFormat.parse(Util.getAttr(node, "ProcessDate")))

  def toXml = {
    <ProcessedSeries SeriesInstanceUID={ SeriesInstanceUID } PatientID={ PatientID } Modality={ Modality } dataDate={ Util.standardDateFormat.format(dataDate) } ProcessDate={ Util.standardDateFormat.format(ProcessDate) }/>
  }

  def toText = ProcessedSeries.prettyPrinter.format(toXml)

  override def toString = {
    "SeriesInstanceUID : " + SeriesInstanceUID +
      "    PatientID : " + PatientID +
      "    Modality : " + Modality +
      "    dataDate : " + dataDate +
      "    ProcessDate : " + ProcessDate
  }
}

object ProcessedSeries extends Logging {

  private val fileName = "ProcessedSeries.xml"
  private val file = new File(ClientConfig.DataDir, fileName)
  private val prettyPrinter = new PrettyPrinter(1024, 2)

  private val ProcessedSeriesMap = scala.collection.mutable.HashMap[String, ProcessedSeries]()

  /**
   * Get the ProcessedSeries with the given SeriesInstanceUID.
   */
  def get(SeriesInstanceUID: String): Option[ProcessedSeries] = ProcessedSeriesMap.synchronized(ProcessedSeriesMap.get(SeriesInstanceUID))

  /**
   * Construct a ProcessedSeries from an AttributeList.
   */
  private def constructProcessedSeries(al: AttributeList): ProcessedSeries = {
    val dateTimeTagPairList = Seq(
      (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    def getDateTime(dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
      try {
        val d = al.get(dateTag).getSingleStringValueOrNull
        val t = al.get(timeTag).getSingleStringValueOrNull
        val ms = DicomUtil.dicomDateFormat.parse(d).getTime + DicomUtil.parseDicomTime(t).get
        Some(new Date(ms))
      } catch {
        case t: Throwable => None
      }
    }

    val dataDate = dateTimeTagPairList.map(dtp => getDateTime(dtp._1, dtp._2)).flatten.head

    val SeriesInstanceUID = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
    val PatientID = al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
    val Modality = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString

    new ProcessedSeries(SeriesInstanceUID, PatientID, Modality, dataDate, new Date)
  }

  def add(processedSeries: ProcessedSeries): Unit = {
    ProcessedSeriesMap.synchronized({
      ProcessedSeriesMap += ((processedSeries.SeriesInstanceUID, processedSeries))
      val fw = new FileWriter(file, true) //the true will append the new data
      fw.write(processedSeries.toText) //appends the string to the file
      fw.close
    })
  }

  /**
   * Read the list of series that have already been processed.
   */
  private def readProcessedSeries: Unit = {
    if (file.canRead) {
      val xmlText = "<ProcessedSeriesList>" + Util.readDicomFile(file) + "<ProcessedSeriesList/>"
      val doc = XML.loadString(xmlText)
      ProcessedSeriesMap.synchronized(
        (doc \ "ProcessedSeries").map(node => new ProcessedSeries(node)).map(ps => ProcessedSeriesMap += ((ps.SeriesInstanceUID, ps))))
      logger.info("Read " + ProcessedSeriesMap.size + " series entries from " + file.getAbsolutePath)
    } else {
      file.getParentFile.mkdirs
      file.createNewFile
    }

  }

  def init = {
    readProcessedSeries

  }
}