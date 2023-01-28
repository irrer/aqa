package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.aqa.Util
import org.aqa.run.RunProcedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.emptyValueMap
import org.aqa.web.WebUtil.getValueMap
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem

class WLRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[WLRunReq] {

  private def dateTime(al: AttributeList) = {
    val dt = Seq(
      DicomUtil.getTimeAndDate(al, TagByName.ContentDate, TagByName.ContentTime),
      DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime)
    ).flatten.head
    dt.getTime
  }

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(al => Util.isRtimage(al)).sortBy(dateTime)

  override def run(extendedData: ExtendedData, runReq: WLRunReq, response: Response): ProcedureStatus.Value = {
    val resultList = runReq.epidList.map(rtimage => (new WLProcessImage(extendedData, rtimage)).process)
    val mainHtmlText = WLMainHtml.generateGroupHtml(extendedData, resultList)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, mainHtmlText)
    logger.info("Wrote main HTML file " + file.getAbsolutePath)

    if (false) { // TODO replace
      val outDir = extendedData.output.dir
      val subDirList = Util.listDirFiles(outDir).filter(_.isDirectory)
      def d2ref(d: File) = <p><a href={d.getName + "/diagnostics.html"}>{d.getName}</a></p>
      val file = new File(outDir, Output.displayFilePrefix + ".html")
      val content = {
        <div>
            {subDirList.map(d2ref)}
          </div>
      }
      val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "Winston Lutz")
      Util.writeFile(file, text)
      ProcedureStatus.done // TODO put real status here
    } else
      ProcedureStatus.done // TODO put real status here
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {

    val epidList = getRtimageList(alList)

    val epidSeriesList = epidList.map(Util.serInstOfAl).distinct

    val gantryAngleList = epidList.map(Util.gantryAngle).map(Util.angleRoundedTo90).distinct.sorted
    val orthogonalAngleList = gantryAngleList.map(_ % 180).distinct

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, WLRunReq] = 0 match {
      case _ if epidList.isEmpty              => formError("No EPID files uploaded")
      case _ if epidSeriesList.size > 1       => formError("EPID images are from " + numSeries + " different series.")
      case _ if orthogonalAngleList.size != 2 => formError("Need to have images with both vertical and horizontal gantry angles.  Given beam had " + gantryAngleList.mkString("  "))
      case _ =>
        val runReq = WLRunReq(epidList.sortBy(dateTime))
        Right(runReq)
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): WLRunReq = {
    val result = WLRunReq(getRtimageList(alList))
    result
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    val list = getRtimageList(alList).map(al => Util.patientIdOfAl(al)).distinct
    list.headOption
  }

  /**
    * Get the earliest date+time stamp of the RTIMAGES by evaluating all occurrences of Content Date+Time and
    * Acquisition Date+Time.
    *
    * @param valueMap Not used.
    * @param alList Seq of incoming DICOM files.
    * @param xmlList Not used.
    * @return Earliest date+time if available.
    */
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    getRtimageList(alList).map(dateTime).map(ms => new Timestamp(ms)).headOption
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {

    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }
}
