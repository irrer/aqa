package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.db.WinstonLutz
import org.aqa.db.WinstonLutzNonCardinal
import org.aqa.webrun.ExtendedData
import org.aqa.Util

import java.io.File
import java.util.Date

abstract class WLResult(extendedData: ExtendedData, runReq: WLRunReq) {

  def offsetX_mm: Double
  def offsetY_mm: Double

  def offsetXY_mm: Double = Math.sqrt((offsetX_mm * offsetX_mm) + (offsetY_mm * offsetY_mm))

  def getImageStatus: WLImageStatus.Value

  def convertToDB: Either[WinstonLutz, WinstonLutzNonCardinal]

  def attrList: AttributeList

  val contentTime: Date = WLImageUtil.timeOf(attrList)

  /** Elapsed time in ms of this slice since the first slice in the series was captured. */
  def elapsedTime_ms: Long = {
    val ms = contentTime.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  val gantry_deg: Double = Util.gantryAngle(attrList)
  val collimator_deg: Double = Util.collimatorAngle(attrList)

  val gantryRounded_deg: Int = Util.angleRoundedTo90(gantry_deg)
  val collimatorRounded_deg: Int = collimator_deg.round.toInt
  val tableAngle_deg: Double = attrList.get(TagByName.PatientSupportAngle).getDoubleValues.head

  val gantryRounded_txt: String = "G" + "%03d".format(gantryRounded_deg)
  val collimatorRounded_txt: String = "C" + "%03d".format(collimatorRounded_deg)

  def imageName: String = gantryRounded_txt + " " + collimatorRounded_txt + " " + elapsedTime_txt

  def indexOf: Int = runReq.indexOf(attrList)

  private def thisType = if (this.isInstanceOf[WLImageResult]) "Cardinal" else "NonCardinal"

  def subDir: File = {
    val dir = new File(extendedData.output.dir, runReq.subDirName(attrList, thisType))
    dir.mkdirs()
    dir
  }

  def elapsedTime_txt: String = {
    val totalSeconds: Int = (elapsedTime_ms / 1000).toInt
    (totalSeconds / 60) + ":" + "%02d".format(totalSeconds % 60)
  }

  val beamName: Option[String] = {
    if (runReq.rtplan.isDefined)
      Util.getBeamNameOfRtimage(runReq.rtplan.get, attrList)
    else
      None
  }

  def getDirectory: File = subDir

  def getBadPixelList: Seq[WLBadPixel] = Seq()

}

object WLResult {

  type EitherWL = Either[WinstonLutz, WinstonLutzNonCardinal]

  def imageStatus(wl: EitherWL): WLImageStatus.ImageStatus = {
    ???
  }

}
