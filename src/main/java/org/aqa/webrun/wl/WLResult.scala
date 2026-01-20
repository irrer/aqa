package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.WinstonLutz
import org.aqa.db.WinLutz360
import org.aqa.webrun.ExtendedData
import org.aqa.Util

import java.io.File
import java.util.Date
import javax.vecmath.Point2d

abstract class WLResult(extendedData: ExtendedData, runReq: WLRunReq) {

  def boxCenter_mm: Point2d
  def ballCenter_mm: Point2d

  def offsetX_mm: Double
  def offsetY_mm: Double

  def offsetXY_mm: Double = Math.sqrt((offsetX_mm * offsetX_mm) + (offsetY_mm * offsetY_mm))

  def getImageStatus: WLImageStatus.Value

  def convertToDB: Either[WinstonLutz, WinLutz360]

  def attrList: AttributeList

  def contentTime: Date = WLImageUtil.timeOf(attrList)

  /** Elapsed time in ms of this slice since the first slice in the series was captured. */
  def elapsedTime_ms: Long = {
    val ms = contentTime.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  def attr(tag: AttributeTag): String = {
    DicomUtil.findAllSingle(attrList, tag).map(_.getSingleStringValueOrEmptyString()).head
  }

  def gantry_deg: Double = Util.gantryAngle(attrList)
  def collimator_deg: Double = Util.collimatorAngle(attrList)
  def collimatorRoundedTo90: Int = Util.angleRoundedTo90(collimator_deg)

  def gantryRounded_deg: Int = Util.angleRoundedTo1(gantry_deg)
  def collimatorRounded_deg: Double = {
    Util.angleRoundedToTenthExceptCardinal(collimator_deg)
  }
  def tableAngle_deg: Double = attrList.get(TagByName.PatientSupportAngle).getDoubleValues.head

  def isCardinal: Boolean = {
    val isCard = ((collimatorRoundedTo90 - collimator_deg).abs < 1) || (collimator_deg > 359)
    isCard
  }


  def gantryRounded_txt: String = "G" + "%03d".format(gantryRounded_deg)
  def collimatorRounded_txt: String = "C" + {
    if (collimatorRounded_deg == collimatorRounded_deg.round)
      "%03d".format(collimatorRounded_deg.round)
    else
      "%05.1f".format(collimatorRounded_deg)
  }

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

  def beamName: Option[String] = {
    if (runReq.rtplan.isDefined)
      Util.getBeamNameOfRtimage(runReq.rtplan.get, attrList)
    else
      None
  }

  def getDirectory: File = subDir

  def getBadPixelList: Seq[WLBadPixel] = Seq()

  def OffsetX1_mm: Option[Double]
  def OffsetX2_mm: Option[Double]
  def OffsetY1_mm: Option[Double]
  def OffsetY2_mm: Option[Double]

  def OffsetTop_mm: Option[Double]
  def OffsetBottom_mm: Option[Double]
  def OffsetLeft_mm: Option[Double]
  def OffsetRight_mm: Option[Double]

}

object WLResult {

  type EitherWL = Either[WinstonLutz, WinLutz360]

  def imageStatus(wl: EitherWL): WLImageStatus.ImageStatus = {
    ???
  }

}
