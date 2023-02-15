package org.aqa.webrun.wl

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import scala.annotation.tailrec

/**
  * Construct a complete set of 4 gantry angles with the same collimator angle and
  * calculate corrections to be made in the X, Y and Z axis.
  */
class WLLaserCorrection(val resultList: Seq[WLImageResult]) extends Logging {

  if (!WLLaserCorrection.startsWithSet(resultList)) throw new RuntimeException("Set of results does not start with a proper set of gantry angles")

  val passedText = "PASSED"

  val failedText = "LIMIT EXCEEDED"

  private def find(g: Int): WLImageResult = resultList.filter(ir => ir.gantryRounded_deg == g).head

  private def offX(g: Int): Double = find(g).offX
  private def offY(g: Int): Double = find(g).offY

  val longitudinal: Double = (offY(180) + offY(0)) / 2

  val lateral: Double = (offX(180) - offX(0)) / 2

  val vertical: Double = (offX(90) - offX(270)) / 2

  val passed: Boolean = (longitudinal.abs <= Config.WLLaserCorrectionLimit) && (lateral.abs <= Config.WLLaserCorrectionLimit) && (vertical.abs <= Config.WLLaserCorrectionLimit)

  /** Description of status. */
  val statusText: String = if (passed) passedText else failedText

  val logValues: Unit = {
    def show(g: Int): String = {
      val ir = resultList.filter(ir => ir.gantryRounded_deg == g).head
      "    " + ir.collimatorRounded_txt + "    " + ir.gantryRounded_txt + "    xOff: " + ir.offX + "    yOff: " + ir.offY
    }
    val text = (0 to 270 by 90).map(g => show(g)).foldLeft("")((t, r) => t + "\n" + r)
    logger.info("Values used for correction:" + text)
  }

  override def toString: String = {
    "longitudinal: " + longitudinal + "    lateral: " + lateral + "    vertical: " + vertical
  }

  private def isFirstInList(imageResult: WLImageResult): Boolean = {
    Util.sopOfAl(imageResult.rtimage).equals(Util.sopOfAl(resultList.head.rtimage))
  }
}

object WLLaserCorrection {

  private val NUM_IN_SET = 4 // Number of fields in a set (covers each multiple of 90 degrees).

  private def startsWithSet(resultList: Seq[WLImageResult]): Boolean = {
    def collSame = resultList.take(NUM_IN_SET).map(ir => ir.collimatorRounded_deg).distinct.size == 1
    def gantryDifferent = resultList.take(NUM_IN_SET).map(ir => ir.gantryRounded_deg).distinct.size == NUM_IN_SET
    (resultList.size >= 4) && collSame && gantryDifferent
  }

  @tailrec
  def setList(resultList: Seq[WLImageResult], correctionList: Seq[WLLaserCorrection] = Seq()): Seq[WLLaserCorrection] = {
    if (resultList.size < NUM_IN_SET) correctionList
    else {
      if (startsWithSet(resultList.take(NUM_IN_SET))) {
        val corList = correctionList.+:(new WLLaserCorrection(resultList.take(NUM_IN_SET)))
        setList(resultList.drop(NUM_IN_SET), corList)
      } else setList(resultList.tail, correctionList)
    }
  }

  /*
  def setList(resultList: Seq[WLImageResult]): Seq[WLLaserCorrection] = {
    setList(resultList, Seq[WLLaserCorrection]())
  }
   */

  def getCorrectionOfImage(laserCorrectionList: Seq[WLLaserCorrection], imageResult: WLImageResult): Option[WLLaserCorrection] = {
    val lcList = laserCorrectionList.filter(lc => lc.isFirstInList(imageResult))
    lcList.headOption
  }
}
