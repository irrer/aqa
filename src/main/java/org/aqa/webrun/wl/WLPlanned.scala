package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomBeam
import org.aqa.Logging
import org.aqa.Util

case class WLPlanned(rtplan: AttributeList, rtimage: AttributeList) extends Logging {

  val cps: DicomBeam = DicomBeam(rtplan, rtimage)

  val x1: Double = {
    val mlc = {
      if (cps.mlcX1PosList.isEmpty)
        None
      else
        Some(cps.mlcX1PosList.filterNot(_ == cps.mlcX1PosList.head).head) // ignore any with the head value
    }
    Seq(mlc, cps.x1Jaw).flatten.max
  }

  val x2: Double = {
    val mlc = {
      if (cps.mlcX2PosList.isEmpty)
        None
      else
        Some(cps.mlcX2PosList.filterNot(_ == cps.mlcX2PosList.head).head) // ignore any with the head value
    }
    Seq(mlc, cps.x2Jaw).flatten.min
  }

  /** The list of leaves that span X1 to X2. */
  private val notSpanList = {
    def doesSpan(i: Int): Boolean = {
      (cps.mlcX1PosList(i) > x2) || (cps.mlcX2PosList(i) < x1)
    }
    cps.mlcX1PosList.indices.filterNot(doesSpan)
  }

  val y1: Double = {
    val mlc: Option[Double] = {
      if (notSpanList.isEmpty)
        None
      else
        Some(cps.leafBoundaryList(notSpanList.last + 1))
    }

    Seq(cps.y1Jaw, mlc).flatten.max
  }

  val y2: Double = {
    val mlc: Option[Double] = {
      if (notSpanList.isEmpty)
        None
      else
        Some(cps.leafBoundaryList(notSpanList.head))
    }

    Seq(cps.y2Jaw, mlc).flatten.min
  }

  val colRounded: Int = Util.angleRoundedTo90(cps.rtimageCollimatorAngle)

  /** Special note: There was no test data for collimator 180 so that has never been tested. */

  /** Top of field. */
  val top: Double = colRounded match {
    case 0   => y2
    case 90  => -x2
    case 180 => -y1
    case 270 => x1
  }

  /** Bottom of field. */
  val bottom: Double = colRounded match {
    case 0   => y1
    case 90  => -x1
    case 180 => -y2
    case 270 => x2
  }

  /** Left side of field. */
  val left: Double = colRounded match {
    case 0   => x1
    case 90  => y2
    case 180 => -x2
    case 270 => -y1
  }

  /** Right side of field. */
  val right: Double = colRounded match {
    case 0   => x2
    case 90  => y1
    case 180 => -x1
    case 270 => -y2
  }

  override def toString: String = {
    s"colAngle: $colRounded    X1: $x1    X2: $x2    Y1: $y1    Y2: $y2    top: $top    bottom: $bottom    left: $left    right: $right"
  }

}
