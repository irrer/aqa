package learn

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil

import java.awt.geom.Point2D

case class Rotator(rtimage: AttributeList) {
  val trans: IsoImagePlaneTranslator = new IsoImagePlaneTranslator(rtimage)

  private val XRayImageReceptorTranslation = rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues
  private val offsetX = XRayImageReceptorTranslation.head
  private val offsetY = XRayImageReceptorTranslation(1)

  private val angle: Double = DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head

  private val radians: Double = Math.toRadians(angle)
  private val cos = Math.cos(radians)
  private val sin = Math.sin(radians)

  private val jaws = DicomUtil.findAllSingle(rtimage, TagByName.LeafJawPositions)

  val jawsXLeft: Double = jaws.head.getDoubleValues.head
  val jawsXRight: Double = jaws.head.getDoubleValues()(1)

  private val jawsYTopStd = jaws(1).getDoubleValues.head
  private val jawsYBottomStd = jaws(1).getDoubleValues()(1)

  val jawsYTop: Double = -jawsYTopStd
  val jawsYBottom: Double = -jawsYBottomStd

  /**
   * Rotate and offset the given iso point according to the rtimage.
   * @param point Rotate this.
   * @return New point in iso coordinates.
   */
  def rot(point: Point2D.Double): Point2D.Double = {

    val y = -point.getY

    val xRot = (point.getX * cos) - (y * sin)
    val yRot = (point.getX * sin) + (y * cos)

    val xFinal = xRot - (offsetX / trans.beamExpansionRatio)
    val yFinal = -(yRot - (offsetY / trans.beamExpansionRatio))

    new Point2D.Double(xFinal, yFinal)
  }
}
