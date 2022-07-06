package org.aqa.webrun.phase2

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.Logging

import java.awt.Point
import java.awt.geom.Point2D
import javax.vecmath.Point2d

class CollimatorCenteringResource(collimatorCentering: Seq[CollimatorCentering], runReq: RunReq) extends Logging {

  /**
    * Convert an instance of collimator centering to whatever is needed.
    * @param makeIt Conversion function.
    * @tparam T Resulting type of conversion.
    * @return Map of gantry angle -> T
    */
  private def anyToCardinalAngleMap[T](makeIt: CollimatorCentering => T) = {
    if (collimatorCentering.size == 1) {
      val it = makeIt(collimatorCentering.head)
      Seq((0, it), (90, it), (180, it), (270, it)).toMap
    } else
      collimatorCentering.map(cc => (cc.gantryAngleRounded_deg, makeIt(cc))).toMap
  }

  private val angleToCollimatorCenteringMap: Map[Int, CollimatorCentering] = anyToCardinalAngleMap(cc => cc)

  /**
    * Get the collimator centering value corresponding to the given gantry angle.
    * @param gantryAngle For this angle.
    * @return Collimator centering value.
    */
  def angleToCollimatorCentering(gantryAngle: Double): CollimatorCentering = angleToCollimatorCenteringMap(Util.angleRoundedTo90(gantryAngle))

  /**
    * Cache center values for quick, easy retrieval.
    */
  private val angleToCenter: Map[Int, Point2D.Double] = anyToCardinalAngleMap(cc => cc.center)

  /**
    * Given a gantry angle, return the center point to be used.  If this is Phase2, then all gantry
    * angles return the same center point.  If Phase3, then return the center point that was captured
    * at that gantry angle.
    * @param gantryAngle Rounded to nearest 90 degrees.
    * @return Collimator centering point.
    */
  def center(gantryAngle: Double): Point2D.Double = angleToCenter(Util.angleRoundedTo90(gantryAngle))

  /**
    * Map of center point lists so that they only need to be calculated once.
    */
  private val angleToCenterDosePointList: Map[Int, Seq[Point]] = anyToCardinalAngleMap(cc => Phase2Util.makeCenterDosePointList(runReq.derivedMap(cc.beamName090).attributeList, cc.center))

  /**
    * Given a gantry angle, return the point list for the center dose.  If this is Phase2, then all gantry
    * angles return the same point list.  If Phase3, then return the center point list adjusted for that
    * gantry angle.
    * @param gantryAngle For this gantry angle.
    * @return List of points comprising center dose.
    */
  def centerPointList(gantryAngle: Double): Seq[Point] = angleToCenterDosePointList(Util.angleRoundedTo90(gantryAngle))

  /**
    * Given a beam name, determine what its gantry angle is rounded to the nearest 90 degrees.  If the RTPLAN has more
    * than one gantry angle for the given beam, then take the average of the beams.  Note that the 'middle' is 180 degrees,
    * so if one of the beam's gantry angle is 350 degrees, it is evaluated as -10 degrees.
    * @param beamName Find angle for beam with this name.
    * @return Gantry angle of beam.
    */
  private def gantryAngleOfBeam(beamName: String): Int = {
    val seq = Phase2Util.getBeamSequenceOfPlan(beamName, runReq.rtplan)
    val gantryAngleList = DicomUtil.findAllSingle(seq, TagByName.GantryAngle).map(_.getDoubleValues.head)

    def distanceTo(angle: Double, angleRounded: Int): Double = {
      val rad1 = Math.toRadians(Util.modulo360(angle))
      val point1 = new Point2d(Math.cos(rad1), Math.sin(rad1))

      val rad2 = Math.toRadians(Util.modulo360(angleRounded))
      val point2 = new Point2d(Math.cos(rad2), Math.sin(rad2))

      point1.distance(point2)
    }

    def sumOfDistances(angleRounded: Int): Double = gantryAngleList.map(ga => distanceTo(ga, angleRounded)).sum

    val closest = Seq(0, 90, 180, 270).minBy(sumOfDistances)

    logger.info("Gantry angles in beam " + beamName + " : " + gantryAngleList.map(Util.fmtDbl).mkString("  ") + " : closest multiple of 90: " + closest)

    closest
  }

  /**
    * Given a beam name, get the center.
    * @param beamName RTPLAN beam name.
    * @return Collimator center.
    */
  def centerOfBeam(beamName: String): Point2D.Double = center(gantryAngleOfBeam(beamName))

  /**
    * Given a beam name, get the center point list.
    * @param beamName RTPLAN beam name.
    * @return List of points within a circle in center of image.
    */
  def centerPointListOfBeam(beamName: String): Seq[Point] = centerPointList(gantryAngleOfBeam(beamName))

  /**
    * Given a beam name, get the collimator centering value to be used.
    * @param beamName RTPLAN beam name.
    * @return Collimator centering.
    */
  def collimatorCenteringOfBeam(beamName: String): CollimatorCentering = {
    if (true) { // TODO rm
      val ga = gantryAngleOfBeam(beamName)
      val cc = angleToCollimatorCentering(ga)

      Trace.trace("Collimator centering  beam: " + beamName + "    beam gantry angle: " + ga + "    cc: " + cc.center + " : " + cc.beamName090 + " + " + cc.beamName270)
    }
    angleToCollimatorCentering(gantryAngleOfBeam(beamName))
  }
}
