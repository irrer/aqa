package org.aqa.webrun.wl.nonCardinal

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import javax.vecmath.Point2d
import scala.annotation.tailrec

class WLNonCardBallFinder(val wlNonCardBall: WLNonCardBall, initialCenterPoint: Point2d, initialCubeLen_pix: Double) extends Logging {

  /** Stop after this many iterations, regardless of if the degree of precision is sufficient. */
  private val maxNumberOfIterations: Int = 100000

  /** For each iteration, multiply the cube length by this amount. */
  private val cubeReductionFactor: Double = 0.9 // 0.7072

  /** Stop iterating if the cube length becomes this small, indicating that the result is sufficiently precise. */
  private val precision: Double = 1.0e-10

  /**
    * Store a point and the interpolation at that point.
    * @param x X coordinate
    * @param y Y coordinate
    */
  case class Pt(x: Double, y: Double) {
    val p2d: Point2d = new Point2d(x, y)
    val value: Double = wlNonCardBall.evaluate(p2d)
  }

  private val centerPt = Pt(initialCenterPoint.getX, initialCenterPoint.getY)

  /** Starting point : center of image. */
  private var maxPoint: Pt = Pt(initialCenterPoint.getX, initialCenterPoint.getY)

  private var prevMaxPoint: Pt = Pt(initialCenterPoint.getX, initialCenterPoint.getY)

  /**
    * Get the point closest to center of ball.
    * @return The point closest to center of ball.
    */
  def getMaxPoint_iso: Pt = maxPoint

  private def updateMaxPoint(point: Pt): Unit =
    maxPoint.synchronized {
      if (point.value > maxPoint.value) {
        prevMaxPoint = maxPoint
        maxPoint = point
        Trace.trace(s"Change in max point: ${maxPoint.p2d.distance(prevMaxPoint.p2d)}")
      }
    }

  private case class WalkingCube(center: Pt, len: Double, id: Int) {

    private val increment = len / 2

    private val incrementList = Seq(-increment, 0.0, increment)

    private val pointList: Seq[Pt] = incrementList.flatMap(xInc => { //
      val x = center.p2d.getX + xInc
      incrementList.map(yInc => { //
        val y = center.p2d.getY + yInc
        new Pt(x, y)
      })
    })

    private val maxPoint = pointList.maxBy(p => p.value)

    updateMaxPoint(maxPoint)

    def nextCube(nextLen: Double): WalkingCube = WalkingCube(maxPoint, nextLen, id)
  }

  /**
    * Recursively walk a cube, searching for a better solution.
    * @param cube Search here
    * @param iteration Current iteration of recursion.  Each iteration increases the precision of the result.
    */
  @tailrec
  private def finder(cube: WalkingCube, iteration: Int): Unit = {

    if ((iteration > 0) && (cube.len > precision)) {
      if ((iteration % 10) == 0) // TODO rm
        Trace.trace(s"iteration: $iteration   cube.len: ${cube.len}   x: ${maxPoint.x}     y: ${maxPoint.y}       max: ${maxPoint.value}")
      finder(cube.nextCube(cube.len * cubeReductionFactor), iteration - 1)
    } else {
      val iterationsPerformed = "iterations performed: " + (maxNumberOfIterations - iteration)
      val precision = "Result is precise to within " + "%20.17f".format(cube.len) + " mm"
      logger.info(s"$iterationsPerformed    $precision")
    }
  }

  /**
    * Find the maximum point via gradient descent.
    */
  private def findMax(): Unit = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(centerPt) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_pix, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished WL ball gradient descent.   Elapsed ms: $elapsed     maxPoint: $maxPoint")
  }

  // run algorithm and set max point on object construction
  private def run(): Unit = {
    val start = System.currentTimeMillis()
    findMax()
    val elapsed = System.currentTimeMillis() - start

    def fmt(d: Double) = "%20.15f".format(d)
    logger.info(s"Final ball location: x: ${fmt(maxPoint.x)}   y: ${fmt(maxPoint.y)}   mean pixel value: ${fmt(maxPoint.x)}   elapsed ms: $elapsed")
  }

  run()
}
