package org.aqa.webrun.psm

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import java.awt.geom.Point2D
import scala.annotation.tailrec

/**
  * Optimize the maximum of the maximum R-squared values
  */

class PSMGradientAscent(psmInterpolator: PSMInterpolator) extends Logging {

  /** Initial size (edge length) of hypercube in mm. */
  private val initialCubeLen_mm: Double = 1.0

  /** Stop after this many iterations, regardless of if the degree of precision is sufficient. */
  private val maxNumberOfIterations: Int = 100000

  /** For each iteration, multiply the cube length by this amount. */
  private val cubeReductionFactor: Double = 0.999

  /** Stop iterating if the cube length becomes this small, indicating that the result is sufficiently precise. */
  private val precision: Double = 1.0e-9

  private case class Pt(pt: Point2D.Double) {
    def this(x: Double, y: Double) = this(new Point2D.Double(x, y))

    val value: Double = psmInterpolator.function.value(pt.getX, pt.getY)
  }

  private val centerPt = Pt(psmInterpolator.trans.iso2Pix(0, 0))

  /** Starting point : center of image. */
  private var bestPoint: Pt = centerPt

  private def updateBestPoint(point: Pt): Unit =
    bestPoint.synchronized {
      if (point.value > bestPoint.value)
        bestPoint = point
    }

  private case class WalkingCube(center: Pt, len: Double, id: Int) {

    private val increment = len / 2

    private val incrementList = Seq(-increment, 0.0, increment)

    // best point for this round
    private var max: Pt = center

    incrementList.foreach(xInc => { //
      val x = center.pt.getX + xInc
      incrementList.foreach(yInc => { //
        val y = center.pt.getY + yInc
        val pt = new Pt(x, y)
        if (pt.value > max.value) max = pt
      })
    })

    updateBestPoint(max)

    def nextCube(nextLen: Double): WalkingCube = WalkingCube(max, nextLen, id)
  }

  /**
    * Recursively walk a cube, searching for a better solution.
    * @param cube Search here
    * @param iteration Current iteration of recursion.  Each iteration increases the precision of the result.
    */
  @tailrec
  private def finder(cube: WalkingCube, iteration: Int): Unit = {
    Trace.trace("Best point: " + psmInterpolator.trans.pix2Iso(bestPoint.pt))
    if ((iteration > 0) && (cube.len > precision)) {
      finder(cube.nextCube(cube.len * cubeReductionFactor), iteration - 1)
    } else {
      val iterationsPerformed = "iterations performed: " + (maxNumberOfIterations - iteration)
      val precision = "Result is precise to within " + cube.len.formatted("%20.17f") + " mm"
      val valuesText = "Calculated values: " + psmInterpolator.trans.pix2Iso(bestPoint.pt)
      logger.info(s"$iterationsPerformed    $precision    $valuesText")
    }
  }

  def findMax(): Point2D.Double = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(centerPt) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_mm, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished gradient ascent.   Elapsed ms: $elapsed     bestPoint: $bestPoint")

    bestPoint.pt
  }
}
