package org.aqa.webrun.psm

import org.aqa.Logging

import java.awt.geom.Point2D
import scala.annotation.tailrec

/**
  * Find the maximum point in the PSM image.
  * @param psmInterpolator Provides 2D interpolator algorithm.
  */
class PSMGradientAscent(val psmInterpolator: PSMInterpolator) extends Logging {

  /** Initial size (edge length) of hypercube in mm. */
  private val initialCubeLen_mm: Double = 1.0

  /** Stop after this many iterations, regardless of if the degree of precision is sufficient. */
  private val maxNumberOfIterations: Int = 100000

  /** For each iteration, multiply the cube length by this amount. */
  private val cubeReductionFactor: Double = 0.999

  /** Stop iterating if the cube length becomes this small, indicating that the result is sufficiently precise. */
  private val precision: Double = 1.0e-10

  private case class Pt(pt: Point2D.Double) {
    def this(x: Double, y: Double) = this(new Point2D.Double(x, y))

    val value: Double = psmInterpolator.function.value(pt.getX, pt.getY)
  }

  private val centerPt = Pt(psmInterpolator.trans.iso2Pix(0, 0))

  /** Starting point : center of image. */
  private var maxPoint: Pt = centerPt

  /**
    * Get the highest point in the PSM image.
    * @return The highest point in the PSM image.
    */
  def getMaxPoint_iso: Point2D.Double = psmInterpolator.trans.pix2Iso(maxPoint.pt)

  private def updateMaxPoint(point: Pt): Unit =
    maxPoint.synchronized {
      if (point.value > maxPoint.value)
        maxPoint = point
    }

  private case class WalkingCube(center: Pt, len: Double, id: Int) {

    private val increment = len / 2

    private val incrementList = Seq(-increment, 0.0, increment)

    // max point for this round
    private var max: Pt = center

    incrementList.foreach(xInc => { //
      val x = center.pt.getX + xInc
      incrementList.foreach(yInc => { //
        val y = center.pt.getY + yInc
        val pt = new Pt(x, y)
        if (pt.value > max.value) max = pt
      })
    })

    updateMaxPoint(max)

    def nextCube(nextLen: Double): WalkingCube = WalkingCube(max, nextLen, id)
  }

  /**
    * Recursively walk a cube, searching for a better solution.
    * @param cube Search here
    * @param iteration Current iteration of recursion.  Each iteration increases the precision of the result.
    */
  @tailrec
  private def finder(cube: WalkingCube, iteration: Int): Unit = {
    if ((iteration > 0) && (cube.len > precision)) {
      finder(cube.nextCube(cube.len * cubeReductionFactor), iteration - 1)
    } else {
      val iterationsPerformed = "iterations performed: " + (maxNumberOfIterations - iteration)
      val precision = "Result is precise to within " + "%20.17f".format(cube.len) + " mm"
      val valuesText = "Calculated values: " + psmInterpolator.trans.pix2Iso(maxPoint.pt)
      logger.info(s"$iterationsPerformed    $precision    $valuesText")
    }
  }

  /**
    * Find the maximum point via gradient descent.
    */
  private def findMax(): Unit = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(centerPt) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_mm, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished PSM gradient ascent.   Elapsed ms: $elapsed     maxPoint: $maxPoint")
  }

  // run algorithm and set max point on object construction
  findMax()

}
