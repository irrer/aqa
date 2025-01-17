package org.aqa.webrun.wl.wlMonthly

import org.aqa.Logging

import scala.annotation.tailrec

/**
  * Optimize the minimum of the maximum R-squared values
  */

class WLCollimatorGradientDescent(collimator: WLCollimator) extends Logging {

  /** Initial size (edge length) of hypercube in mm. */
  private val initialCubeLen_mm: Double = 0.5

  /** Stop after this many iterations, regardless of if the degree of precision is sufficient. */
  private val maxNumberOfIterations: Int = 100000

  /** For each iteration, multiply the cube length by this amount. */
  private val cubeReductionFactor: Double = 0.999

  /** Stop iterating if the cube length becomes this small, indicating that the result is sufficiently precise. */
  private val precision: Double = 1.0e-9

  /**
    * A point in the 2-dimensional space being searched.
    *
    * @param Coll_X     Coll-X coordinate
    * @param Coll_Z     Coll-Z coordinate
    * @return
    */
  private class WLCollimatorPointLocal(Coll_X: Double, Coll_Z: Double) extends WLCollimatorPoint(Coll_X, Coll_Z) {
    val minSquare: Double = collimator.MinCA_Rpp(Coll_X, Coll_Z)

    override def toString: String = {
      def fmt(d: Double): String = d.formatted("%21.18f")
      super.toString + s"    min R: ${fmt(minSquare)}"
    }
  }

  private var bestPoint: WLCollimatorPointLocal = new WLCollimatorPointLocal(-1, -1)

  private def updateBestPoint(point: WLCollimatorPointLocal): Unit =
    bestPoint.synchronized {
      if (point.minSquare < bestPoint.minSquare)
        bestPoint = point
    }

  private case class WalkingCube(center: WLCollimatorPointLocal, len: Double, id: Int) {

    private val increment = len / 2

    // private val incrementList = (-hi to hi).map(_ * increment)
    private val incrementList = Seq(-increment, 0.0, increment)

    private var min: WLCollimatorPointLocal = new WLCollimatorPointLocal(10, 10)

    def getMin: WLCollimatorPointLocal = min

    incrementList.foreach(Coll_X_Inc => { //
      val Coll_X = center.Coll_X + Coll_X_Inc
      incrementList.foreach(Coll_Z_Inc => { //
        val Coll_Z = center.Coll_Z + Coll_Z_Inc
        val p = new WLCollimatorPointLocal(Coll_X, Coll_Z)
        if (p.minSquare < min.minSquare) min = p
      })
    })

    updateBestPoint(min)

    def nextCube(nextLen: Double): WalkingCube = WalkingCube(min, nextLen, id)
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
      val precision = "Result is precise to within " + cube.len.formatted("%20.17f") + " mm"
      val valuesText = "Calculated values: " + cube.getMin.toString
      logger.info(s"$iterationsPerformed    $precision    $valuesText")
    }
  }

  def findMin(): WLCollimatorPoint = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(new WLCollimatorPointLocal(0, 0)) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_mm, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished gradient descent.   Elapsed ms: $elapsed     bestPoint: $bestPoint")

    bestPoint.asInstanceOf[WLCollimatorPoint]
  }
}
