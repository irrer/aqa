package org.aqa.webrun.wl.isoCheck

import org.aqa.Logging

import scala.annotation.tailrec

/**
  * Optimize the minimum of the maximum R-squared values
  */

class WLIsoTableGradientDescent(isoTable: WLIsoTable) extends Logging {

  /** Initial size (edge length) of hypercube in mm. */
  private val initialCubeLen_mm: Double = 0.5

  /** Stop after this many iterations, regardless of if the degree of precision is sufficient. */
  private val maxNumberOfIterations: Int = 100000

  /** For each iteration, multiply the cube length by this amount. */
  private val cubeReductionFactor: Double = 0.999

  /** Stop iterating if the cube length becomes this small, indicating that the result is sufficiently precise. */
  private val precision: Double = 1.0e-9

  /**
    * A point in the 4 dimensional hyperspace being searched.
    *
    * @param dX     dX coordinate
    * @param dZ     dZ coordinate
    * @param isoTableX isoTableX coordinate
    * @param isoTableZ isoTableZ coordinate
    * @return
    */
  private class WLIsoTablePointLocal(dX: Double, dZ: Double, isoTableX: Double, isoTableZ: Double) extends WLIsoTablePoint(dX, dZ, isoTableX, isoTableZ) {
    val minSquare: Double = isoTable.minSquareOfBBDisplacement(dX, dZ, isoTableX, isoTableZ)

    override def toString: String = {
      def fmt(d: Double): String = d.formatted("%21.18f")
      super.toString + s"    min R^2: ${fmt(minSquare)}"
    }
  }

  private var bestPoint: WLIsoTablePointLocal = new WLIsoTablePointLocal(100, 100, 100, 100)

  private def updateBestPoint(point: WLIsoTablePointLocal): Unit =
    bestPoint.synchronized {
      if (point.minSquare < bestPoint.minSquare)
        bestPoint = point
    }

  private case class WalkingCube(center: WLIsoTablePointLocal, len: Double, id: Int) {

    private val increment = len / 2

    // private val incrementList = (-hi to hi).map(_ * increment)
    private val incrementList = Seq(-increment, 0.0, increment)

    private var min: WLIsoTablePointLocal = new WLIsoTablePointLocal(0, 0, 0, 0)

    def getMin: WLIsoTablePointLocal = min

    incrementList.foreach(dXInc => { //
      val dX = center.dX + dXInc
      incrementList.foreach(dZInc => { //
        val dZ = center.dZ + dZInc
        incrementList.foreach(isoTableXInc => { //
          val isoTableX = center.isoTableX + isoTableXInc
          incrementList.foreach(isoTableZInc => { //
            val isoTableZ = center.isoTableZ + isoTableZInc
            val p = new WLIsoTablePointLocal(dX, dZ, isoTableX, isoTableZ)
            if (p.minSquare < min.minSquare) min = p
          })
        })
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

  def findMin(): WLIsoTablePoint = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(new WLIsoTablePointLocal(0, 0, 0, 0)) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_mm, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished gradient descent.   Elapsed ms: $elapsed     bestPoint: $bestPoint")

    if (false) { // TODO rm
      val excel = new WLIsoTablePointLocal(0.1839722351, 0.3329206495, 0.3320187925, 0.3238829870)
      excel
    } else
      bestPoint.asInstanceOf[WLIsoTablePoint] // TODO put back
  }
}
