package org.aqa.webrun.wl.wlMonthly

import org.aqa.Logging

import scala.annotation.tailrec

/**
  * Optimize the minimum of the maximum R-squared values
  */

class WLTableGradientDescent(table: WLTable) extends Logging {

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
    * @param tableX tableX coordinate
    * @param tableZ tableZ coordinate
    * @return
    */
  private class WLTablePointLocal(dX: Double, dZ: Double, tableX: Double, tableZ: Double) extends WLTablePoint(dX, dZ, tableX, tableZ) {
    val minSquare: Double = table.minSquareOfBBDisplacement(dX, dZ, tableX, tableZ)

    override def toString: String = {
      def fmt(d: Double): String = d.formatted("%19.16f")
      super.toString + s"    min R^2: ${fmt(minSquare)}"
    }
  }

  private var bestPoint: WLTablePointLocal = new WLTablePointLocal(100, 100, 100, 100)

  private def updateBestPoint(point: WLTablePointLocal): Unit =
    bestPoint.synchronized {
      if (point.minSquare < bestPoint.minSquare)
        bestPoint = point
    }

  private case class WalkingCube(center: WLTablePointLocal, len: Double, id: Int) {

    private val increment = len / 2

    // private val incrementList = (-hi to hi).map(_ * increment)
    private val incrementList = Seq(-increment, 0.0, increment)

    private var min: WLTablePointLocal = new WLTablePointLocal(10, 10, 10, 10)

    def getMin: WLTablePointLocal = min

    incrementList.foreach(dXInc => { //
      val dX = center.dX + dXInc
      incrementList.foreach(dZInc => { //
        val dZ = center.dZ + dZInc
        incrementList.foreach(tableXInc => { //
          val tableX = center.tableX + tableXInc
          incrementList.foreach(tableZInc => { //
            val tableZ = center.tableZ + tableZInc
            val p = new WLTablePointLocal(dX, dZ, tableX, tableZ)
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

  def findMin(): WLTablePoint = {
    val start = System.currentTimeMillis()
    val initialCubeCenterList = Seq(new WLTablePointLocal(0, 0, 0, 0)) // makeInitialCubeCenterList

    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen_mm, index), maxNumberOfIterations))

    val elapsed = System.currentTimeMillis() - start
    logger.info(s"Finished gradient descent.   Elapsed ms: $elapsed     bestPoint: $bestPoint")

    bestPoint.asInstanceOf[WLTablePoint]
  }
}
