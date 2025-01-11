package org.aqa.webrun.wl.wlMonthly

import edu.umro.ScalaUtil.Trace

import scala.annotation.tailrec

/**
  * Optimize the minimum of the maximum R-squared values
  */

class WLTableGradientDescent(table: WLTable) {

  private val initialCubeLen: Double = 0.5

  private val initialDepth: Int = 50

  private val degree: Int = 31

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
      super.toString + s" => ${fmt(minSquare)}"
    }
  }

  private var bestPoint: WLTablePointLocal = new WLTablePointLocal(100, 100, 100, 100)

  private def updateBestPoint(point: WLTablePointLocal): Unit =
    bestPoint.synchronized {
      if (point.minSquare < bestPoint.minSquare)
        bestPoint = point
    }

  private case class WalkingCube(center: WLTablePointLocal, len: Double, id: Int) {

    private val radius = len / 2

    private val increment = len / (degree - 1)

    private val hi: Int = (degree - 1) / 2

    private val incrementList = (-hi to hi).map(_ * increment)

    private var min: WLTablePointLocal = new WLTablePointLocal(10, 10, 10, 10)

    def near(a: Double, b: Double): Boolean = {
      (a-b).abs < 0.0001
    }

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

  private def makeInitialCubeCenterList: Seq[WLTablePointLocal] = {

    val cList: Seq[Double] = Seq(-1.0, 1.0)

    val list = //
      cList.flatMap(dX => //
        cList.flatMap(dZ => //
          cList.flatMap(tableX => //
            cList.map(tableZ => //
              new WLTablePointLocal(dX, dZ, tableX, tableZ)
            )
          )
        )
      )
    list
  }

  /**
    * Recursively walk a cube, searching for a better solution.
    * @param cube Search here
    * @param depth Current depth of recursion.
    */
  @tailrec
  private def finder(cube: WalkingCube, depth: Int): Unit = {
    if (depth > 0) {
      Trace.trace(s"""depth: ${depth.formatted("%3d")}    len: ${cube.len.formatted("%10.8f")}""")
      finder(cube.nextCube(cube.len * 0.9), depth - 1)
    }
  }

  def findMin(): WLTablePoint = {
    val start = System.currentTimeMillis()
    Trace.trace()
    val initialCubeCenterList =  Seq(new WLTablePointLocal(0, 0, 0, 0)) // makeInitialCubeCenterList

    Trace.trace()
    initialCubeCenterList.indices.par.foreach(index => finder(WalkingCube(initialCubeCenterList(index), initialCubeLen, index), initialDepth))

    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Elapsed ms: $elapsed     bestPoint: $bestPoint")

    bestPoint.asInstanceOf[WLTablePoint]
  }
}
