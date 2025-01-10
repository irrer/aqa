package org.aqa.webrun.wl.wlMonthly

import edu.umro.ScalaUtil.Trace

/**
  * Optimize the minimum of the maximum R-squared values
  *
  * @param dX     dX
  * @param dZ     dZ
  * @param tableX tableX
  * @param tableZ tableZ
  * @return
  */

object WLTableGradientDescent {

  /** The number of increments in each dimension to explore.  The smaller this number is, the
    *  greater chance that a local minimum will be missed.  The larger it is, the greater the
    *  execution time.
    *
    * Compute time is a multiple of n**4, so 10  produces 10,000 calculations.
    */
  private var degree: Int = 32 // TODO change from var to val

  /** Initial width of search field in mm. */
  private val initialDepth: Int = 40

  /** Initial width of search field in mm. */
  private val initialWidth_mm: Double = 3.0

  /** For each successive approximation, reduce the search area by this factor. */
  private val reductionFactor: Double = 2.0

  /** For each successive approximation, reduce the search area by this factor. */
  private val maxBest: Int = 1

  /**
    * A segment of one dimension in hyperspace.
    * @param center The position of the center of the range.
    * @param len The length of the range.
    */
  private case class Dim(center: Double, len: Double) {
    // lower limit of coordinate space
    private val lo = center - (len / 2)

    // separation between adjacent points
    private val separation = len / degree

    private def coordinateToValue(coordinate: Int) = center + (separation * coordinate)

    /** Absolute position of coordinates in real space.  Provides translation of index to position. */
    val list: Seq[Double] = (0 until degree).map(coordinateToValue)
  }

  private class Best {

    val best: scala.collection.mutable.ArrayBuffer[WLTablePoint] = scala.collection.mutable.ArrayBuffer[WLTablePoint]()

    def put(point: WLTablePoint): Unit =
      best.synchronized {
        val j = best.lastOption
        if (best.isEmpty || (point.minSquare < best.last.minSquare)) {
          best.append(point)
          val newBest = best.sortBy(_.minSquare).take(maxBest)
          best.clear()
          best.appendAll(newBest)
        }
      }

    def goodEnough(minSq: Double): Boolean =
      best.synchronized {
        val j = best.lastOption
        val good = best.isEmpty || (minSq < best.last.minSquare)
        good
      }

    def entireList: Seq[WLTablePoint] = best.synchronized { best.toSeq }

    def getVeryBest: WLTablePoint = best.synchronized { best.head }

  }

  /* private case class SearchVolume( dX: Dim, dZ: Dim, tableX: Dim, tableZ: Dim ) { def minMax(): Double = { 0.0 // xODO } val dXMin = ??? } */

  /**
    * Optimize the minimum of the maximum R-squared values
    *
    * @param dXi     dX index
    * @param dZi     dZ index
    * @param tableXi tableX index
    * @param tableZi tableZ index
    * @return
    */

  private val fullRange: Seq[Int] = 0 until degree
  private val subRange: Seq[Int] = fullRange.tail.dropRight(1)

  case class WLTablePoint(dX: Double, dZ: Double, tableX: Double, tableZ: Double, table: WLTable) {
    val minSquare: Double = table.minSquareOfBBDisplacement(dX, dZ, tableX, tableZ)
    override def toString(): String = {

      def fmt(d: Double): String = d.formatted("%19.16f")

      s"dX: ${fmt(dX)}    dZ: ${fmt(dZ)}    tableX: ${fmt(tableX)}    tableZ: ${fmt(tableZ)} => ${fmt(minSquare)}"
    }
  }

  // list of offsets for coordinate adjacent to a central point
  private val offsetList: Seq[(Int, Int, Int, Int)] = {
    val plusMinus1 = Seq(-1, 0, 1)
    for ( //
      dx <- plusMinus1; //
      dz <- plusMinus1; //
      tableX <- plusMinus1; //
      tableZ <- plusMinus1 //
      if (dx, dz, tableX, tableZ) != (0, 0, 0, 0) // do not include center point
    ) yield (dx, dz, tableX, tableZ)
  }

  def findMin(table: WLTable): WLTablePoint = {

    val bestList = new Best

    def finder(dXp: Dim, dZp: Dim, tableXp: Dim, tableZp: Dim, depth: Int): Unit = {

      Trace.trace("Searching array")

      fullRange.par.foreach(dXi => {
        fullRange.foreach(dZi => {
          fullRange.foreach(tableXi => {
            fullRange.foreach(tableZi => {
              if (bestList.goodEnough(table.minSquareOfBBDisplacement(dXp.list(dXi), dZp.list(dZi), tableXp.list(tableXi), tableZp.list(tableZi)))) {
                bestList.put(WLTablePoint(dXp.list(dXi), dZp.list(dZi), tableXp.list(tableXi), tableXp.list(tableZi), table))
                // Trace.trace(s"put best: ${bestList.getVeryBest}")
              }
            })
          })
        })
      })

      if (depth > 0) {
        val width = dXp.len / reductionFactor
        Trace.trace(s"depth: $depth    width: $width    best: ${bestList.getVeryBest}")
        bestList.entireList.foreach(p =>
          finder( //
            Dim(p.dX, width), //
            Dim(p.dZ, width), //
            Dim(p.tableX, width), //
            Dim(p.tableZ, width), //
            depth - 1
          )
        )
      }
    }

    Trace.trace("Done searching")

    finder(Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), initialDepth)

    Trace.trace(s"very best: ${bestList.getVeryBest}")

    bestList.getVeryBest
  }
}
