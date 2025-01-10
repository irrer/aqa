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
  private val initialDepth: Int = 5

  /** Initial width of search field in mm. */
  private val initialWidth_mm: Double = 3.0

  /** For each successive approximation, reduce the search area by this factor. */
  private val reductionFactor: Double = 4.0

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

  case class WLTablePoint(dX: Double, dZ: Double, tableX: Double, tableZ: Double) {
    def minSquare(table: WLTable): Double = table.minSquareOfBBDisplacement(dX, dZ, tableX, tableZ)
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

    def finder(dXp: Dim, dZp: Dim, tableXp: Dim, tableZp: Dim, depth: Int): Seq[WLTablePoint] = {

      Trace.trace("Making pointArray")

      val pointArray: Seq[Seq[Seq[Seq[Double]]]] = {

        var count: Long = 0.toLong // TODO rm

        def make(dXi: Int): Seq[Seq[Seq[Double]]] = {
          for (dZi <- fullRange) yield {
            for (tableXi <- fullRange) yield {
              for (tableZi <- fullRange) yield {
                count = count + 1
                table.minSquareOfBBDisplacement(dXp.list(dXi), dZp.list(dZi), tableXp.list(tableXi), tableXp.list(tableZi))
              }
            }
          }
        }

        val pa = fullRange.par.map(make).toArray.toSeq
        Trace.trace(s"count: $count") // TODO rm

        pa
      }

      Trace.trace("Done making pointArray")

      /** Determine if the given point is a local minimum. */
      def isMin(dXi: Int, dZi: Int, tableXi: Int, tableZi: Int): Boolean = {
        val centerValue: Double = pointArray(dXi)(dZi)(tableXi)(tableZi)
        val smaller = offsetList.find(p =>
          pointArray //
          (p._1 + dXi) //
          (p._2 + dZi) //
          (p._3 + tableXi) //
          (p._4 + tableZi) //
            < centerValue
        )
        smaller.isEmpty
      }

      Trace.trace("Traversing pointArray")
      val minList =
        for ( //
          dXi <- subRange; //
          dZi <- subRange; //
          tableXi <- subRange; //
          tableZi <- subRange //
          if isMin(dXi, dZi, tableXi, tableZi)
        )
          yield //
          WLTablePoint(dXp.list(dXi), dZp.list(dZi), tableXp.list(tableXi), tableZp.list(tableZi))

      Trace.trace("Done traversing pointArray")

      minList // TODO rm
        .sortBy(_.minSquare(table))
        .foreach(p => { // TODO rm
          Trace.trace(s"dX: ${p.dX}    dZ: ${p.dZ}    tableX: ${p.tableX}    tableZ: ${p.tableZ} => ${p.minSquare(table)}   ")
        })

      if (depth > 0) {
        def doit(p: WLTablePoint): Seq[WLTablePoint] = {
          val width = dXp.len / reductionFactor
          finder(Dim(p.dX, width), Dim(p.dZ, width), Dim(p.tableX, width), Dim(p.tableZ, width), depth - 1)
        }
        minList.flatMap(doit)
      } else
        minList
    }

    val minList = finder(Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), Dim(0, initialWidth_mm), initialDepth)

    Trace.trace("Number of min: " + minList.size) // TODO rm

    minList // TODO rm
      .sortBy(_.minSquare(table))
      .foreach(p => { // TODO rm
        Trace.trace(s"dX: ${p.dX}    dZ: ${p.dZ}    tableX: ${p.tableX}    tableZ: ${p.tableZ} => ${p.minSquare(table)}   ")
      })

    Trace.trace()
    minList.head
  }
}
