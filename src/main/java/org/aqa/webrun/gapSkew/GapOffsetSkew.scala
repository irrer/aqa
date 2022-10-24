package org.aqa.webrun.gapSkew

import org.aqa.db.GapSkew

/**
  * Provide a generalized way to represent a gap-offset-skew value.
  * @param v The actual value
  * @param name Commonly used name.
  * @param description Detailed description of the value.
  * @param derivation Equation showing the math of how the value was derived.
  * @param references List of values (if any) from which this value is derived or
  *                   otherwise related.  Default is no references.
  * @param units Unit of measurement.  Default is mm.
  */
case class GosValue(v: Double, name: String, description: String, derivation: String, references: Seq[GosValue], units: String) {

  def this(v: Double, name: String, description: String, derivation: Double) { this(v, name, description, derivation.toString, Seq(), "mm") }

  def this(v: Double, name: String, description: String, derivation: Double, units: String) {
    this(v, name, description, derivation.toString, Seq(), units)
  }

  def this(v: Double, name: String, description: String, derivation: Double, references: Seq[GosValue]) {
    this(v, name, description, derivation.toString, references, "mm")
  }

  def this(v: Double, name: String, description: String, derivation: Double, references: Seq[GosValue], units: String) {
    this(v, name, description, derivation.toString, references, units)
  }

  def this(v: Double, name: String, description: String, derivation: String) {
    this(v, name, description, derivation, Seq(), "mm")
  }

  def this(v: Double, name: String, description: String, derivation: String, units: String) {
    this(v, name, description, derivation, Seq(), units)
  }

  def this(v: Double, name: String, description: String, derivation: String, references: Seq[GosValue]) {
    this(v, name, description, derivation, references, "mm")
  }

}

/**
  * Provide values and descriptions of values related to the Jaw-Jaw image and comparisons to bank A at C270.
  * @param bankA Values measured and calculated for MLC bank A.
  * @param bankB Values measured and calculated for MLC bank B.
  */

case class ColAngle(bankA: GapSkew, bankB: GapSkew) {

  /** True if using the top edge of bankA, otherwise, using the bottom edge. */
  private val aTop = bankA.topRightValue_mm.get.abs < bankA.bottomRightValue_mm.get.abs

  /** True if using the top edge of bankB, otherwise, using the bottom edge. */
  private val bTop = bankB.topRightValue_mm.get.abs < bankB.bottomRightValue_mm.get.abs

  private val aR = if (aTop) bankA.topRightValue_mm.get else bankA.bottomRightValue_mm.get
  private val aL = if (aTop) bankA.topLeftValue_mm.get else bankA.bottomLeftValue_mm.get

  private val bR = if (bTop) bankB.topRightValue_mm.get else bankB.bottomRightValue_mm.get
  private val bL = if (bTop) bankB.topLeftValue_mm.get else bankB.bottomLeftValue_mm.get

  val separation: GosValue = {
    val v = bankA.plannedEdgeSeparation_mm
    new GosValue(
      v,
      name = "Horizontal Separation",
      description = s"Horizontal (x axis) distance between the centers of the right and left measurements.  Used to calculate skew angle.",
      derivation = v
    )
  }

  // ----- Bank A -----

  val aRight: GosValue = new GosValue(aR, "Bank A Right", s"Vertical measurement of Bank A (X2) MLC leaves on the right-hand end.", aR)

  val aLeft: GosValue = new GosValue(aL, "Bank A Left", s"Vertical measurement of Bank A (X2) MLC leaves on the left-hand end.", aL)

  val aRightLeftDiff: GosValue = new GosValue(
    v = aRight.v - aLeft.v,
    name = "Bank A Right - Left",
    description = s"""${aRight.name} - ${aLeft.name} showing the vertical change""",
    derivation = s"""${aRight.v - aLeft.v} = ${aRight.v} - ${aLeft.v}""",
    references = Seq(aRight, aLeft)
  )

  val aSkew: GosValue = {
    val v = if (aTop) bankA.topHorzSkew_deg else bankA.bottomHorzSkew_deg
    GosValue(
      v,
      "Bank A Skew angle",
      s"atan((${aRightLeftDiff.name}) / ${separation.name}",
      s"$v = atan((${aRightLeftDiff.v}) / ${separation.v})",
      Seq(aRightLeftDiff, separation),
      units = "deg"
    )
  }

  val aAvg: GosValue = {
    val v = (aRight.v + aLeft.v) / 2
    new GosValue(
      v,
      "Bank A avg",
      s"(${aRight.name} + ${aLeft.name}) / 2 : Average of Bank A giving the vertical position of the midpoint",
      s"$v = (${aRight.v} + ${aLeft.v}) / 2",
      Seq(aRight, aLeft)
    )
  }

  // ----- Bank B -----

  val bRight: GosValue = new GosValue(bR, "Bank B Right", s"Vertical measurement of Bank B (X1) MLC leaves on the right-hand end for", bR)

  val bLeft: GosValue = new GosValue(bL, "Bank B Left", s"Vertical measurement of Bank B (X1) MLC leaves on the left-hand end for", bL)

  val bRightLeftDiff: GosValue = new GosValue(
    v = bRight.v - bLeft.v,
    name = "Bank B Right - Left",
    description = s"${bRight.name} - ${bLeft.name} showing the vertical change",
    derivation = s"""${bRight.v - bLeft.v} = ${bRight.v} - ${bLeft.v}""",
    references = Seq(bRight, bLeft)
  )

  val bSkew: GosValue = {
    val v = if (bTop) bankB.topHorzSkew_deg else bankB.bottomHorzSkew_deg
    GosValue(
      v,
      "Bank B Skew angle",
      s"atan((${bRightLeftDiff.name}) / ${separation.name}",
      s"$v = atan((${bRightLeftDiff.v}) / ${separation.v})",
      Seq(bRightLeftDiff, separation),
      units = "deg"
    )
  }

  val bAvg: GosValue = {
    val v = (bRight.v + bLeft.v) / 2
    new GosValue(
      v,
      "Bank B avg",
      s"(${bRight.name} + ${bLeft.name}) / 2 : Average of Bank B giving the vertical position of the midpoint",
      s"$v = (${bRight.v} + ${bLeft.v}) / 2",
      Seq(bRight, bLeft)
    )
  }

  // ----- A-B differences -----

  val abRightDiff: GosValue = {
    val v = aRight.v - bRight.v
    new GosValue(
      v,
      s"${aRight.name} - ${bRight.name}",
      s"${aRight.name} - ${bRight.name} : Difference for each bank's right-hand values",
      s"$v = ${aRight.v} - ${bRight.v}",
      Seq(aRight, bRight)
    )
  }

  val abLeftDiff: GosValue = {
    val v = aLeft.v - bLeft.v
    new GosValue(
      v,
      s"${aLeft.name} - ${bLeft.name}",
      s"${aLeft.name} - ${bLeft.name} : Difference for each bank's left-hand values",
      s"$v = ${aLeft.v} - ${bLeft.v}",
      Seq(aLeft, bLeft)
    )
  }

  val abRightLeftDiff: GosValue = {
    val v = aRightLeftDiff.v - bRightLeftDiff.v
    new GosValue(
      v,
      s"${aRightLeftDiff.name} - ${bRightLeftDiff.name}",
      s"${aRightLeftDiff.name} - ${bRightLeftDiff.name} : Difference for each bank's right and left-hand difference values",
      s"$v = ${aRightLeftDiff.v} - ${bRightLeftDiff.v}",
      Seq(aRightLeftDiff, bRightLeftDiff)
    )
  }

  val abSkewDiff: GosValue = {
    val v = aSkew.v - bSkew.v
    GosValue(
      v,
      s"${aSkew.name} - ${bSkew.name}",
      s"${aSkew.name} - ${bSkew.name} : Difference of skew between bank A and B",
      s"$v = ${aSkew.v} - ${bSkew.v}",
      Seq(aSkew, bSkew),
      units = "deg"
    )
  }

  val abAvgDiff: GosValue = {
    val v = aAvg.v - bAvg.v
    new GosValue(
      v,
      s"${aAvg.name} - ${bAvg.name}",
      s"${aAvg.name} - ${bAvg.name} : Difference of averages between bank A and B",
      s"$v = ${aAvg.v} - ${bAvg.v}",
      Seq(aAvg, bAvg)
    )
  }

  // ----- (A+B)/2 averages -----

  val abRightAvg: GosValue = {
    val v = (aRight.v + bRight.v) / 2
    new GosValue(
      v,
      s"(${aRight.name} + ${bRight.name}) / 2",
      s"(${aRight.name} + ${bRight.name}) / 2 : Average of each bank's right-hand values",
      s"$v = (${aRight.v} + ${bRight.v}) / 2",
      Seq(aRight, bRight)
    )
  }

  val abLeftAvg: GosValue = {
    val v = (aLeft.v + bLeft.v) / 2
    new GosValue(
      v,
      s"(${aLeft.name} + ${bLeft.name}) / 2",
      s"(${aLeft.name} + ${bLeft.name}) / 2 : Average of each bank's left-hand values",
      s"$v = (${aLeft.v} + ${bLeft.v}) / 2",
      Seq(aLeft, bLeft)
    )
  }

  val abRightLeftAvg: GosValue = {
    val v = (aRightLeftDiff.v + bRightLeftDiff.v) / 2
    new GosValue(
      v,
      s"(${aRightLeftDiff.name} + ${bRightLeftDiff.name}) / 2",
      s"(${aRightLeftDiff.name} + ${bRightLeftDiff.name}) / 2 : Average of each bank's right and left-hand differences",
      s"$v = (${aRightLeftDiff.v} + ${bRightLeftDiff.v}) / 2",
      Seq(aRightLeftDiff, bRightLeftDiff)
    )
  }

  val abSkewAvg: GosValue = {
    val v = (aSkew.v + bSkew.v) / 2
    GosValue(
      v,
      s"(${aSkew.name} + ${bSkew.name}) / 2",
      s"(${aSkew.name} + ${bSkew.name}) / 2 : Average of skews for bank A and B",
      s"$v = (${aSkew.v} + ${bSkew.v}) / 2",
      Seq(aSkew, bSkew),
      units = "deg"
    )
  }

  val abAvgAvg: GosValue = {
    val v = (aAvg.v + bAvg.v) / 2
    new GosValue(
      v,
      s"(${aAvg.name} + ${bAvg.name}) / 2",
      s"(${aAvg.name} + ${bAvg.name}) / 2 : Average of averages for bank A and B",
      s"$v = (${aAvg.v} + ${bAvg.v}) / 2",
      Seq(aAvg, bAvg)
    )
  }

}

/**
  * Provide values and descriptions of values related to the Jaw-Jaw image and comparisons to bank A at C270.
  * @param jawPair Values measured and calculated for jaw.
  * @param colAngle Values measured and calculated for MLC.
  */
case class JawJaw(jawPair: GapSkew, colAngle: ColAngle) {

  private val jawIsTop: Boolean = jawPair.topLeftEdgeType.isJaw

  private val c = colAngle // shorthand

  // ----- Jaw (the one near the vertical center of the image) -----

  val jawRight: GosValue = {
    // find the one closest to zero.  Should be the top one unless they change the RTPLAN.
    val v = Seq(jawPair.topRightValue_mm.get, jawPair.bottomRightValue_mm.get).minBy(_.abs)

    new GosValue(
      v = v,
      s"Jaw Right",
      s"Measurement taken at right of edge of jaw near vertical center of image.",
      v
    )
  }

  val jawLeft: GosValue = {
    // find the one closest to zero.  Should be the top one unless they change the RTPLAN.
    val v = Seq(jawPair.topLeftValue_mm.get, jawPair.bottomLeftValue_mm.get).minBy(_.abs)
    new GosValue(
      v = v,
      s"Jaw Left",
      s"Measurement taken at left of edge of jaw near vertical center of image.",
      v
    )
  }

  val jawRightLeftDiff: GosValue = {
    val v = jawRight.v - jawLeft.v
    new GosValue(
      v = v,
      s"${jawRight.name} - ${jawLeft.name}",
      s"Jaw difference between ${jawRight.name} - ${jawLeft.name}.",
      s"$v = ${jawRight.v} - ${jawLeft.v}"
    )
  }

  val jawSkew: GosValue = {
    val v = if (jawIsTop) jawPair.topHorzSkew_deg else jawPair.bottomHorzSkew_deg
    GosValue(
      v,
      "Jaw Skew angle",
      s"atan((${jawRightLeftDiff.name}) / ${c.separation.name}",
      s"$v = atan((${jawRightLeftDiff.v}) / ${c.separation.v})",
      Seq(jawRightLeftDiff, c.separation),
      units = "deg"
    )
  }

  val jawAvg: GosValue = {
    val v = (jawRight.v + jawLeft.v) / 2
    new GosValue(
      v = v,
      s"Jaw Avg",
      s"Jaw average (midpoint) between ${jawRight.name} and ${jawLeft.name}.",
      s"$v = (${jawRight.v} + ${jawLeft.v}) / 2"
    )
  }

  // ----- Differences between Bank A and Jaw -----

  val aJawRightDiff: GosValue = {
    val v = c.aRight.v - jawRight.v
    new GosValue(
      v,
      s"${c.aRight.name} - ${jawRight.name}",
      s"${c.aRight.name} - ${jawRight.name} : Difference between bank A and jaw right-hand values",
      s"$v = ${c.aRight.v} - ${jawRight.v}",
      Seq(c.aRight, jawRight)
    )
  }

  val aJawLeftDiff: GosValue = {
    val v = c.aLeft.v - jawLeft.v
    new GosValue(
      v,
      s"${c.aLeft.name} - ${jawLeft.name}",
      s"${c.aLeft.name} - ${jawLeft.name} : Difference between bank A and left-hand values",
      s"$v = ${c.aLeft.v} - ${jawLeft.v}",
      Seq(c.aLeft, jawLeft)
    )
  }

  val aJawRightLeftDiff: GosValue = {
    val v = c.aRightLeftDiff.v - jawRightLeftDiff.v
    new GosValue(
      v,
      s"${c.aRightLeftDiff.name} - ${jawRightLeftDiff.name}",
      s"${c.aRightLeftDiff.name} - ${jawRightLeftDiff.name} : Difference between bank A and jaw right-hand differences",
      s"$v = ${c.aRightLeftDiff.v} - ${jawRightLeftDiff.v}",
      Seq(c.aRightLeftDiff, jawRightLeftDiff)
    )
  }

  val aJawSkewDiff: GosValue = {
    val v = c.aSkew.v - jawSkew.v
    GosValue(
      v,
      s"${c.aSkew.name} - ${jawSkew.name}",
      s"${c.aSkew.name} - ${jawSkew.name} : Difference of skew between bank A and Jaw",
      s"$v = ${c.aSkew.v} - ${jawSkew.v}",
      Seq(c.aSkew, jawSkew),
      units = "deg"
    )
  }

  val aJawAvgDiff: GosValue = {
    val v = c.aAvg.v - jawAvg.v
    new GosValue(
      v,
      s"${c.aAvg.name} - ${jawAvg.name}",
      s"${c.aAvg.name} - ${jawAvg.name} : Difference of averages between bank A and Jaw",
      s"$v = ${c.aAvg.v} - ${jawAvg.v}",
      Seq(c.aAvg, jawAvg)
    )
  }

  // ----- averages -----

  val aJawRightAvg: GosValue = {
    val v = (c.aRight.v + jawRight.v) / 2
    new GosValue(
      v,
      s"(${c.aRight.name} + ${jawRight.name}) / 2",
      s"(${c.aRight.name} + ${jawRight.name}) / 2 : Average of bank A and jaw right-hand values",
      s"$v = (${c.aRight.v} + ${jawRight.v}) / 2",
      Seq(c.aRight, jawRight)
    )
  }

  val aJawLeftAvg: GosValue = {
    val v = (c.aLeft.v + jawLeft.v) / 2
    new GosValue(
      v,
      s"(${c.aLeft.name} + ${jawLeft.name}) / 2",
      s"(${c.aLeft.name} + ${jawLeft.name}) / 2 : Average of bank A and jaw left-hand values",
      s"$v = (${c.aLeft.v} + ${jawLeft.v}) / 2",
      Seq(c.aLeft, jawLeft)
    )
  }

  val aJawRightLeftAvg: GosValue = {
    val v = (c.aRightLeftDiff.v + jawRightLeftDiff.v) / 2
    new GosValue(
      v,
      s"(${c.aRightLeftDiff.name} + ${jawRightLeftDiff.name}) / 2",
      s"(${c.aRightLeftDiff.name} + ${jawRightLeftDiff.name}) / 2 : Average of bank A and jaw right and left-hand difference values",
      s"$v = (${c.aRightLeftDiff.v} + ${jawRightLeftDiff.v}) / 2",
      Seq(c.aRightLeftDiff, jawRightLeftDiff)
    )
  }

  val aJawSkewAvg: GosValue = {
    val v = (c.aSkew.v + jawSkew.v) / 2
    GosValue(
      v,
      s"(${c.aSkew.name} + ${jawSkew.name}) / 2",
      s"(${c.aSkew.name} + ${jawSkew.name}) / 2 : Average of skews for bank A and Jaw",
      s"$v = (${c.aSkew.v} + ${jawSkew.v}) / 2",
      Seq(c.aSkew, jawSkew),
      units = "deg"
    )
  }

  val aJawAvgAvg: GosValue = {
    val v = (c.aAvg.v + jawAvg.v) / 2
    new GosValue(
      v,
      s"(${c.aAvg.name} + ${jawAvg.name}) / 2",
      s"(${c.aAvg.name} + ${jawAvg.name}) / 2 : Average of averages for bank A and Jaw",
      s"$v = (${c.aAvg.v} + ${jawAvg.v}) / 2",
      Seq(c.aAvg, jawAvg)
    )
  }

}

case class GapOffsetSkew(
    c090A: GapSkew,
    c090B: GapSkew,
    c270A: GapSkew,
    c270B: GapSkew,
    c270Jaw: GapSkew
) {

  val col090: ColAngle = ColAngle(c090A, c090B)
  val col270: ColAngle = ColAngle(c270A, c270B)
  val jawJaw: JawJaw = JawJaw(c270Jaw, col270)

}

object GapOffsetSkew {
  def makeGapOffsetSkew(gapSkewList: Seq[GapSkew]): Either[String, GapOffsetSkew] = {

    def findMlc(angle: Int, mlcBank: Int, name: String): Either[String, GapSkew] = {
      gapSkewList.find(gs => (gs.angleRounded == angle) && gs.edgeList.exists(e => e.isMlc && e.bank == mlcBank)) match {
        case Some(mlc) => Right(mlc)
        case _         => Left(name)
      }
    }

    // top X2 ABank
    val c090A = findMlc(90, 2, "Col 90 Bank A / X2")
    // bottom X1 BBank
    val c090B = findMlc(90, 1, "Col 90 Bank B / X1")

    // bottom X2 ABank
    val c270A = findMlc(270, 2, "Col 270 Bank A / X2")
    // bottom X1 BBank
    val c270B = findMlc(270, 1, "Col 270 Bank B / X1")

    val c270Jaw = {
      gapSkewList.find(gs => (gs.angleRounded == 270) && gs.edgeList.count(e => e.isJaw) == 4) match {
        case Some(jaw) => Right(jaw)
        case _         => Left("Col 270 with Jaw 1 and Jaw 2")
      }
    }
    val list = Seq(c090A, c090B, c270A, c270B, c270Jaw)

    val fail = list.filter(_.isLeft)

    if (fail.isEmpty) {
      val gos = GapOffsetSkew(
        c090A.right.get,
        c090B.right.get,
        c270A.right.get,
        c270B.right.get,
        c270Jaw.right.get
      )
      Right(gos)
    } else {
      Left(fail.map(_.left.get).mkString("\n"))
    }
  }
}
