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

case class ColAngle(bankA: GapSkew, bankB: GapSkew) {

  /** True if using the top edge of bankA, otherwise, using the bottom edge. */
  private val aTop = bankA.topRightValue_mm.get.abs < bankA.bottomRightValue_mm.get.abs

  /** True if using the top edge of bankB, otherwise, using the bottom edge. */
  private val bTop = bankB.topRightValue_mm.get.abs < bankB.bottomRightValue_mm.get.abs

  private val aR = if (aTop) bankA.topRightValue_mm.get else bankA.bottomRightValue_mm.get
  private val aL = if (aTop) bankA.topLeftValue_mm.get else bankA.bottomLeftValue_mm.get

  private val bR = if (bTop) bankB.topRightValue_mm.get else bankB.bottomRightValue_mm.get
  private val bL = if (bTop) bankB.topLeftValue_mm.get else bankB.bottomLeftValue_mm.get

  /** Collimator angle, rounded to nearest 90 degrees. */
  private val col: Int = bankA.angleRounded

  val aSeparation: GosValue = {
    val v = bankA.plannedEdgeSeparation_mm
    new GosValue(
      v,
      name = " Horizontal Separation",
      description = s"Horizontal (x axis) distance between right and left measurements for Bank A col $col deg.  Used to calculate skew angle.",
      derivation = v
    )
  }

  val bSeparation: GosValue = {
    val v = bankB.plannedEdgeSeparation_mm
    new GosValue(
      v,
      " Horizontal Separation",
      s"Horizontal (x axis) distance between right and left measurements for Bank B col $col.  Used to calculate skew angle.",
      v
    )
  }

  val aRight: GosValue = new GosValue(aR, "Bank A Right", "Vertical measurement of Bank A (X2) MLC leaves on the right hand end for col $col deg.", aR)
  val aLeft: GosValue = new GosValue(aL, "Bank A Left", "Vertical measurement of Bank A (X2) MLC leaves on the left hand end for col $col deg.", aL)

  val bRight: GosValue = new GosValue(bR, "Bank B Right", "Vertical measurement of Bank B (X1) MLC leaves on the right hand end for col " + col + " deg.", bR)
  val bLeft: GosValue = new GosValue(bL, "Bank B Left", "Vertical measurement of Bank B (X1) MLC leaves on the left hand end for col " + col + " deg.", bL)

  val aRightLeftDiff: GosValue = new GosValue(
    v = aRight.v - aLeft.v,
    name = "Bank A Right - Left",
    description = s"""${aRight.name} - ${aLeft.name} showing the vertical change.""",
    derivation = s"""${aRight.v - aLeft.v} = ${aRight.v} - ${aLeft.v}""",
    references = Seq(aRight, aLeft)
  )

  val bRightLeftDiff: GosValue = new GosValue(
    v = bRight.v - bLeft.v,
    name = "Bank B Right - Left",
    description = s"${bRight.name} - ${bLeft.name} showing the vertical change.",
    derivation = s"""${bRight.v - bLeft.v} = ${bRight.v} - ${bLeft.v}""",
    references = Seq(bRight, bLeft)
  )

  val aSkew: GosValue = {
    val v = if (aTop) bankA.topHorzSkew_deg else bankA.bottomHorzSkew_deg
    GosValue(
      v,
      "Skew angle deg",
      s"atan((${aRightLeftDiff.name}) / ${aSeparation.name})",
      s"$v = atan((${aRightLeftDiff.v}) / ${aSeparation.v})",
      Seq(aRightLeftDiff, aSeparation),
      units = "deg"
    )
  }

  val bSkew: GosValue = {
    val v = if (bTop) bankB.topHorzSkew_deg else bankB.bottomHorzSkew_deg
    GosValue(
      v,
      "Skew angle deg",
      s"atan((${bRightLeftDiff.name}) / ${bSeparation.name})",
      s"$v = atan((${bRightLeftDiff.v}) / ${bSeparation.v})",
      Seq(bRightLeftDiff, bSeparation),
      units = "deg"
    )
  }

  val aAvg: GosValue = {
    val v = (aRight.v + aLeft.v) / 2
    new GosValue(
      v,
      "Bank A avg",
      s"(${aRight.name} + ${aLeft.name}) / 2 : Average of Bank A giving the vertical position of the midpoint.",
      s"$v = (${aRight.v} + ${aLeft.v}) / 2",
      Seq(aRight, aLeft)
    )
  }

  val bAvg: GosValue = {
    val v = (bRight.v + bLeft.v) / 2
    new GosValue(
      v,
      "Bank B avg",
      s"(${bRight.name} + ${bLeft.name}) / 2 : Average of Bank B giving the vertical position of the midpoint.",
      s"$v = (${bRight.v} + ${bLeft.v}) / 2",
      Seq(bRight, bLeft)
    )
  }

  // ----- differences -----

  val abRightDiff: GosValue = {
    val v = aRight.v - bRight.v
    new GosValue(
      v,
      s"${aRight.name} - ${bRight.name}",
      s"${bRight.name} - ${bRight.name} : Difference for each bank's right hand values.",
      s"$v = ${bRight.v} - ${bRight.v}",
      Seq(bRight, bRight)
    )
  }

  val abLeftDiff: GosValue = {
    val v = aLeft.v - bLeft.v
    new GosValue(
      v,
      s"${aLeft.name} - ${bLeft.name}",
      s"${bLeft.name} - ${bLeft.name} : Difference for each bank's left hand values.",
      s"$v = ${bLeft.v} - ${bLeft.v}",
      Seq(bLeft, bLeft)
    )
  }

  val abRightLeftDiff: GosValue = {
    val v = aRightLeftDiff.v - bRightLeftDiff.v
    new GosValue(
      v,
      s"${aRightLeftDiff.name} - ${bRightLeftDiff.name}",
      s"${aRightLeftDiff.name} - ${bRightLeftDiff.name} : Difference for each bank's right and left hand difference values.",
      s"$v = ${aRightLeftDiff.v} - ${bRightLeftDiff.v}",
      Seq(aRightLeftDiff, bRightLeftDiff)
    )
  }

  val abSkewDiff: GosValue = {
    val v = aSkew.v - bSkew.v
    new GosValue(
      v,
      s"${aSkew.name} - ${bSkew.name}",
      s"${aSkew.name} - ${bSkew.name} : Difference of skew between bank A and B.",
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
      s"${aAvg.name} - ${bAvg.name} : Difference of averages between bank A and B.",
      s"$v = ${aAvg.v} - ${bAvg.v}",
      Seq(aAvg, bAvg)
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

}
object GapOffsetSkew {
  def makeGapOffsetSkew(gapSkewList: Seq[GapSkew]): GapOffsetSkew = {

    def findMlc(angle: Int, mlcBank: Int): GapSkew = {
      gapSkewList.find(gs => (gs.angleRounded == angle) && gs.edgeList.exists(e => e.isMlc && e.bank == mlcBank)).get
    }

    // top X2 ABank
    val c090A = findMlc(90, 2)
    // bottom X1 BBank
    val c090B = findMlc(90, 1)

    // bottom X2 ABank
    val c270A = findMlc(270, 2)
    // bottom X1 BBank
    val c270B = findMlc(270, 1)

    val c270Jaw = gapSkewList.find(gs => (gs.angleRounded == 270) && gs.edgeList.filter(e => e.isJaw).size == 4).get

    val gos = GapOffsetSkew(
      c090A,
      c090B,
      c270A,
      c270B,
      c270Jaw
    )
    gos
  }
}
