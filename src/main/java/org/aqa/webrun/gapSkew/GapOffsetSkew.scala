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
case class GosValue(v: Double, name: String, group: String, description: String, derivation: String, references: Seq[GosValue], units: String) {

  def this(v: Double, name: String, group: String, description: String, derivation: Double) { this(v, name, group, description, derivation.toString, Seq(), "mm") }

  def this(v: Double, name: String, group: String, description: String, derivation: Double, units: String) {
    this(v, name, group, description, derivation.toString, Seq(), units)
  }

  def this(v: Double, name: String, group: String, description: String, derivation: Double, references: Seq[GosValue]) {
    this(v, name, group, description, derivation.toString, references, "mm")
  }

  def this(v: Double, name: String, group: String, description: String, derivation: Double, references: Seq[GosValue], units: String) {
    this(v, name, group, description, derivation.toString, references, units)
  }

  def this(v: Double, name: String, group: String, description: String, derivation: String) {
    this(v, name, group, description, derivation, Seq(), "mm")
  }

  def this(v: Double, name: String, group: String, description: String, derivation: String, units: String) {
    this(v, name, group, description, derivation, Seq(), units)
  }

  def this(v: Double, name: String, group: String, description: String, derivation: String, references: Seq[GosValue]) {
    this(v, name, group, description, derivation, references, "mm")
  }

  val fullName: String = (group + " " + name).trim

}

/**
  * Provide values and descriptions of values related to the Jaw-Jaw image and comparisons to bank A at C270.
  * @param bankA Values measured and calculated for MLC bank A.
  * @param bankB Values measured and calculated for MLC bank B.
  */

case class ColAngle(bankA: GapSkew, bankB: GapSkew) {

  /** True if using the top edge of bankA, otherwise, using the bottom edge. */
  private val aTop = {
    (bankA.topRightValue_mm, bankA.bottomRightValue_mm) match {
      case (Some(t), Some(b)) => t.abs < b.abs
      case (Some(_), _)       => true
      case (_, Some(_))       => false
      case _                  => true
    }
  }

  /** True if using the top edge of bankB, otherwise, using the bottom edge. */
  private val bTop = {
    (bankB.topRightValue_mm, bankB.bottomRightValue_mm) match {
      case (Some(t), Some(b)) => t.abs < b.abs
      case (Some(_), _)       => true
      case (_, Some(_))       => false
      case _                  => true
    }
  }

  private val aR = if (aTop) bankA.topRightValue_mm.get else bankA.bottomRightValue_mm.get
  private val aL = if (aTop) bankA.topLeftValue_mm.get else bankA.bottomLeftValue_mm.get

  private val bR = if (bTop) bankB.topRightValue_mm.get else bankB.bottomRightValue_mm.get
  private val bL = if (bTop) bankB.topLeftValue_mm.get else bankB.bottomLeftValue_mm.get

  private val group = "C" + bankA.angleRounded

  val separation: GosValue = {
    val v = bankA.plannedEdgeSeparation_mm
    new GosValue(
      v.get,
      name = "Horizontal Separation",
      group,
      description = s"Horizontal (x axis) distance between the centers of the right and left measurements.  Used to calculate skew angle.",
      derivation = v.get
    )
  }

  // ----- Bank A -----

  // @formatter:off
  val aRight: GosValue = new GosValue(aR,
     name = "Bank A Right", 
     group, 
     description = s"Vertical measurement of Bank A (X2) MLC leaves on the right-hand end.", 
     derivation = aR)
  // @formatter:on

  // @formatter:off
  val aLeft: GosValue = new GosValue(aL,
    name = "Bank A Left",
    group, 
    description = s"Vertical measurement of Bank A (X2) MLC leaves on the left-hand end.", 
    derivation = aL)
  // @formatter:on

  val aRightLeftDiff: GosValue = new GosValue(
    v = aRight.v - aLeft.v,
    name = "Bank A Right - Left",
    group,
    description = s"""${aRight.name} - ${aLeft.name} showing the vertical change""",
    derivation = s"""${aRight.v - aLeft.v} = ${aRight.v} - ${aLeft.v}""",
    references = Seq(aRight, aLeft)
  )

  val aSkew_deg: GosValue = {
    val v = if (aTop) bankA.topHorzSkew_deg else bankA.bottomHorzSkew_deg
    GosValue(
      v.get,
      name = "Bank A Skew angle",
      group,
      description = s"atan(${aRightLeftDiff.name} / ${separation.name})",
      derivation = s"$v = atan(${aRightLeftDiff.v} / ${separation.v})",
      Seq(aRightLeftDiff, separation),
      units = "deg"
    )
  }

  val aSkew_mmPer40cm: GosValue = {
    val v = if (aTop) bankA.topHorzSkew_mmPer40cm else bankA.bottomHorzSkew_mmPer40cm
    GosValue(
      v.get,
      name = "Bank A Skew",
      group,
      description = s"(${aRightLeftDiff.name} / ${separation.name}) * 40",
      derivation = s"$v = (${aRightLeftDiff.v} / ${separation.v}) * 40",
      Seq(aRightLeftDiff, separation),
      units = "mm/40cm"
    )
  }

  val aRightLeftAvg: GosValue = {
    val v = (aRight.v + aLeft.v) / 2
    new GosValue(
      v,
      name = "Bank A avg",
      group,
      description = s"(${aRight.name} + ${aLeft.name}) / 2 : Average of Bank A giving the vertical position of the midpoint",
      derivation = s"$v = (${aRight.v} + ${aLeft.v}) / 2",
      Seq(aRight, aLeft)
    )
  }

  // ----- Bank B -----

  // @formatter:off
  val bRight: GosValue = new GosValue(
    bR,
    name = "Bank B Right",
    group,
    description =  s"Vertical measurement of Bank B (X1) MLC leaves on the right-hand end for",
    bR)
  // @formatter:on

  // @formatter:off
  val bLeft: GosValue = new GosValue(
    bL,
    name = "Bank B Left",
    group,
    description =  s"Vertical measurement of Bank B (X1) MLC leaves on the left-hand end for",
    bL)
  // @formatter:on

  val bRightLeftDiff: GosValue = new GosValue(
    v = bRight.v - bLeft.v,
    name = "Bank B Right - Left",
    group,
    description = s"${bRight.name} - ${bLeft.name} showing the vertical change",
    derivation = s"""${bRight.v - bLeft.v} = ${bRight.v} - ${bLeft.v}""",
    references = Seq(bRight, bLeft)
  )

  val bSkew_deg: GosValue = {
    val v = if (bTop) bankB.topHorzSkew_deg else bankB.bottomHorzSkew_deg
    GosValue(
      v.get,
      name = "Bank B Skew angle",
      group,
      description = s"atan(${bRightLeftDiff.name} / ${separation.name})",
      derivation = s"$v = atan(${bRightLeftDiff.v} / ${separation.v})",
      Seq(bRightLeftDiff, separation),
      units = "deg"
    )
  }

  val bSkew_mmPer40cm: GosValue = {
    val v = if (bTop) bankB.topHorzSkew_mmPer40cm else bankB.bottomHorzSkew_mmPer40cm
    GosValue(
      v.get,
      name = "Bank B Skew",
      group,
      description = s"(${bRightLeftDiff.name} / ${separation.name}) * 40",
      derivation = s"$v = (${bRightLeftDiff.v} / ${separation.v}) * 40",
      Seq(bRightLeftDiff, separation),
      units = "mm/40cm"
    )
  }

  val bRightLeftAvg: GosValue = {
    val v = (bRight.v + bLeft.v) / 2
    new GosValue(
      v,
      name = "Bank B avg",
      group,
      description = s"(${bRight.name} + ${bLeft.name}) / 2 : Average of Bank B giving the vertical position of the midpoint",
      derivation = s"$v = (${bRight.v} + ${bLeft.v}) / 2",
      Seq(bRight, bLeft)
    )
  }

  // ----- A-B differences -----

  val rightDiff: GosValue = {
    val v = aRight.v - bRight.v
    new GosValue(
      v,
      name = "Right Diff",
      group,
      description = s"${aRight.name} - ${bRight.name} : Difference for each bank's right-hand values",
      derivation = s"$v = ${aRight.v} - ${bRight.v}",
      Seq(aRight, bRight)
    )
  }

  val leftDiff: GosValue = {
    val v = aLeft.v - bLeft.v
    new GosValue(
      v,
      name = "Left Diff",
      group,
      description = s"${aLeft.name} - ${bLeft.name} : Difference for each bank's left-hand values",
      derivation = "$v = ${aLeft.v} - ${bLeft.v}",
      Seq(aLeft, bLeft)
    )
  }

  val rightLeftDiff: GosValue = {
    val v = aRightLeftDiff.v - bRightLeftDiff.v
    new GosValue(
      v,
      name = "Diff of Diffs",
      group,
      description = s"${aRightLeftDiff.name} - ${bRightLeftDiff.name} : Difference for each bank's right and left-hand difference values",
      derivation = s"$v = ${aRightLeftDiff.v} - ${bRightLeftDiff.v}",
      Seq(aRightLeftDiff, bRightLeftDiff)
    )
  }

  val skewDiff_deg: GosValue = {
    val v = aSkew_deg.v - bSkew_deg.v
    GosValue(
      v,
      name = "Skew Diff",
      group,
      description = s"${aSkew_deg.name} - ${bSkew_deg.name} : Difference of skew between bank A and B",
      derivation = s"$v = ${aSkew_deg.v} - ${bSkew_deg.v}",
      Seq(aSkew_deg, bSkew_deg),
      units = "deg"
    )
  }

  val skewDiff_mmPer40cm: GosValue = {
    val v = aSkew_mmPer40cm.v - bSkew_mmPer40cm.v
    GosValue(
      v,
      name = s"Skew Diff",
      group,
      description = s"${aSkew_mmPer40cm.name} - ${bSkew_mmPer40cm.name} : Difference of skew between bank A and B",
      derivation = s"$v = ${aSkew_mmPer40cm.v} - ${bSkew_mmPer40cm.v}",
      Seq(aSkew_mmPer40cm, bSkew_mmPer40cm),
      units = "mm/40cm"
    )
  }

  val gap: GosValue = {
    val v = aRightLeftAvg.v - bRightLeftAvg.v
    new GosValue(
      v,
      name = s"Gap",
      group,
      description = s"${aRightLeftAvg.name} - ${bRightLeftAvg.name} : Difference of averages between bank A and B",
      derivation = s"$v = ${aRightLeftAvg.v} - ${bRightLeftAvg.v}",
      Seq(aRightLeftAvg, bRightLeftAvg)
    )
  }

  // ----- (A+B)/2 averages -----

  val rightAvg: GosValue = {
    val v = (aRight.v + bRight.v) / 2
    new GosValue(
      v,
      name = s"Right Avg",
      group,
      description = s"(${aRight.name} + ${bRight.name}) / 2 : Average of each bank's right-hand values",
      derivation = s"$v = (${aRight.v} + ${bRight.v}) / 2",
      Seq(aRight, bRight)
    )
  }

  val leftAvg: GosValue = {
    val v = (aLeft.v + bLeft.v) / 2
    new GosValue(
      v,
      name = s"Left Avg",
      group,
      description = s"(${aLeft.name} + ${bLeft.name}) / 2 : Average of each bank's left-hand values",
      derivation = s"$v = (${aLeft.v} + ${bLeft.v}) / 2",
      Seq(aLeft, bLeft)
    )
  }

  val abRightLeftAvg: GosValue = {
    val v = (aRightLeftDiff.v + bRightLeftDiff.v) / 2
    new GosValue(
      v,
      name = "Diff of A,B Diffs",
      group,
      description = s"(${aRightLeftDiff.name} + ${bRightLeftDiff.name}) / 2 : Average of each bank's right and left-hand differences",
      derivation = s"$v = (${aRightLeftDiff.v} + ${bRightLeftDiff.v}) / 2",
      Seq(aRightLeftDiff, bRightLeftDiff)
    )
  }

  val abSkewAvg_deg: GosValue = {
    val v = (aSkew_deg.v + bSkew_deg.v) / 2
    GosValue(
      v,
      name = "A,B Skew Angle Avg",
      group,
      description = s"(${aSkew_deg.name} + ${bSkew_deg.name}) / 2 : Average of skews for bank A and B",
      derivation = s"$v = (${aSkew_deg.v} + ${bSkew_deg.v}) / 2",
      Seq(aSkew_deg, bSkew_deg),
      units = "deg"
    )
  }

  val abSkewAvg_mmPer40cm: GosValue = {
    val v = (aSkew_mmPer40cm.v + bSkew_mmPer40cm.v) / 2
    GosValue(
      v,
      name = "A,B Skew Avg",
      group,
      description = s"(${aSkew_mmPer40cm.name} + ${bSkew_mmPer40cm.name}) / 2 : Average of skews for bank A and B",
      derivation = s"$v = (${aSkew_mmPer40cm.v} + ${bSkew_mmPer40cm.v}) / 2",
      Seq(aSkew_mmPer40cm, bSkew_mmPer40cm),
      units = "mm/40cm"
    )
  }

  val offset: GosValue = {
    val v = (aRightLeftAvg.v + bRightLeftAvg.v) / 2
    new GosValue(
      v,
      name = s"Offset",
      group,
      description = s"(${aRightLeftAvg.name} + ${bRightLeftAvg.name}) / 2 : Average of averages for bank A and B",
      derivation = s"$v = (${aRightLeftAvg.v} + ${bRightLeftAvg.v}) / 2",
      Seq(aRightLeftAvg, bRightLeftAvg)
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

  private val group = "J" + colAngle.bankA.angleRounded

  // ----- Jaw (the one near the vertical center of the image) -----

  val jawRight: GosValue = {
    // find the one closest to zero.  Should be the top one unless they change the RTPLAN.
    val v = Seq(jawPair.topRightValue_mm, jawPair.bottomRightValue_mm).flatten.minBy(_.abs)

    new GosValue(
      v = v,
      name = s"Jaw Right",
      group,
      description = s"Measurement taken at right of edge of jaw near vertical center of image.",
      v
    )
  }

  val jawLeft: GosValue = {
    // find the one closest to zero.  Should be the top one unless they change the RTPLAN.
    val v = Seq(jawPair.topLeftValue_mm, jawPair.bottomLeftValue_mm).flatten.minBy(_.abs)
    new GosValue(
      v = v,
      name = s"Jaw Left",
      group,
      description = s"Measurement taken at left of edge of jaw near vertical center of image.",
      v
    )
  }

  val jawRightLeftDiff: GosValue = {
    val v = jawRight.v - jawLeft.v
    new GosValue(
      v = v,
      name = "Jaw Diff",
      group,
      description = s"Jaw difference between ${jawRight.name} - ${jawLeft.name}.",
      derivation = s"$v = ${jawRight.v} - ${jawLeft.v}"
    )
  }

  val jawSkew_deg: GosValue = {
    val v = if (jawIsTop) jawPair.topHorzSkew_deg else jawPair.bottomHorzSkew_deg
    GosValue(
      v.get,
      name = "Jaw Skew angle",
      group,
      description = s"atan(${jawRightLeftDiff.name} / ${c.separation.name})",
      derivation = s"$v = atan(${jawRightLeftDiff.v} / ${c.separation.v})",
      Seq(jawRightLeftDiff, c.separation),
      units = "deg"
    )
  }

  val jawSkew_mmPer40cm: GosValue = {
    val v = if (jawIsTop) jawPair.topHorzSkew_mmPer40cm else jawPair.bottomHorzSkew_mmPer40cm
    GosValue(
      v.get,
      name = "Jaw Skew",
      group,
      description = s"(${jawRightLeftDiff.name} / ${c.separation.name}) * 40",
      derivation = s"$v = (${jawRightLeftDiff.v} / ${c.separation.v}) * 40",
      Seq(jawRightLeftDiff, c.separation),
      units = "mm/40cm"
    )
  }

  val jawAvg: GosValue = {
    val v = (jawRight.v + jawLeft.v) / 2
    new GosValue(
      v = v,
      name = s"Jaw Avg",
      group,
      description = s"Jaw average (midpoint) between ${jawRight.name} and ${jawLeft.name}.",
      derivation = s"$v = (${jawRight.v} + ${jawLeft.v}) / 2"
    )
  }

  // ----- Differences between Bank A and Jaw -----

  val aJawRightDiff: GosValue = {
    val v = c.aRight.v - jawRight.v
    new GosValue(
      v,
      name = "Right Diff",
      group,
      description = s"${c.aRight.name} - ${jawRight.name} : Difference between bank A and jaw right-hand values",
      derivation = s"$v = ${c.aRight.v} - ${jawRight.v}",
      Seq(c.aRight, jawRight)
    )
  }

  val aJawLeftDiff: GosValue = {
    val v = c.aLeft.v - jawLeft.v
    new GosValue(
      v,
      name = "Left Diff",
      group,
      description = s"${c.aLeft.name} - ${jawLeft.name} : Difference between bank A and left-hand values",
      derivation = s"$v = ${c.aLeft.v} - ${jawLeft.v}",
      Seq(c.aLeft, jawLeft)
    )
  }

  val aJawRightLeftDiff: GosValue = {
    val v = c.aRightLeftDiff.v - jawRightLeftDiff.v
    new GosValue(
      v,
      name = "Right-Left Diffs Diff",
      group,
      description = s"${c.aRightLeftDiff.name} - ${jawRightLeftDiff.name} : Difference between bank A and jaw right-hand differences",
      derivation = s"$v = ${c.aRightLeftDiff.v} - ${jawRightLeftDiff.v}",
      Seq(c.aRightLeftDiff, jawRightLeftDiff)
    )
  }

  val aJawSkewDiff_deg: GosValue = {
    val v = c.aSkew_deg.v - jawSkew_deg.v
    GosValue(
      v,
      name = "Skew Angle Diff",
      group,
      description = s"${c.aSkew_deg.name} - ${jawSkew_deg.name} : Difference of skew between bank A and Jaw",
      derivation = s"$v = ${c.aSkew_deg.v} - ${jawSkew_deg.v}",
      Seq(c.aSkew_deg, jawSkew_deg),
      units = "deg"
    )
  }

  val aJawSkewDiff_mmPer40cm: GosValue = {
    val v = c.aSkew_mmPer40cm.v - jawSkew_mmPer40cm.v
    GosValue(
      v,
      name = "Skew Diff",
      group,
      description = s"${c.aSkew_mmPer40cm.name} - ${jawSkew_mmPer40cm.name} : Difference of skew between bank A and Jaw",
      derivation = s"$v = ${c.aSkew_mmPer40cm.v} - ${jawSkew_mmPer40cm.v}",
      Seq(c.aSkew_mmPer40cm, jawSkew_mmPer40cm),
      units = "mm/40cm"
    )
  }

  val gap: GosValue = {
    val v = c.aRightLeftAvg.v - jawAvg.v
    new GosValue(
      v,
      name = s"Gap",
      group,
      description = s"${c.aRightLeftAvg.name} - ${jawAvg.name} : Difference of averages between bank A and Jaw",
      derivation = s"$v = ${c.aRightLeftAvg.v} - ${jawAvg.v}",
      Seq(c.aRightLeftAvg, jawAvg)
    )
  }

  // ----- averages -----

  val aJawRightAvg: GosValue = {
    val v = (c.aRight.v + jawRight.v) / 2
    new GosValue(
      v,
      name = "Right Avg",
      group,
      description = s"(${c.aRight.name} + ${jawRight.name}) / 2 : Average of bank A and jaw right-hand values",
      derivation = s"$v = (${c.aRight.v} + ${jawRight.v}) / 2",
      Seq(c.aRight, jawRight)
    )
  }

  val aJawLeftAvg: GosValue = {
    val v = (c.aLeft.v + jawLeft.v) / 2
    new GosValue(
      v,
      name = "Left Avg",
      group,
      description = s"(${c.aLeft.name} + ${jawLeft.name}) / 2 : Average of bank A and jaw left-hand values",
      derivation = s"$v = (${c.aLeft.v} + ${jawLeft.v}) / 2",
      Seq(c.aLeft, jawLeft)
    )
  }

  val aJawRightLeftAvg: GosValue = {
    val v = (c.aRightLeftDiff.v + jawRightLeftDiff.v) / 2
    new GosValue(
      v,
      name = s"Right,Left Diffs Avg",
      group,
      description = s"(${c.aRightLeftDiff.name} + ${jawRightLeftDiff.name}) / 2 : Average of bank A and jaw right and left-hand difference values",
      derivation = s"$v = (${c.aRightLeftDiff.v} + ${jawRightLeftDiff.v}) / 2",
      Seq(c.aRightLeftDiff, jawRightLeftDiff)
    )
  }

  val aJawSkewAvg_deg: GosValue = {
    val v = (c.aSkew_deg.v + jawSkew_deg.v) / 2
    GosValue(
      v,
      name = "Avg Skew Angle",
      group,
      description = s"(${c.aSkew_deg.name} + ${jawSkew_deg.name}) / 2 : Average of skews for bank A and Jaw",
      derivation = s"$v = (${c.aSkew_deg.v} + ${jawSkew_deg.v}) / 2",
      Seq(c.aSkew_deg, jawSkew_deg),
      units = "deg"
    )
  }

  val aJawSkewAvg_mmPer40cm: GosValue = {
    val v = (c.aSkew_mmPer40cm.v + jawSkew_mmPer40cm.v) / 2
    GosValue(
      v,
      name = "Avg Skew",
      group,
      description = s"(${c.aSkew_mmPer40cm.name} + ${jawSkew_mmPer40cm.name}) / 2 : Average of skews for bank A and Jaw",
      derivation = s"$v = (${c.aSkew_mmPer40cm.v} + ${jawSkew_mmPer40cm.v}) / 2",
      Seq(c.aSkew_mmPer40cm, jawSkew_mmPer40cm),
      units = "mm/40cm"
    )
  }

  val offset: GosValue = {
    val v = (c.aRightLeftAvg.v + jawAvg.v) / 2
    new GosValue(
      v,
      name = s"Offset",
      group,
      description = s"(${c.aRightLeftAvg.name} + ${jawAvg.name}) / 2 : Average of averages for bank A and Jaw",
      derivation = s"$v = (${c.aRightLeftAvg.v} + ${jawAvg.v}) / 2",
      Seq(c.aRightLeftAvg, jawAvg)
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

    def findMlc(angle: Int, mlcBank: Int, name: String, top: Boolean): Either[String, GapSkew] = {
      // @formatter:off
      gapSkewList.find(gs =>
        (gs.angleRounded == angle) &&
        gs.edgeList.exists(e => e.isMlc && e.bank == mlcBank) &&
        (
          (  top && gs.topLeftValue_mm.isDefined     && gs.topRightValue_mm.isDefined   ) ||
          ((!top) && gs.bottomLeftValue_mm.isDefined && gs.bottomRightValue_mm.isDefined)
        )
      ) match {
        case Some(mlc) => Right(mlc)
        case _         => Left(name)
      }
      // @formatter:on
    }

    // top X2 ABank
    val c090A = findMlc(90, 2, "Col 90 Bank A / X2", top = true)
    // bottom X1 BBank
    val c090B = findMlc(90, 1, "Col 90 Bank B / X1", top = false)

    // bottom X2 ABank
    val c270A = findMlc(270, 2, "Col 270 Bank A / X2", top = false)
    // bottom X1 BBank
    val c270B = findMlc(270, 1, "Col 270 Bank B / X1", top = true)

    val c270Jaw = {
      gapSkewList.find(gs => (gs.angleRounded == 270) && gs.edgeList.count(e => e.isJaw) == 4 && gs.topRightValue_mm.isDefined && gs.topLeftValue_mm.isDefined) match {
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
