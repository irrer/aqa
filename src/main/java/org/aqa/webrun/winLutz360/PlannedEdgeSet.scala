package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.winLutz360.PlannedEdgeSet.EdgeType
import org.aqa.webrun.winLutz360.PlannedEdgeSet.PlannedEdge

import java.io.File

/**
  * Determine whether the jaw or MLC are defining the edge of a Winston Lutz field.
  *
  * Caveats:
  *
  *  1: This class assumes that the beams describe a rectangular field.  Any other shapes
  *     will produce unpredictable results.
  *
  *  2: The DICOM specification allows for the definition of Y collimators, but this
  *     class ignores them except for logging a warning message.
  *
  * @param beam Part of the plan that defines beam delivery.
  */
case class PlannedEdgeSet(beam: AttributeList) extends Logging {
  def this(rtplan: AttributeList, rtimage: AttributeList) = this(Util.getBeamOfRtimage(rtplan, rtimage).get)

  /**
    *
    * @param isX        True if X1/X2, false if Y1/Y2
    * @param loPosition Position of the end of the lower leaf (X1 or Y1). Note: Varian machines do not have A Y collimator.d
    * @param hiPosition Position of the end of the upper leaf (X2 or Y2). Note: Varian machines do not have A Y collimator.
    * @param topBoundary Position of the top value side of leaf with collimator angle 0
    * @param botBoundary Position of the bottom side of leaf with collimator angle 0
    */
  private case class LeafPair( //
      isX: Boolean,
      loPosition: Double,
      hiPosition: Double,
      topBoundary: Double,
      botBoundary: Double
  ) {
    override def toString: String = {
      val x = if (isX) "X" else "Y"
      s"$x  loPos: $loPosition   hiPos: $hiPosition   topBoundary: $topBoundary      botBoundary: $botBoundary"
    }
  }

  /**
    * Determine if the given spec has a device type with the given name.
    *
    * @param spec Either a BeamLimitingDeviceSequence or BeamLimitingDevicePositionSequence.
    * @param name Name as specified in RTBeamLimitingDeviceType.
    * @return True if the given spec has a device type with the given name.
    */
  private def hasName(spec: AttributeList, name: String): Boolean = {
    spec.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString().equalsIgnoreCase(name)
  }

  private case class Jaw(isX: Boolean, lo: Double, hi: Double) {
    override def toString: String = {
      val x = if (isX) "X" else "Y"
      s"$x    lo: ${"%3d".format(lo.toInt)}    hi: ${"%3d".format(hi.toInt)}"
    }
  }

  private def makeJaw(XY: String): Option[Jaw] = {
    try {

      val jawPosAl = {
        val posSeq = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDevicePositionSequence).head.asInstanceOf[SequenceAttribute]
        //noinspection SpellCheckingInspection
        DicomUtil.alOfSeq(posSeq).find(pos => hasName(pos, XY) || hasName(pos, s"ASYM$XY"))
      }

      val pos = jawPosAl.get.get(TagByName.LeafJawPositions).getDoubleValues

      val jaw = Jaw(XY.equalsIgnoreCase("X"), pos.head, pos(1))

      Some(jaw)

    } catch {
      case _: Throwable =>
        logger.warn(s"Could not find $XY jaw position.  This should be defined in the RTPLAN as BeamLimitingDevicePositionSequence with LeafJawPositions, but is missing.")
        None
    }
  }

  private case class MLC(isX: Boolean, boundaryList: Seq[Double], positionList: Seq[Double]) {

    private val pairCount = positionList.size / 2

    private def makePair(index: Int): Option[LeafPair] = {
      val lo = positionList(index)
      val hi = positionList(index + pairCount)

      if ((hi - lo).abs < 0.01)
        None
      else {
        val lp = LeafPair(isX, lo, hi, boundaryList(index), boundaryList(index + 1))
        Some(lp)
      }
    }

    private val leafPairList: Seq[LeafPair] = {
      positionList.indices.take(pairCount).flatMap(makePair)
    }

    private def posList: Seq[Double] = leafPairList.flatMap(lp => Seq(lp.loPosition, lp.hiPosition))

    private def bndList: Seq[Double] = leafPairList.flatMap(lp => Seq(lp.loPosition, lp.hiPosition))

    /** Position of X1 edge. */
    val X1: Double = {
      if (isX)
        posList.min
      else
        bndList.min
    }

    /** Position of X2 edge. */
    val X2: Double = {
      if (isX)
        posList.max
      else
        bndList.max
    }

    /** Position of Y1 edge. */
    val Y1: Double = {
      if (isX)
        bndList.min
      else
        posList.min
    }

    /** Position of Y2 edge. */
    val Y2: Double = {
      if (isX)
        bndList.max
      else
        posList.max
    }

    override def toString: String = {
      val x = if (isX) "X" else "Y"
      val ll = leafPairList.mkString("\n    ")
      s"$x\n    $ll    "
    }
  }

  private def makeMLC(XY: String): Option[MLC] = {
    try {

      val mlcPosAl = {
        val posSeq = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDevicePositionSequence).head.asInstanceOf[SequenceAttribute]
        DicomUtil.alOfSeq(posSeq).find(pos => hasName(pos, s"MLC$XY"))
      }

      if (mlcPosAl.isEmpty)
        None
      else {
        val positionList = mlcPosAl.get.get(TagByName.LeafJawPositions).getDoubleValues.toSeq

        val boundaryList = {
          val defSeq = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDeviceSequence).head.asInstanceOf[SequenceAttribute]
          val list = DicomUtil.alOfSeq(defSeq).find(pos => hasName(pos, s"MLC$XY")).get
          list.get(TagByName.LeafPositionBoundaries).getDoubleValues
        }

        if (boundaryList.isEmpty)
          None
        else {
          val mlc = MLC(XY.equalsIgnoreCase("X"), boundaryList, positionList)

          Some(mlc)
        }
      }
    } catch {
      case _: Throwable =>
        logger.warn(s"Error making MLC $XY.  Missing DICOM metadata in RTPLAN. Required: BeamLimitingDeviceSequence, LeafPositionBoundaries, LeafJawPositions.")
        None
    }
  }

  private val xJaw = makeJaw("X")
  private val yJaw = makeJaw("Y")

  private val xMLC = makeMLC("X")
  private val yMLC = makeMLC("Y") // Y MLC not supported

  if (yMLC.isDefined)
    logger.warn(s"RTPLAN specifies a Y collimator (MLC), but Y MLC is not supported and is ignored.  Only X collimator is supported.")

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** X1 edge */
  val x1: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("X1", edgeType, position)

    def mlc = xMLC.get.X1

    def jaw = xJaw.get.lo

    0 match {
      case _ if xJaw.isDefined && xMLC.isDefined =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc > jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, mlc)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if xJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined =>
        PE(EdgeType.MLC, jaw)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** X2 edge */
  val x2: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("X2", edgeType, position)

    def mlc = xMLC.get.X2

    def jaw = xJaw.get.hi

    0 match {

      case _ if xJaw.isDefined && xMLC.isDefined =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc > jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, mlc)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if xJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined =>
        PE(EdgeType.MLC, mlc)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** Y1 edge */
  val y1: PlannedEdge = {

    /** Negate position to convert from standard coordinates to AQA coordinates. */
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("Y1", edgeType, -position)

    def mlc = xMLC.get.Y1

    def jaw = yJaw.get.lo

    0 match {
      case _ if yJaw.isDefined && xMLC.isDefined =>
        0 match {
          case _ if mlc > jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc < jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, jaw)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if yJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined =>
        PE(EdgeType.MLC, mlc)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** Y2 edge */
  val y2: PlannedEdge = {

    /** Negate position to convert from standard coordinates to AQA coordinates. */
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("Y2", edgeType, -position)

    def mlc = xMLC.get.Y2

    def jaw = yJaw.get.hi

    0 match {
      case _ if yJaw.isDefined && xMLC.isDefined =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc > jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, mlc)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if xJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined =>
        PE(EdgeType.MLC, mlc)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  override def toString: String = {
    s"$x1  |  $x2  |  $y1  |  $y2"
  }

}

object PlannedEdgeSet extends Logging {

  object EdgeType extends Enumeration {

    val Jaw: Value = Value
    val MLC: Value = Value
    //noinspection ScalaWeakerAccess
    val JawAndMLC: Value = Value
    val NA: Value = Value
  }

  case class PlannedEdge(name: String, edgeType: EdgeType.Value, position: Double) {
    override def toString: String = s"edge: $name    type: $edgeType    pos: ${"%3d".format(position.toInt)}"
  }

  def main(args: Array[String]): Unit = {
    //noinspection SpellCheckingInspection
    val dir = new File("src/test/resources/TestWinLutz360PlannedEdgeSet")
    val planFile = new File(dir, "RTPLAN.dcm")
    val rtplan = new DicomFile(planFile).attributeList.get

    def show(beam: AttributeList): Unit = {

      val plannedEdgeSet = new PlannedEdgeSet(beam)

      val beamName = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamName).head.getSingleStringValueOrEmptyString()
        "%-12s".format(name)
      }

      val beamNumber = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamNumber).head.getIntegerValues.head
        "%3d".format(name)
      }

      val xJaw = plannedEdgeSet.makeJaw("X")
      val yJaw = plannedEdgeSet.makeJaw("Y")

      val msg = s"beam: $beamName : $beamNumber   xJaw: ${xJaw.get}    yJaw: ${yJaw.get}    ::   $plannedEdgeSet"

      println(msg)

    }

    val beamNumberList = DicomUtil.findAllSingle(rtplan, TagByName.BeamNumber).flatMap(_.getIntegerValues)

    {
      println("MLC parameters")
      val plannedEdgeSet = new PlannedEdgeSet(DicomUtil.getBeamOfRtimage(rtplan, 2).get)
      val xMLC = plannedEdgeSet.makeMLC("X")
      println("MLCX:\n" + xMLC.get)
    }

    println("\nAll edges should be MLC except when specified otherwise in the beam name.\n")

    beamNumberList.foreach(beamNumber => show(DicomUtil.getBeamOfRtimage(rtplan, beamNumber).get))

  }
}
