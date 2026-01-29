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
  * @param beam Part of the plan that defines beam delivery.
  */
case class PlannedEdgeSet(beam: AttributeList) extends Logging {
  def this(rtplan: AttributeList, rtimage: AttributeList) = this(Util.getBeamOfRtimage(rtplan, rtimage).get)

  /**
    *
    * @param isX        True if X1/X2, false if Y1/Y2
    * @param loPosition Position of the end of the lower leaf (X1 or Y1). Note: Varian machines do not have A Y collimator.d
    * @param hiPosition Position of the end of the upper leaf (X2 or Y2). Note: Varian machines do not have A Y collimator.
    * @param loBoundary Position of the lower value side of leaf
    * @param hiBoundary Position of the higher value side of leaf
    */
  private case class LeafPair( //
      isX: Boolean,
      loPosition: Double,
      hiPosition: Double,
      loBoundary: Double,
      hiBoundary: Double
  ) {}

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

  private case class Jaw(isX: Boolean, lo: Double, hi: Double) {}

  private def makeJaw(XY: String): Option[Jaw] = {
    try {

      val jawPosAl = {
        val posSeq = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDevicePositionSequence).head.asInstanceOf[SequenceAttribute]
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

    val leafPairList: Seq[LeafPair] = {
      positionList.indices.take(pairCount).flatMap(makePair)
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

        val mlc = MLC(XY.equalsIgnoreCase("X"), boundaryList, positionList)

        Some(mlc)
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
    throw new RuntimeException(s"RTPLAN specifies a Y collimator (MLC), but Y MLC is not supported is not supported.  Only X collimator is supported.")

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** X1 edge */
  val x1: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("X1", edgeType, position)

    def mlc = xMLC.get.leafPairList.map(_.loPosition).max

    def jaw = xJaw.get.lo

    0 match {
      case _ if xJaw.isDefined && xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc > jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, mlc)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if xJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
        PE(EdgeType.MLC, jaw)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** X2 edge */
  val x2: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("X2", edgeType, position)

    def mlc = xMLC.get.leafPairList.map(_.hiPosition).min

    def jaw = xJaw.get.hi

    0 match {

      case _ if xJaw.isDefined && xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc > jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, mlc)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if xJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
        PE(EdgeType.MLC, mlc)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** Y1 edge */
  val y1: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("Y1", edgeType, position)

    def mlc = xMLC.get.leafPairList.map(_.loBoundary).max

    def jaw = yJaw.get.lo

    0 match {
      case _ if yJaw.isDefined && xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
        0 match {
          case _ if mlc < jaw  => PE(EdgeType.MLC, mlc)
          case _ if mlc > jaw  => PE(EdgeType.Jaw, jaw)
          case _ if mlc == jaw => PE(EdgeType.JawAndMLC, jaw)
          case _               => PE(EdgeType.NA, Double.NaN)
        }

      case _ if yJaw.isDefined =>
        PE(EdgeType.Jaw, jaw)

      case _ if xMLC.isDefined =>
        val maxOfLo = xMLC.get.leafPairList.map(_.loBoundary).max
        PE(EdgeType.MLC, maxOfLo)

      case _ =>
        PE(EdgeType.NA, Double.NaN)
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /** Y2 edge */
  val y2: PlannedEdge = {
    def PE(edgeType: EdgeType.Value, position: Double) = PlannedEdge("Y2", edgeType, position)
    def mlc = xMLC.get.leafPairList.map(_.hiBoundary).min
    def jaw = yJaw.get.hi

    0 match {
      case _ if yJaw.isDefined && xMLC.isDefined && xMLC.get.leafPairList.nonEmpty =>
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
    s"X1: $x1    X2: $x2    Y1: $y1    Y2: $y2"
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

  case class PlannedEdge(name: String, edgeType: EdgeType.Value, position: Double) {}

  def main(args: Array[String]): Unit = {
    //noinspection SpellCheckingInspection
    val dir = new File("src/test/resources/TestWLEdgeType")
    val planFile = new File(dir, "RTPLAN1.dcm")
    val rtplan = new DicomFile(planFile).attributeList.get

    def show(beam: AttributeList): Unit = {

      val planenedEdgeSet = new PlannedEdgeSet(beam)

      val beamName = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamName).head.getSingleStringValueOrEmptyString()
        "%-20s".format(name)
      }

      val beamNumber = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamNumber).head.getIntegerValues.head
        "%3d".format(name)
      }

      val msg = s"beam: $beamName : $beamNumber :: $planenedEdgeSet"

      println(msg)

    }

    val beamNumberList = DicomUtil.findAllSingle(rtplan, TagByName.BeamNumber).flatMap(_.getIntegerValues).distinct.sorted

    beamNumberList.foreach(beamNumber => show(DicomUtil.getBeamOfRtimage(rtplan, beamNumber).get))

  }
}
