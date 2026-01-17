package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util

import java.io.File

object EdgeType extends Enumeration with Logging {
  val Jaw: Value = Value
  val MLC: Value = Value
  //noinspection ScalaWeakerAccess
  val JawAndMLC: Value = Value

  def main(args: Array[String]): Unit = {
    //noinspection SpellCheckingInspection
    val dir = new File("src/test/resources/TestWLEdgeType")
    val planFile = new File(dir, "RTPLAN1.dcm")
    val rtplan = new DicomFile(planFile).attributeList.get

    def show(beam: AttributeList): Unit = {

      val edgeType = new EdgeType(beam)

      val beamName = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamName).head.getSingleStringValueOrEmptyString()
        "%-20s".format(name)
      }

      val beamNumber = {
        val name = DicomUtil.findAllSingle(beam, TagByName.BeamNumber).head.getIntegerValues.head
        "%3d".format(name)
      }

      val msg = s"beam: $beamName : $beamNumber :: $edgeType"

      println(msg)

    }

    val beamNumberList = DicomUtil.findAllSingle(rtplan, TagByName.BeamNumber).flatMap(_.getIntegerValues).distinct.sorted

    beamNumberList.foreach(beamNumber => show(DicomUtil.getBeamOfRtimage(rtplan, beamNumber).get))

  }
}









/**
  * Determine whether the jaw or MLC are defining the edge of a Winston Lutz field.
  * @param beam Part of the plan that defines beam delivery.
  */
case class EdgeType(beam: AttributeList) {
  def this(rtplan: AttributeList, rtimage: AttributeList) = this(Util.getBeamOfRtimage(rtplan, rtimage).get)

  /** The part of the plan that positions this beam. */
  // private val beam = Util.getBeamOfRtimage(rtplan, rtimage).get

  private val BeamLimitingDevicePositionSequence = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDevicePositionSequence).head
  private val list: Seq[AttributeList] = DicomUtil.alOfSeq(BeamLimitingDevicePositionSequence.asInstanceOf[SequenceAttribute])

  private case class Edge(al: AttributeList) {
    val name: String = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString()
    private val leafList: Seq[Double] = al.get(TagByName.LeafJawPositions).getDoubleValues
    private val numPair = leafList.size / 2

    /**
      * Return true if the opposing pairs of leaves are separated.
      * @param leafNum Index of leaf in bank 1.
      * @return True if separated, false if touching.
      */
    private def isSeparated(leafNum: Int): Boolean = {
      val separation = (leafList(leafNum) - leafList(leafNum + numPair)).abs
      separation > 1
    }

    private val pairList = (0 until numPair).filter(isSeparated).map(pairNum => (leafList(pairNum), leafList(pairNum + numPair)))
    val positionLo: Double = pairList.map(_._1).min
    val positionHi: Double = pairList.map(_._2).max

    override def toString: String = {
      s"Name: $name    positionLo: $positionLo    positionHi: $positionHi     "
    }
  }

  private val edgeList = list.map(Edge)

  private val JawX = edgeList.find(e => e.name.equals("X") || e.name.equals("ASYMX"))
  private val MLCX = edgeList.find(e => e.name.equals("MLCX"))
  private val JawY = edgeList.find(e => e.name.equals("Y") || e.name.equals("ASYMY"))
  private val MLCY = edgeList.find(e => e.name.equals("MLCY"))

  // TODO : account for the leaf sides.  Use: 300a,00be LeafPositionBoundaries

  val x1: EdgeType.Value = 0 match {
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionLo > MLCX.get.positionLo =>
      EdgeType.Jaw
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionLo < MLCX.get.positionLo =>
      EdgeType.MLC
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionLo == MLCX.get.positionLo =>
      EdgeType.JawAndMLC
    case _ if MLCX.isDefined =>
      EdgeType.MLC
    case _ =>
      EdgeType.Jaw // assume a default
  }

  val x2: EdgeType.Value = 0 match {
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionHi < MLCX.get.positionHi =>
      EdgeType.Jaw
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionHi > MLCX.get.positionHi =>
      EdgeType.MLC
    case _ if JawX.isDefined && MLCX.isDefined && JawX.get.positionHi == MLCX.get.positionHi =>
      EdgeType.JawAndMLC
    case _ if MLCX.isDefined =>
      EdgeType.MLC
    case _ =>
      EdgeType.Jaw // assume a default
  }

  val y1: EdgeType.Value = 0 match {
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionLo > MLCY.get.positionLo =>
      EdgeType.Jaw
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionLo < MLCY.get.positionLo =>
      EdgeType.MLC
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionLo == MLCY.get.positionLo =>
      EdgeType.JawAndMLC
    case _ if MLCY.isDefined =>
      EdgeType.MLC
    case _ =>
      EdgeType.Jaw // assume a default
  }

  val y2: EdgeType.Value = 0 match {
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionHi < MLCY.get.positionHi =>
      EdgeType.MLC
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionHi > MLCY.get.positionHi =>
      EdgeType.Jaw
    case _ if JawY.isDefined && MLCY.isDefined && JawY.get.positionHi == MLCY.get.positionHi =>
      EdgeType.JawAndMLC
    case _ if MLCY.isDefined =>
      EdgeType.MLC
    case _ =>
      EdgeType.Jaw // assume a default
  }

  override def toString: String = {
    s"X1: $x1    X2: $x2    Y1: $y1    Y2: $y2"
  }

}
