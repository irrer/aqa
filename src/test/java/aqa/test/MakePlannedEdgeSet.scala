/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package aqa.test

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile

import java.io.File

/**
  * Make an RTPLAN that will exercise the self test for PlannedEdgeSet.
  *
  * This is for development testing only.
  *
  * The file RTPLAN.dcm is created from RTPLAN_orig.dcm  .  This should only ever need to be done once.
  */
object MakePlannedEdgeSet {

  private val dir = new File("src/test/resources/TestWinLutz360PlannedEdgeSet")
  private val origPlan = new DicomFile(new File(dir, "RTPLAN_orig.dcm")).attributeList.get
  private val newPlanFile = new File(dir, "RTPLAN.dcm")

  private case class BeamSpec(name: String, X1: Double = -15, X2: Double = 15, Y1: Double = -15, Y2: Double = 15) {}

  private def setBeam(beamSpec: BeamSpec, beam: AttributeList): Unit = {

    val beamName = beam.get(TagByName.BeamName)
    beamName.removeValues()
    beamName.addValue(beamSpec.name)

    val cps = {
      val cpsSeq = DicomUtil.findAllSingle(beam, TagByName.BeamLimitingDevicePositionSequence).head

      val al = cpsSeq.asInstanceOf[SequenceAttribute]

      DicomUtil.alOfSeq(al)
    }

    def cpsTypeName(al: AttributeList): String = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString

    def getJaw(jawName: String) = cps.find(c => cpsTypeName(c).equals(jawName)).get.get(TagByName.LeafJawPositions)

    def setJaw(jawName: String, one: Double, two: Double): Unit = {
      val jaw = getJaw(jawName)
      jaw.removeValues()
      jaw.addValue(one)
      jaw.addValue(two)
    }

    setJaw("X", beamSpec.X1, beamSpec.X2)
    setJaw("Y", beamSpec.Y1, beamSpec.Y2)
  }

  def main(args: Array[String]): Unit = {

    println("Starting")

    val beamSpecList = Seq(
      BeamSpec("AllJaw"),
//
      BeamSpec("X1_Jaw___9", X1 = -9),
      BeamSpec("X1_And__10", X1 = -10),
      BeamSpec("X1_MLC__11", X1 = -11),
      BeamSpec("X2_Jaw___9", X2 = 9),
      BeamSpec("X2_And__10", X2 = 10),
      BeamSpec("X2_MLC__11", X2 = 11),
      BeamSpec("Y1_Jaw__-9", Y1 = -9),
      BeamSpec("Y1_And_-10", Y1 = -10),
      BeamSpec("Y1_MLC_-11", Y1 = -11),
      BeamSpec("Y2_Jaw__-9", Y2 = 9),
      BeamSpec("Y2_And_-10", Y2 = 10),
      BeamSpec("Y2_MLC_-11", Y2 = 11),
      BeamSpec("X1X2_Jaw__-9", X1 = -9, X2 = 9),
      BeamSpec("Y1Y2_Jaw__-9", Y1 = -9, Y2 = 9),
      BeamSpec("AllAnd", X1 = -10, X2 = 10, Y1 = -10, Y2 = 10)
    )

    val beamNumberList = DicomUtil.findAllSingle(origPlan, TagByName.BeamNumber).map(_.getIntegerValues.head)

    val beamList = beamNumberList.flatMap(beamNum => DicomUtil.getBeamOfRtimage(origPlan, beamNum))

    beamSpecList.indices.foreach(index => setBeam(beamSpecList(index), beamList(index)))

    newPlanFile.delete()

    DicomUtil.writeAttributeListToFile(origPlan, newPlanFile, "AQA")
    println("wrote file to " + newPlanFile.getAbsolutePath)

    println("Done")
  }
}
