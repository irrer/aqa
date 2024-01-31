package org.aqa.customizeRtPlan.phase3plan

/*
 * Copyright 2024 Regents of the University of Michigan
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

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.EPID

import java.io.File

case class SPMetaData(machine: Machine) {

  private val exampleBeamImagesDirNameList: Seq[String] = Util.listDirFiles(new File(Config.imageDirFile, "exampleBeamImages")).map(_.getName)

  private val exampleBeamImagesPatternList = Seq("30x40", "40x40")

  private def exampleBeamImagesDir(epid: EPID): File = {
    val pattern = exampleBeamImagesPatternList.find(p => epid.model.contains(p)).get
    val dirName = exampleBeamImagesDirNameList.find(example => example.contains(pattern)).get

    val dir = {
      val d = new File(Config.imageDirFile, "exampleBeamImages")
      new File(d, dirName)
    }

    dir
  }

  /**
    * Given a beam, return the URL to an example image for that beam.
    * If there is nothing appropriate, then return an 'NA' image.
    *
    * @param beam For this beam.
    * @return URL of image.
    */
  def urlOfExampleImage(beam: Beam): String = {
    def normalize(t: String) = t.replaceAll("[^a-z0-9]", "-")

    val imageFile: File = {
      val file = exampleImageFileList.find(f => normalize(f.getName).equalsIgnoreCase(normalize(beam.beamName + ".png")))
      if (file.isDefined)
        file.get
      else
        new File(exampleImageFileList.head.getParentFile, "NoImage.png")
    }

    val imageUrl = imageFile.getAbsolutePath.drop(Config.staticDirFile.getParentFile.getAbsolutePath.length).replace('\\', '/')
    imageUrl
  }

  /**
    * Get all beams from Phase3 rtplans that are defined in the configured plans.
    *
    * Some of these beams may specify energies or other parameters that are not supported by
    * the machine.  They are passed to the sub-procedures anyway that they may serve as a
    * prototype for a beam.  It is up to the SubProcedure to handle this.
    *
    * @return List of beams from multiple plans.
    */
  private def makePrototypeBeamList(): Seq[Beam] = {

    def beamsFromPlan(plan: AttributeList): Seq[Beam] = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).map(al => Beam.makeBeamFromAl(machine, al))

    val collimatorModel = MultileafCollimator.get(machine.multileafCollimatorPK).get.model

    def getPlan(name: String): Option[AttributeList] = {
      try {
        val plan = Config.PlanFileList.find(pf => pf.collimatorModel.equals(collimatorModel) && pf.procedure.equals(name)).get
        Some(DicomFile(plan.file).attributeList.get)
      } catch {
        case _: Throwable =>
          None
      }
    }

    val beamList = Seq("Phase3", "FocalSpot").flatMap(getPlan).flatMap(beamsFromPlan)
    beamList
  }

  /** List of beam energies for this machine. */
  val beamEnergyList: Seq[MachineBeamEnergy] = {
    val machineType = MachineType.get(machine.machineTypePK).get
    CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get).map(energy => CustomizeRtPlanUtil.resolveEnergy(energy, machineType))
  }

  /** Aggregated list of beams from all Phase3 RTPLAN files.  These beams may specify energies not supported by this machine. */
  val prototypeBeamList: Seq[Beam] = makePrototypeBeamList()

  /** Multileaf collimator for this machine. */
  val multileafCollimator: MultileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get

  /** EPID for this machine. */
  val epid: EPID = EPID.get(machine.epidPK).get

  /** List of PNG files representing examples of beams. */
  private val exampleImageFileList: Seq[File] = Util.listDirFiles(exampleBeamImagesDir(epid))

  /**
    * Determine the machine supports this energy.
    * @param energy Beam to examine.
    * @return True if the machine supports it.
    */
  def beamEnergyIsSupported(energy: MachineBeamEnergy): Boolean = {

    /** Return true if the beams have the same energy parameters. */
    def isSame(beamEnergyA: MachineBeamEnergy, beamEnergyB: MachineBeamEnergy): Boolean = {

      def pho = beamEnergyA.photonEnergy_MeV.get == beamEnergyB.photonEnergy_MeV.get

      def dose = {
        (beamEnergyA.maxDoseRate_MUperMin, beamEnergyB.maxDoseRate_MUperMin) match {
          case (Some(doseA), Some(doseB)) => doseA == doseB
          case _                          => true
        }
      }

      def fff: Boolean = {
        (beamEnergyA.fffEnergy_MeV, beamEnergyB.fffEnergy_MeV) match {
          case (Some(fffA), Some(fffB)) => fffA == fffB
          case _                        => true
        }
      }

      pho && dose && fff
    }

    val same = beamEnergyList.exists(supportedEnergy => isSame(supportedEnergy, energy))
    same
  }

}
