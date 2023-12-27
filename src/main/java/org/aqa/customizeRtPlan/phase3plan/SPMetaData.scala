package org.aqa.customizeRtPlan.phase3plan

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

}
