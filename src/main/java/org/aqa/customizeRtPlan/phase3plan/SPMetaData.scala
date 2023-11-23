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
import org.aqa.web.WebUtil.ValueMapT

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
    * Return the URL prefix for the example images directory.
    *
    * @return Prefix for example beam images.
    */
  private def exampleBeamImagesUrlPrefix(): String = {
    val prefix = "/static/images/exampleBeamImages/"
    val patternList = Seq("30x40", "40x40")
    val pattern = patternList.find(p => epid.model.contains(p)).get
    val dirName = exampleBeamImagesDirNameList.find(example => example.contains(pattern)).get
    prefix + dirName
  }

  /**
    * Given a beam, return the URL to an example image for that beam.
    * If there is nothing appropriate, then return an 'NA' image.
    *
    * @param beam For this beam.
    * @return URL of image.
    */
  def urlOfExampleImage(beam: Beam): String = {
    def normalize(t: String) = t.replaceAll("[^a-z]", "-")

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

  private def makeSubProcedureList: Seq[SubProcedure] = {

    val sp = this // force initialization of this object

    Seq(
      new SPFocalSpot(sp),
      new SPCenterDose(sp)
    )
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
  val exampleImageFileList: Seq[File] = Util.listDirFiles(exampleBeamImagesDir(epid))

  /** List of Phase3 sub-procedures. */
  val subProcedureList: Seq[SubProcedure] = makeSubProcedureList

  /**
    * Return the list of sub procedures that are selecting the beam.
    * @param beam For this beam.
    * @param valueMap User selections.
    * @return List of sub procedures that are selecting the beam.
    */
  def beamUseList(beam: Beam, valueMap: ValueMapT): Seq[SubProcedure] = {
    val list = subProcedureList.flatMap(_.selectionList).filter(sel => sel.isSelected(valueMap)).map(_.subProcedure).groupBy(_.name).map(_._2.head)
    list.toSeq
  }

  /**
    * Determine if the beam is used by the sub-procedure.
    * @param beam For this beam.
    * @param subProcedure For this sub procedure.
    * @param valueMap User selections.
    * @return True if the beam is used by the sub-procedure.
    */
  def subProcedureUsesBeam(beam: Beam, subProcedure: SubProcedure, valueMap: ValueMapT): Boolean = {
    beamUseList(beam, valueMap).exists(_.name.equals(subProcedure.name))
  }
}
