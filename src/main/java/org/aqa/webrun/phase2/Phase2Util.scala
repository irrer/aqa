package org.aqa.webrun.phase2

import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import org.aqa.db.Machine
import com.pixelmed.dicom.SOPClass
import org.aqa.Logging
import org.aqa.Util
import java.io.File

object Phase2Util extends Logging {
  /**
   * Determine if the given image references the given plan.
   */
  def imageReferencesPlan(plan: DicomFile, image: DicomFile): Boolean = {
    try {
      val planUID = plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
      def doesRef(al: AttributeList) = al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull.equals(planUID)
      Util.seq2Attr(image.attributeList.get, TagFromName.ReferencedRTPlanSequence).map(al => doesRef(al)).nonEmpty
    } catch {
      case t: Throwable => false
    }
  }

  /**
   * Get all RTPLAN files, including any that were just now downloaded and any that are in the
   * machine's configuration directory.  If a plan is found more than once then return only one
   * occurrence of it.  If the plan occurs both in the machine's configuration directory and was
   * downloaded, then prefer the one in the machine's configuration directory.
   */
  def getPlanList(dicomList: Seq[DicomFile], machine: Machine): Seq[DicomFile] = {
    val configuredPlans: Seq[DicomFile] = {
      try {
        DicomFile.readDicomInDir(machine.configDir.get).filter(df => df.isModality(SOPClass.RTPlanStorage))
      } catch {
        case t: Throwable => {
          logger.warn("Unexpected problem while getting pre-configured RTPLANs: " + t)
          Seq[DicomFile]()
        }
      }
    }

    val downloadedPlans: Seq[DicomFile] = try {
      val configSopList = configuredPlans.map(c => Util.sopOfAl(c.attributeList.get))
      dicomList.filter(df => df.isModality(SOPClass.RTPlanStorage)).filter(d => !configSopList.contains(Util.sopOfAl(d.attributeList.get)))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected problem while getting RTPLAN: " + t)
        Seq[DicomFile]()
      }
    }

    DicomFile.distinctSOPInstanceUID(configuredPlans ++ downloadedPlans)
  }

  /**
   * If the serial number for the machine is not already set, then set it by using the DeviceSerialNumber in the RTIMAGE.
   */
  def setMachineSerialNumber(machine: Machine, rtimage: AttributeList) = {
    if (machine.serialNumber.isEmpty) {
      try {
        val DeviceSerialNumber = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrNull
        if (DeviceSerialNumber != null) Machine.setSerialNumber(machine.machinePK.get, DeviceSerialNumber)
      } catch {
        case t: Throwable => logger.warn("Unable to update machine serial number " + machine + " : " + t)
      }
    }
  }

  /**
   * If a plan was used that was not already saved, then save it.
   */
  def saveRtplan(machine: Machine, plan: DicomFile) = {
    try {
      Machine.get(machine.machinePK.get) match {
        case Some(mach) => {
          val cfgDir = mach.configDir.get
          if (!cfgDir.equals(plan.file.getParentFile)) {
            val data = Util.readBinaryFile(plan.file)
            val planFile = new File(cfgDir, plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull + Util.dicomFileNameSuffix)
            Util.writeBinaryFile(planFile, data.right.get)
            logger.info("Wrote new plan file " + planFile)
          }
        }
        case _ => logger.error("Machine not in database.  Unable to save rtplan for machine " + machine)
      }
    } catch {
      case t: Throwable => logger.error("Unable to save rtplan for machine " + machine + " : " + t)
    }
  }

}