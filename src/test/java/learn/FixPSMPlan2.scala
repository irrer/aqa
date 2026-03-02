package learn

import com.pixelmed.dicom.Attribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile

import java.io.File

/**
  * View image characteristics.  Review coordinate system.
  */
object FixPSMPlan2 {

  // private val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_6xFFF_250317")
  private val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_10x_250317")
  // private val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_10xFFF_250317/")
  private val plan = new DicomFile(new File(dir, "newRTPLAN.dcm")).attributeList.get
  private val planOutFile = new File(dir, "RTPLAN.dcm")

  private def fluenceToFFF(): Unit = {
    def setFFF(attr: Attribute): Unit = {
      attr.removeValues()
      attr.addValue("FFF")
    }
    DicomUtil.findAllSingle(plan, TagByName.FluenceMode).foreach(setFFF)
  }

  private def setNominalBeamEnergy(nbe: Double): Unit = {
    def setNBE(attr: Attribute): Unit = {
      attr.removeValues()
      attr.addValue(nbe)
    }
    DicomUtil.findAllSingle(plan, TagByName.NominalBeamEnergy).foreach(setNBE)
  }

  def main(args: Array[String]): Unit = {

    // fluenceToFFF()
    setNominalBeamEnergy(10.0)
    DicomUtil.writeAttributeListToFile(plan, planOutFile, "AQA")
    println(s"Wrote new plan at: $planOutFile")
  }

}
