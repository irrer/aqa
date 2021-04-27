package org.aqa.db

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Db.driver.api._


/**
 * This class is for migrating to the version of the database where a DicomSeries has a procedurePK.
 *
 * Once migration is complete, this code will be obsolete.
 */
object IdentifyProcedureOfPlan {

  def identifyProcedureOfPlan(plan: AttributeList): Option[Procedure] = {

    val beamNames = DicomUtil.findAllSingle(plan, TagByName.BeamName).map(_.getSingleStringValueOrEmptyString).mkString(" || ")

    val proc = 0 match {
      case _ if beamNames.toUpperCase().contains("FLOOD") || beamNames.toUpperCase().contains("J18G0") => Procedure.ProcOfPhase2
      case _ if beamNames.toUpperCase().contains("MMSEP")                                              => Procedure.ProcOfLOC
      case _ if beamNames.toUpperCase().contains("CBCT")                                               => Procedure.ProcOfBBbyEPID
      case _ if beamNames.toUpperCase().contains("BANK")                                               => Procedure.ProcOfMlcQa
      case _ =>
        None
    }
    proc
  }

  def main(args: Array[String]): Unit = {
    DbSetup.init
    println("\n\n\n\nIdentifyProcedureOfPlan Starting.  Use parameter DoIt to change DB.")

    val DoIt = {
      args.nonEmpty && args.head.equals("DoIt")
    }
    println("DoIt: " + DoIt)

    val action = for {
      planPK <- DicomSeries.query.filter(_.modality === "RTPLAN").map(_.dicomSeriesPK)
    } yield planPK

    val planPkList = Db.run(action.result)

    def procedureOfPlan(planPK: Long): Unit = {
      val ds = DicomSeries.get(planPK).get
      val al = ds.attributeListList.head

      val beamNameList = DicomUtil.findAllSingle(al, TagByName.BeamName).map(_.getSingleStringValueOrEmptyString)
      val numberOfBeams = beamNameList.size
      val beamNames = beamNameList.mkString(" || ")

      def show(name: String): Unit = {
        val procedure = identifyProcedureOfPlan(al)
        if (DoIt && procedure.isDefined) {
          val ds2 = ds.copy(procedurePK = procedure.get.procedurePK)
          ds2.insertOrUpdate()
          println("DidIt  " + planPK.formatted("%6d") + "  " + name.formatted("%-12s") + " number of beams:" + numberOfBeams.formatted("%3d") + " : " + beamNames)
        } else
          println("NotDid " + planPK.formatted("%6d") + "  " + name.formatted("%-12s") + " number of beams:" + numberOfBeams.formatted("%3d") + " : " + beamNames)
      }

      0 match {
        case _ if beamNames.toUpperCase().contains("FLOOD") || beamNames.toUpperCase().contains("J18G0") => show("Phase2")
        case _ if beamNames.toUpperCase().contains("MMSEP")                                              => show("LOC")
        case _ if beamNames.toUpperCase().contains("CBCT")                                               => show("DailyQA")
        case _ if beamNames.toUpperCase().contains("BANK")                                               => show("LeafGapSkew")
        case _ =>
          show("unknown")
          println("\n\n" + DicomUtil.attributeListToString(al) + "\n\n")
      }
    }
    println("\n\nNumber of plans: " + planPkList.size)
    planPkList.foreach(procedureOfPlan)
  }
}
