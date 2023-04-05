package org.aqa.customizeRtPlan

import org.aqa.customizeRtPlan.CustomizeRtPlan.LOCRtplanPair
import org.aqa.Config
import org.aqa.db.Machine
import org.aqa.db.MultileafCollimator
import org.aqa.Logging

object RtplanMaster extends Logging {

  private val patternPhase2 = ".*phase *2.*"
  private val patternPhase3 = ".*phase *3.*"
  private val patternDailyQA = ".*daily.*qa.*"
  private val patternGapSkew = ".*gap.*skew.*"
  private val patternWinstonLutz = ".*winston.*lutz.*"
  private val patternLOC = ".*loc.*"
  private val patternLOCBaseline = ".*baseline.*"
  private val patternLOCDelivery = ".*delivery.*"
  private val patternFocalSpot = ".*focal.*spot.*"

  /**
    * Get the template rtplan for the given machine matching the given pattern.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatiblePlanForMachine(machine: Machine, pattern: String): Seq[Config.PlanFileConfig] = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.PlanFileList.filter(pf =>
      pf.procedure.toLowerCase.matches(pattern) &&
        pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) &&
        pf.collimatorModel.equalsIgnoreCase(collimator.model)
    )
    planFile
  }

  /**
    * Get the template rtplan for the given machine for Phase 2.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatiblePhase2PlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternPhase2).headOption
  }

  /**
    * Get the template rtplan for the given machine for Phase 3.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatiblePhase3PlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternPhase3).headOption
  }

  /**
    * Get the template rtplan for the given machine for Daily QA.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleDailyQAPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternDailyQA).headOption
  }

  /**
    * Get the template rtplan for the given machine for Gap Skew.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleGapSkewPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternGapSkew).headOption
  }

  /**
    * Get the template rtplan for the given machine for Focal Spot.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleFocalSpotPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternFocalSpot).headOption
  }

  /**
    * Get the template rtplan for the given machine for Winston Lutz.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleWinstonLutzPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternWinstonLutz).headOption
  }

  /**
    * Get the template rtplans for the given machine for LOC.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatibleLocPlanPairForMachine(machine: Machine): Option[LOCRtplanPair] = {
    val planList = getCollimatorCompatiblePlanForMachine(machine, patternLOC) // gets both baseline and delivery files

    val baseline = planList.find(pf => pf.procedure.toLowerCase.matches(patternLOCBaseline))
    val delivery = planList.find(pf => pf.procedure.toLowerCase.matches(patternLOCDelivery))
    val locRtplanPair = (baseline, delivery) match {
      case (Some(b), Some(d)) if b.dicomFile.attributeList.isDefined && d.dicomFile.attributeList.isDefined => Some(LOCRtplanPair(b, d))
      case _                                                                                                => None
    }
    locRtplanPair
  }

}
