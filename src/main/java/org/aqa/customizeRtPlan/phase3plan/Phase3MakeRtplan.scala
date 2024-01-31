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
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Logging
import org.restlet.Response

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Phase3MakeRtplan extends Logging {

  /**
    * A set of beams that must be delivered consecutively.  They may be delivered in any order, but the order does not matter.
    * @param beamList List of beams.  In most cases this will be only 1 beam.
    */
  private case class BeamSet(beamList: Seq[Beam]) {

    val size: Int = {
      beamList.size match {
        case 1    => 1 // 1 factorial
        case 2    => 2 // 2 factorial
        case size => throw new RuntimeException(s"Unsupported number of beams in beam set: $size")
      }
    }

    def firstGantryAngle: Double = beamList.head.gantryAngleList_deg.head
    def lastGantryAngle: Double = beamList.last.gantryAngleList_deg.last

    def firstMv: Double = beamList.head.beamEnergy.photonEnergy_MeV.get
    def lastMv: Double = beamList.last.beamEnergy.photonEnergy_MeV.get

    def firstMLCAngle: Double = beamList.head.colAngle_deg
    def lastMLCAngle: Double = beamList.last.colAngle_deg

    def firstFFF: Boolean = beamList.head.isFFF

    def lastFFF: Boolean = beamList.last.isFFF
  }

  private def isSelected(valueMap: ValueMapT, selection: Selection): Boolean = valueMap.contains(selection.htmlId)

  /**
    * Check to ensure that no beam must be used consecutively with another beam in more than one beam set.
    *
    * If this check fails, then it is because of a programming error, so throw an exception.
    *
    * @param distinctBeamSetList List of distinct beam sets to be delivered.
    */
  private def checkForConsecutiveUseConflict(distinctBeamSetList: Iterable[BeamSet]): Unit = {
    val beamList = distinctBeamSetList.flatMap(_.beamList)
    val distinctBeamList = beamList.groupBy(_.beamName).values.map(_.head)
    if (beamList.size != distinctBeamList.size) {
      val beamNameList = beamList.map(_.beamName).toList.diff(distinctBeamList.map(_.beamName).toList).distinct.sorted.mkString("\n")
      throw new RuntimeException(s"A beam appears in more than one beam set.  This makes it potentially impossible to optimize beam ordering.  Beams: $beamNameList")
    }
  }

  /**
    * The same BeamSet may be used by more than one procedure.  Make the list of BeamSets that are
    * distinct.  Distinct means they each have a different set of beams.
    *
    * @param beamSetList List of all beam sets.
    * @return List of distinct beam sets.
    */
  private def makeDistinctBeamSetList(beamSetList: Iterable[BeamSet]): Iterable[BeamSet] = {

    /** Make a signature unique to the BeamSet content.  This allows comparison of beam sets. */
    def beamSetSignature(beamSet: BeamSet): String = beamSet.beamList.map(_.beamName).sorted.mkString("\n")
    val distinctBeamSetList = beamSetList.groupBy(beamSetSignature).values.map(_.head)
    distinctBeamSetList
  }

  /**
    * Make a list of all beam sets to be delivered.  Check to ensure that no beam appears in more than one BeamSet.
    * @param selectedDistinctBeamList List of selected beams.
    * @param subProcedureList List of all sub-procedures
    * @return List of all beam sets to be delivered.
    */
  private def makeBeamSetList(selectedDistinctBeamList: Iterable[Beam], subProcedureList: SubProcedureList): Iterable[BeamSet] = {

    val selectedBeamSetList: Iterable[BeamSet] = {
      val allBeamSets = subProcedureList.subProcedureList.flatMap(sub => sub.consecutivelyDeliveredBeamSets.map(BeamSet))
      val selectedBeamNameList = selectedDistinctBeamList.map(_.beamName).toSet

      /** Return true if the beam set contains selected beams.  Throw exception if only some are selected.  */
      def hasSelectedBeam(beamSet: BeamSet): Boolean = {
        val numberOfBeamsSelected = beamSet.beamList.count(beam => selectedBeamNameList.contains(beam.beamName))
        val hasSelected = numberOfBeamsSelected match {
          case 0                                                   => false
          case _ if numberOfBeamsSelected == beamSet.beamList.size => true
          case _                                                   => throw new RuntimeException(s"BeamSet has some beams selected and some not.")
        }
        hasSelected
      }
      allBeamSets.filter(hasSelectedBeam)
    }

    val distinctBeamSetList = makeDistinctBeamSetList(selectedBeamSetList)

    checkForConsecutiveUseConflict(distinctBeamSetList)

    distinctBeamSetList
  }

  /*
  private def optimizeBeamSetByEnergy(beamSetList: Iterable[BeamSet]): Iterable[BeamSet] = {
    def energySignature(beamSet: BeamSet): String = {
      (0 until beamSet.size).map(p => s" ${beamSet.firstMv} ${beamSet.firstFFF} -> ${beamSet.lastMv} ${beamSet.lastFFF} ").mkString("\n")
    }

    // list of beam sets made from single beam BeamSets
    val aggregatedSingleBeamSetList = {
      // find all with the same energy signature
      val list = beamSetList.filter(_.size == 1).groupBy(energySignature).values
      list.map(setList => BeamSet(setList.map(_.beamList.head).toSeq))
    }

    val aggregatedMultiBeamSetList = {
      val list = beamSetList.filter(_.size == 1).groupBy(energySignature).values
      list.map(setList => BeamSet(setList.map(_.beamList.head).toSeq))
    }

    ???
  }
   */

  /*
  private def makeBeamListX(valueMap: ValueMapT, subProcedureList: SubProcedureList): Seq[Beam] = {
    val selectedList = subProcedureList.subProcedureList.flatMap(_.selectionList).filter(sel => isSelected(valueMap, sel))
    val selectedDistinctBeamList = selectedList.flatMap(_.beamList).groupBy(_.beamName).values.map(_.head)

    val beamSetListToBeDelivered = makeBeamSetList(selectedDistinctBeamList, subProcedureList)

    def gantryAngleUse(beamSet: BeamSet): String = {
      (0 until beamSet.size).map(p => s"${beamSet.firstGantryAngle} -> ${beamSet.firstGantryAngle}").mkString(" |||| ")
    }

    val beamSetListGroupedByGantryUse = beamSetListToBeDelivered.groupBy(gantryAngleUse).values.map(optimizeBeamSetByEnergy)

    val sortedBeamSet = beamSetListGroupedByGantryUse

    beamSetListToBeDelivered.groupBy(gantryAngleUse)

    ???
  }
   */

  /**
    * Calculate a 'cost' of delivering a set of beams in the given order.
    * @param beamSetList List of beams to be delivered.
    * @return Cost (in time) of delivery.  A larger number means more time.
    */
  private def fitnessOf(beamSetList: Seq[BeamSet]): Double = {
    val gantryScale = 1000000.0
    val mlcScale = 1.0
    val energyScale = 1.0

    /**
      * Rate the severity of the transition of one beam set to another.  The larger the number, the more time it takes.
      * @param a First beam delivered.
      * @param b Second beam delivered.
      * @return Transition rating.
      */
    def scoreTransition(a: BeamSet, b: BeamSet): Double = {
      val mlc = { if (a.lastMLCAngle == b.firstMLCAngle) 0 else 1 } * mlcScale
      val energy = {
        val aa = s"${a.lastMv} ${a.lastFFF}"
        val bb = s"${b.firstMv} ${b.firstFFF}"
        if (aa.equals(bb)) 0.0 else energyScale
      }

      mlc + energy
    }

    (1 until beamSetList.size).map(index => scoreTransition(beamSetList(index - 1), beamSetList(index))).sum
  }

  // for random mutation
  private val random = new Random()
  private def randD = random.nextDouble()

  private case class BeamSetList(list: Seq[BeamSet]) {
    val fitness: Double = fitnessOf(list)
  }

  private def mutate(rate: Double, bsList: BeamSetList): BeamSetList = {
    val beamSetList = bsList.list
    val newList1: Seq[BeamSet] =
      if ((randD * rate) < 1)
        beamSetList
      else {
        // Move a random number of elements to a different position in the list.
        // Also, randomly either keep moved list in the original order or reverse it.
        val lb = ListBuffer[BeamSet]()
        lb ++= beamSetList

        val sliceSize = (beamSetList.size * (randD / 2)).toInt
        val sliceOffset = (beamSetList.size * randD).toInt
        lb.remove(sliceOffset, sliceSize)

        val slice = lb.slice(sliceOffset, sliceOffset + sliceSize)
        if (randD < .5) slice.reverse

        val insertOffset = (lb.size * randD).toInt
        lb.insertAll(insertOffset, slice)

        lb
      }

    val newList2: Seq[BeamSet] = {
      if ((randD * rate) < 1)
        newList1
      else {
        val candidateList = beamSetList.indices.filter(i => beamSetList(i).size > 1)
        val toChange = (candidateList.size * randD).toInt
        val newBeamSet = BeamSet(beamSetList(toChange).beamList.reverse)
        val list = newList1.patch(toChange, Seq(newBeamSet), toChange)
        list
      }

    }
    BeamSetList(newList2)
  }

  /**
    * Reorder the given beam set list so as to optimize the delivery time.
    * @param beamSetList List of beam sets to be delivered.
    * @return
    */
  private def optimizeBeamSetByEnergy(beamSetList: Seq[BeamSet]): Seq[BeamSet] = {

    val start = System.currentTimeMillis()
    val timeout = start + 2000

    @tailrec
    def nextGeneration(generation: Int, mutationRate: Double, population: Seq[BeamSetList]): Seq[BeamSetList] = {
      // create the next generation by removing the least fit, and adding a mutated version of the most fit.
      val nextGen = {
        val mostFit = population.sortBy(_.fitness).take(population.size / 2)
        mostFit ++ mostFit.map(bsl => mutate(mutationRate, bsl))
      }

      val meanFitness = nextGen.map(_.fitness).sum / nextGen.size
      logger.info(
        s"Generation $generation" +
          s"    mutation rate: $mutationRate" +
          s"    best fitness: ${nextGen.minBy(_.fitness).fitness}" +
          s"    mean fitness: $meanFitness" +
          s"    elapsed ms: ${System.currentTimeMillis() - start}"
      )
      if ((timeout > System.currentTimeMillis()) && (mutationRate > 1))
        nextGeneration(generation + 1, mutationRate * 0.95, nextGen)
      else
        nextGen
    }

    val populationSize = 1000
    val initialMutationRate = 2.0

    val bsl = BeamSetList(beamSetList)

    val population = (0 until populationSize).map(_ => mutate(initialMutationRate, bsl))

    val finalPopulation = nextGeneration(0, initialMutationRate, population)

    finalPopulation.minBy(_.fitness).list
  }

  private def makeBeamList(valueMap: ValueMapT, subProcedureList: SubProcedureList): Seq[Beam] = {
    val selectedList = subProcedureList.subProcedureList.flatMap(_.selectionList).filter(sel => isSelected(valueMap, sel))
    val selectedDistinctBeamList = selectedList.flatMap(_.beamList).groupBy(_.beamName).values.map(_.head)

    val beamSetListToBeDelivered = makeBeamSetList(selectedDistinctBeamList, subProcedureList).toSeq

    def gantryAngleUse(beamSet: BeamSet): String = {
      s"${beamSet.firstGantryAngle} -> ${beamSet.lastGantryAngle}"
    }

    val beamSetListGroupedByGantryUse = beamSetListToBeDelivered.groupBy(gantryAngleUse).values.map(optimizeBeamSetByEnergy)

    def showGroup(list: Seq[BeamSet]): Unit = {
      val text = list.flatMap(_.beamList).map(_.toString).mkString("\n")
    }

    logger.info("Beams to be delivered\n" + beamSetListGroupedByGantryUse.map(showGroup).mkString("\n\n"))

    selectedDistinctBeamList.toSeq // TODO rm.  Should return optimized list.
  }

  private def makeDeliverableBeam(beam: Beam): AttributeList = {

    CustomizeRtPlanUtil.makeBeam(beam.beamEnergy, beam.prototypeBeam, BeamName = beam.beamName, BeamNumber = ???)
  }

  def make(valueMap: ValueMapT, response: Response, subProcedureList: SubProcedureList): Unit = {

    val beamList = makeBeamList(valueMap, subProcedureList)

  }
}
