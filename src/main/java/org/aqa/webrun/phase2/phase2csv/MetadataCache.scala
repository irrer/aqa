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

package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.EPID
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.db.OutputNote
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.Logging
import org.aqa.Util

/**
  * Container for getting names/ids of institutions and machine information.
  */
class MetadataCache extends Logging {

  logger.info("Creating MetadataCache ...")
  private val start = System.currentTimeMillis()

  /** Map of machines by machinePK. */
  val machineMap: Map[Long, Machine] = Machine.list.sortBy(_.institutionPK).map(m => (m.machinePK.get, m)).toMap

  /** Map to retrieve anonymized name of institution by institutionPK. */
  val institutionNameMap: Map[Long, String] = Institution.list.map(i => (i.institutionPK.get, i.name)).sortBy(_._2).toMap

  /** Map of machine types and what they are called. */
  val machineTypeMap: Map[Long, String] = MachineType.list.map(mt => (mt.machineTypePK.get, mt.toName)).toMap

  /** Map of collimator types. */
  val collimatorTypeMap: Map[Long, String] = MultileafCollimator.list.map(c => (c.multileafCollimatorPK.get, c.model)).toMap

  /** Map of EPID types. */
  val epidTypeMap: Map[Long, String] = EPID.list.map(e => (e.epidPK.get, e.model)).toMap

  /** Map of users. */
  val userMap: Map[Long, String] = User.list.map(u => (u.userPK.get, u.id)).toMap

  /** Map of procedures. */
  val procedureMap: Map[Long, Procedure] = Procedure.list.map(p => (p.procedurePK.get, p)).toMap

  /**
   * Get the procedure PK in a safe way.
   * @param procedure For this procedure.
   * @return PK, or -1 on failure.
   */
  private def getProc(procedure: Option[Procedure]): Long = {
    if (procedure.isDefined && procedure.get.procedurePK.isDefined)
      procedure.get.procedurePK.get
    else
      -1
  }

  val phase2ProcedurePK: Long = getProc(Procedure.ProcOfPhase2)

  val phase3ProcedurePK: Long = getProc(Procedure.ProcOfPhase3)

  val gapSkewProcedurePK: Long = getProc(Procedure.ProcOfGapSkew)

  val focalSpotProcedurePK: Long = getProc(Procedure.ProcOfFocalSpot)

  val noteMap: Map[Long, String] = OutputNote.list().map(n => (n.outputPK, n.contentAsText)).toMap

  private val elapsed = System.currentTimeMillis() - start
  logger.info("Created MetadataCache.  Elapsed time: " + Util.elapsedTimeHumanFriendly(elapsed))
}

/**
  * Keep an application-wide copy of the metadata.
  */
object MetadataCache extends Logging {

  /** cache */
  private val cache: scala.collection.mutable.ArrayBuffer[MetadataCache] = scala.collection.mutable.ArrayBuffer()

  /** Invalidate the cache after this much time (in ms) to keep it up to date. */
  private val timeoutInterval_ms: Long = 10 * 60 * 1000

  /**
    * Get the cache entry.  If it is empty, then construct it.
    * @return cache
    */
  def get: MetadataCache = {
    val timeout = System.currentTimeMillis() - timeoutInterval_ms
    if (cache.nonEmpty && (cache.head.start > timeout)) {
      cache.head
    } else {
      cache.clear
      logger.info("refreshing MetadataCache cache.")
      cache.append(new MetadataCache)
      cache.head
    }
  }

  /**
    * Mark cache as invalid.
    * TODO find all points in the application where the cache should be invalidated.  This would be better than a timeout.
    */
  def invalidate(): Unit = {
    cache.clear
  }

}
