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

package org.aqa.webrun

import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.aqa.webrun.LOC.LOCRun
import org.aqa.webrun.LOCBaseline.LOCBaselineRun
import org.aqa.webrun.bbByCBCT.BBbyCBCTRun
import org.aqa.webrun.bbByEpid.BBbyEPIDRun
import org.aqa.webrun.gapSkew.GapSkewRun
import org.aqa.webrun.machLog.MachLogRun
import org.aqa.webrun.phase2.Phase2
import org.aqa.webrun.phase3.Phase3
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet

/**
  * Web interface for running the procedures.
  *
  * Looks at the incoming request and determines which procedure restlet the client wants to
  * invoke.  If the restlet has not been instantiated, then do so, add it to the internal
  * list (WebRunProcedure.lookup) and return it.
  */

class WebRun extends Restlet {
  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val restlet = WebRun.getNext(request)
    if (restlet.isRight)
      restlet.right.get.handle(request, response)
    else {
      internalFailure(response, restlet.left.get)
    }
  }
}

abstract class WebRunProcedure(procedure: Procedure) extends Restlet {}

object WebRun {

  type ConstructInterfaceT = Procedure => WebRunProcedure

  private val interfaceList: Map[String, ConstructInterfaceT] = Map(
    ("LOCRun_1", procedure => new LOCRun_1(procedure)), // old LOC delivery
    ("Phase2", procedure => new Phase2(procedure)),
    ("Phase3", procedure => new Phase3(procedure)),
    ("BBbyCBCT", procedure => new BBbyCBCTRun(procedure)),
    ("BBbyEPID", procedure => new BBbyEPIDRun(procedure)),
    ("LOCUploadBaseFiles_1", procedure => new LOCUploadBaseFiles_1(procedure)), // old LOC baseline
    ("GapSkewRun", procedure => new GapSkewRun(procedure)),
    ("UploadAndChooseMachine_1", procedure => new UploadAndChooseMachine_1(procedure)),
    ("WinstonLutz_1", procedure => new WinstonLutz_1(procedure)),
    ("MachineLog", procedure => new MachLogRun(procedure)),
    ("LOC", procedure => new LOCRun(procedure)),
    ("LOCBaseline", procedure => new LOCBaselineRun(procedure))
  )

  /** Possible choices for procedure interfaces. */
  def interfaceChoices: Iterable[String] = interfaceList.keys

  /** List of procedurePK -> interface pairs that have been instantiated. */
  private val lookup = new scala.collection.mutable.HashMap[Long, Restlet]()

  private def interfaceNotFound(procedure: Procedure): String = {
    "The selected procedure " + procedure.fullName + " specifies that it needs the web interface " +
      procedure.webInterface + ", but that interface does not exist.  The known interfaces are: " +
      interfaceList.foldLeft("")((t, i) => t + "\n    " + i)
  }

  /**
    * Make a new interface Restlet for handling the running of the given
    * procedure.  On failure, return a String explaining the problem.
    */
  private def makeNewInterfaceInstance(procedurePK: Long): Either[String, Restlet] = {
    val procedure = Procedure.get(procedurePK)
    if (procedure.isDefined) {
      val constructor = interfaceList.get(procedure.get.webInterface)
      if (constructor.isDefined) {
        val newRestlet = constructor.get(procedure.get)
        lookup.put(procedurePK, newRestlet)
        Right(newRestlet)
      } else Left(interfaceNotFound(procedure.get))
    } else {
      Left("No such procedure with procedurePK " + procedurePK + " in database.")
    }
  }

  /**
    * Given a procedure, get a restlet to handle it.  On success return Right, on failure return Left(failure message).
    */
  def get(procedurePK: Long): Either[String, Restlet] = { // TODO should return type RunTrait instead of Restlet
    // it is important to synchronize this because even one web client often makes multiple overlapping calls, and we
    // do not want to instantiate multiple copies of the same thing.
    lookup.synchronized({
      val restlet = lookup.get(procedurePK)
      if (restlet.isEmpty) makeNewInterfaceInstance(procedurePK)
      else Right(restlet.get)
    })
  }

  def getNext(request: Request): Either[String, Restlet] = {
    val procedurePK = request.getResourceRef.getLastSegment.replaceAll(".*_", "").toLong
    get(procedurePK)
  }
}
