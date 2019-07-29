package org.aqa.webrun

import org.aqa.db.Procedure
import org.restlet.Restlet
import scala.collection.mutable.HashMap
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.WebUtil._
import org.aqa.web.ProcedureUpdate
import scala.xml.Elem
import org.aqa.web.Session
import org.aqa.webrun.phase2.Phase2
import org.aqa.webrun.bbByCBCT.BBbyCBCTRun

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
    val restlet = WebRun.getNext(request, response)
    if (restlet.isRight)
      restlet.right.get.handle(request, response)
    else {
      internalFailure(response, restlet.left.get)
      None
    }
  }
}

abstract class WebRunProcedure(procedure: Procedure) extends Restlet {
}

object WebRun {

  type ConstructInterfaceT = (Procedure) => WebRunProcedure

  private val interfaceList: Map[String, ConstructInterfaceT] = Map(
    ("LOCRun_1", procedure => new LOCRun_1(procedure)),
    ("Phase2", procedure => new Phase2(procedure)),
    ("BBbyCBCT", procedure => new BBbyCBCTRun(procedure)),
    ("LOCUploadBaseFiles_1", procedure => new LOCUploadBaseFiles_1(procedure)),
    ("UploadAndChooseMachine_1", procedure => new UploadAndChooseMachine_1(procedure)),
    ("WinstonLutz_1", procedure => new WinstonLutz_1(procedure)))

  /** Possible choices for procedure interfaces. */
  def interfaceChoices = interfaceList.map(nc => nc._1)

  /** List of procedurePK -> interface pairs that have been instantiated. */
  private val lookup = new HashMap[Long, Restlet]()

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

  private def get(procedurePK: Long): Either[String, Restlet] = {
    // it is important to synchronize this because even one web client often makes multiple overlapping calls, and we
    // do not want to instantiate multiple copies of the same thing.
    lookup.synchronized({
      val restlet = lookup.get(procedurePK)
      if (restlet.isEmpty) makeNewInterfaceInstance(procedurePK)
      else Right(restlet.get)
    })
  }

  def getNext(request: Request, response: Response): Either[String, Restlet] = {
    val procedurePK = request.getResourceRef.getLastSegment.replaceAll(".*_", "").toLong
    get(procedurePK)
  }
}
