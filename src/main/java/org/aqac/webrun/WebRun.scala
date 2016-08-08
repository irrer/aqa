package org.aqac.webrun

import org.aqac.db.Procedure
import org.restlet.Restlet
import scala.collection.mutable.ArrayBuffer
import org.restlet.Request
import org.restlet.Response
import org.aqac.web.WebUtil._
import org.aqac.web.ProcedureUpdate
import org.aqac.web.WebServer

/**
 * Web interface for running a procedure.
 */
abstract class WebRun(val procedure: Procedure) extends Restlet {
    //def handle(valueMap: ValueMapT, request: Request, response: Response): Unit
}

class XWebRunInvoke extends Restlet {
    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)

        val procedurePK = valueMap.get(ProcedureUpdate.procedurePKTag).get.toLong
        val procedure: Procedure = Procedure.get(procedurePK).get

        XWebRun.getWebRun(procedure).handle(request, response)
    }
}

object XWebRun {

    private val webList: List[Class[WebRun]] forSome { type WebRun } = List(
        WinstonLutz_1.getClass)

    private val procList = ArrayBuffer[WebRun]();

    private val runProcedureList: Seq[WebRun] = {
        //Procedure.list.map(p => 
        Procedure.list.filter { p => p.name.equalsIgnoreCase("Winston-Lutz") }.map(wl => new WinstonLutz_1(wl)) //  TODO put back in
    }

    def getWebRun(procedure: Procedure): WebRun = {
        procList.synchronized({
            //val wrx = procList.find(p => p.procedure.name.equals(procedure.name) && p.procedure.version.equals(procedure.version))  TODO remove
            val pk = procedure.procedurePK.get
            val wr = procList.find(p => p.procedure.procedurePK.get == pk)
            if (wr.isEmpty) {
                val w = new WinstonLutz_1(procedure) // TODO should go through webList
                // TODO how to convert class name to nice name
                // val className = WinstonLutz_1.getClass.getName.replace('$', ' ').trim.replaceAll(".*\\.", "")      // TODO rm
                //attachToRouter(w)
                procList += w
                w
            }
            else wr.head
        })
    }

    def loadProcedures: Unit = Procedure.list.map(p => getWebRun(p))

}
