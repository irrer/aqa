package org.aqa.run

import org.aqa.db.Output
import scala.collection.mutable.HashMap
import scala.sys.process._
import org.restlet.Request

/**
 * Keep track of web clients that are waiting for procedures that have not yet terminated or
 * created output files.  This allows redirection to the output file when it becomes available,
 * yet later also view the summary.
 *
 */
object OutputPending {

  // Map of clients that initially asked for output when the output was not yet available.
  private val pendingClients = new HashMap[String, Output]()

  private def add(op: OutputPending): Unit = pendingClients.synchronized({ pendingClients.put(op.clientId, op.output) })

  def get(clientId: String) = pendingClients.synchronized({ pendingClients.get(clientId) })

  def remove(clientId: String) = pendingClients.synchronized({ pendingClients.remove(clientId) })

  def list = pendingClients.synchronized({ pendingClients.map(op => new OutputPending(op._1, op._2)) })

  private def makeUniqueClientId(request: Request): String = {
    val clientInfo = request.getClientInfo
    clientInfo.getAddress + clientInfo.getPort + System.currentTimeMillis
  }

  def add(request: Request, output: Output): String = {
    val op = new OutputPending(makeUniqueClientId(request), output)
    add(op)
    op.clientId
  }
}

class OutputPending(val clientId: String, val output: Output);