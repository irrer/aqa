package org.aqa

import edu.umro.EventNetClient.AgentIdentification
import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig
import org.aqa.webrun.wl.EventWLQASRSDone

object AQAEventNetClient extends Logging {

  private val defaultAgentIdentification: AgentIdentification = new AgentIdentification("AQA")

  private var eventNetConfig: Option[EventNetClientConfig] = None

  private var eventNetClient: Option[EventNetClient] = None

  def agentIdentification: AgentIdentification = {
    if (eventNetClient.isDefined)
      eventNetClient.get.agentIdentification
    else defaultAgentIdentification
  }

  private def makeEventNetConfig: Option[EventNetClientConfig] = {
    Some(
      new EventNetClientConfig( //
        Broker = Config.AMQPBrokerHost.get,
        Port = Config.AMQPBrokerPort.get.toInt,
        Exchange = "gbtopic", // standard EventNet exchange
        AdminExchange = "admintopic",
        RoutingKeyPrefix = ""
      )
    )
  }

  private def makeEventNetClient: Option[EventNetClient] = {
    if (Config.AMQPBrokerHost.isDefined && Config.AMQPBrokerPort.isDefined) {
      try {
        val enc = new EventNetClient(config = eventNetConfig.get, serviceName = "AQA", channelLimit = 5, restartDelayMs = 10 * 1000)
        logger.info("Constructed AQA EventNetClient with config: " + eventNetConfig.toString)
        Some(enc)
      } catch {
        case t: Throwable =>
          logger.error(s"Unexpected exception.  Unable to set up connection to AMQP for EventNet: ${fmtEx(t)}")
          None
      }
    } else
      None
  }

  def sendEventWLQASRSDone(event: EventWLQASRSDone): Unit = {
    val msg = event.toText
    if (eventNetClient.isDefined) {
      logger.info(s"Sending EventNet message. Exchange:${eventNetConfig.get.Exchange}   RoutingKeyPrefix: ${eventNetConfig.get.RoutingKeyPrefix}:\n$msg")
      eventNetClient.get.sendEvent(eventNetConfig.get.Exchange, eventNetConfig.get.RoutingKeyPrefix + "Aria.Event.EventWLQASRSDone", msg.getBytes)
    } else
      logger.info(s"EventNet is not configured.  Message not sent:\n$msg")
  }

  def init(): Unit = {
    if (Config.AMQPBrokerHost.isDefined) {
      eventNetConfig = makeEventNetConfig
      eventNetClient = makeEventNetClient
      logger.info(s"EventNet configured: AMQP Host: ${Config.AMQPBrokerHost.get}    AMQP Port: ${Config.AMQPBrokerPort.get}")
    } else
      logger.info(s"EventNet not configured.  These are parameters are not set up in the XML config file: AMQPBrokerHost AMQPBrokerPort")
  }

}
