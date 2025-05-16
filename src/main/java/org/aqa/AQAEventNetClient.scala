package org.aqa

import edu.umro.EventNetClient.AgentIdentification
import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig

object AQAEventNetClient extends Logging {

  private val eventNetClient: Option[EventNetClient] = {
    if (Config.AMQPBrokerHost.isDefined && Config.AMQPBrokerPort.isDefined) {
      val eventNetConfig = new EventNetClientConfig(
        Broker = Config.AMQPBrokerHost.get,
        Port = Config.AMQPBrokerPort.get.toInt,
        Exchange = "gbtopic", // standard EventNet exchange
        AdminExchange = "admintopic",
        RoutingKeyPrefix = ""
      )
      try {
        val enc = new EventNetClient(eventNetConfig, "AQA", 10, 10 * 1000)
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

  val agentIdentification: AgentIdentification = {
    if (eventNetClient.isDefined)
      eventNetClient.get.agentIdentification
    else
      new AgentIdentification("AQA")
  }

  def send(amqpExchange: String, amqpRoutingKey: String, msg: String): Unit = {

    if (eventNetClient.isDefined) {
      logger.info(s"Sending EventNet message:\n$msg")
      eventNetClient.get.sendEvent(amqpExchange, amqpRoutingKey, msg.getBytes)
    } else
      logger.info(s"EventNet is not configured.  Message not sent:\n$msg")

  }

  def init(): Unit = {
    if (eventNetClient.isDefined) {
      eventNetClient.get.agentIdentification
      logger.info(s"EventNet configured: AMQP Host: ${Config.AMQPBrokerHost.get}    AMQP Port: ${Config.AMQPBrokerPort.get}")
    } else
      logger.info(s"EventNet not configured.  These are parameters are not set up in the XML config file: AMQPBrokerHost AMQPBrokerPort")
  }

}
