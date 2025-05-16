package org.aqa.webrun.wl

import edu.umro.EventNetClient.OriginatingEvent
import org.aqa.run.ProcedureStatus
import org.aqa.AQAEventNetClient
import org.aqa.Logging

import java.util.Date
import scala.xml.Elem

class EventWLQASRSDone(PatientId: String, CareEventStart: Date, Status: ProcedureStatus.Value, NumberOfImages: Int, ReportURL: String, TreatmentMachine: String)
    extends OriginatingEvent(AQAEventNetClient.agentIdentification)
    with Logging {

  override val xml: Elem = {
    <EventWLQASRSDone xmlns='urn:EventWLQASRSDone'>
      <PatientId>{PatientId}</PatientId>
      <CourseId>NA</CourseId>
      <CourseSer>NA</CourseSer>
      <CareEventStart>{edu.umro.EventNetClient.Util.dateToText(CareEventStart)}</CareEventStart>
      <Status>{Status}</Status>
      <NumberOfImages>{NumberOfImages}</NumberOfImages>
      <ReportURL>{ReportURL}</ReportURL>
      <TreatmentMachine>{TreatmentMachine}</TreatmentMachine>
      {header.xml}
    </EventWLQASRSDone>
  }

  def send(): Unit = {
    try {

      val msg = this.toText

      AQAEventNetClient.send(amqpExchange = "", amqpRoutingKey = "", msg)
      logger.info(s"Sent EventNet message: $msg")
    } catch {
      case t: Throwable =>
        logger.error(s"Failed to send EventNet event: ${fmtEx(t)}")
    }

  }
}
