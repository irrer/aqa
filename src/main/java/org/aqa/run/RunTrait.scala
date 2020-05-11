package org.aqa.run

import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.Logging
import org.aqa.web.WebUtil.WebForm
import org.aqa.webrun.ExtendedData
import java.sql.Timestamp
import org.aqa.db.Procedure

trait RunTrait[RunReqClassType] extends Logging {
  def run(extendedData: ExtendedData, runReq: RunReqClassType): ProcedureStatus.Value;
  def redo(extendedData: ExtendedData, runReq: RunReqClassType): ProcedureStatus.Value;
  val procedureName: String;
  val procedureUrl: String;
  def makeRunReq(alList: Seq[AttributeList]): RunReqClass;
  def validate(valueMap: ValueMapT, form: WebForm, request: Request, response: Response): Either[StyleMapT, RunReqClass];
  def getMachine(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Machine];
  def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String];
  def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp];
  def getProcedure: Procedure;
  def run(valueMap: ValueMapT, runReq: RunReqClass, response: Response)
}