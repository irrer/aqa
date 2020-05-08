package org.aqa.run

import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.db.Machine

trait RunTrait[RunReqT] {
  def run(valueMap:  ValueMapT, request: Request, response: Response): Unit;
  val procedureName: String;
  val procedureUrl: String;
  def makeRunReq(alList: Seq[AttributeList]): RunReqClass;
  def validate(valueMap: ValueMapT, request: Request, response: Response): Either[StyleMapT, RunReqT];
  def getMachine(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Machine];
}