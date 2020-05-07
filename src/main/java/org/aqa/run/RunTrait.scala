package org.aqa.run

import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status

trait RunTrait {
  def run(valueMap:  ValueMapT, request: Request, response: Response): Unit;
  val procedureName: String;
  val procedureUrl: String;
}