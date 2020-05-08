package org.aqa.run

import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.OutputList
import edu.umro.util.Utility
import org.aqa.web.WebRunIndex
import org.restlet.data.Status

object RunProcedure extends Logging {

  private val runButtonName = "Run"
  private val cancelButtonName = "Cancel"

  def makeForm(runTrait: RunTrait[RunReqClass]) = {
    val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

    def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
      val action = runTrait.procedureUrl + "?" + name + "=" + name
      new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    val runButton = makeButton(runButtonName, true, ButtonType.BtnDefault)
    val cancelButton = makeButton(cancelButtonName, false, ButtonType.BtnDefault)

    val form = new WebForm(runTrait.procedureUrl, Some("BBbyEPID"), List(List(machineSelector), List(runButton, cancelButton)), 10)
    form
  }

  private def buttonIs(valueMap: ValueMapT, buttonName: String): Boolean = {
    val value = valueMap.get(buttonName)
    value.isDefined && value.get.toString.equals(buttonName)
  }

  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => Utility.deleteFileTree(dir)
      case _ => ;
    }
    WebRunIndex.redirect(response)
  }

  /**
   * Respond to the 'Run' button.
   */
  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response, runTrait: RunTrait[RunReqClass]) = {

    logger.info("Validating data")
    runTrait.validate(valueMap, request, response) match {
      case Left(errMap) => {
        logger.info("BBbyEPIDRun Bad request: " + errMap.keys.map(k => k + " : " + valueMap.get(k)).mkString("\n    "))
        makeForm(runTrait).setFormResponse(valueMap, errMap, runTrait.procedureName, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        if (isAwait(valueMap)) awaitTag.synchronized {
          run(valueMap, runReq, response)
        }
        else run(valueMap, runReq, response)
      }

    }

  }

  private def emptyForm(valueMap: ValueMapT, response: Response, runTrait: RunTrait[RunReqClass]): Unit = {
    makeForm(runTrait).setFormResponse(valueMap, styleNone, runTrait.procedureName, response, Status.SUCCESS_OK)
  }

  def handle(request: Request, response: Response, runTrait: RunTrait[RunReqClass]): Unit = {

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    val redo = valueMap.get(OutputList.redoTag)
    val del = valueMap.get(OutputList.deleteTag)

    try {
      0 match {
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
        case _ if buttonIs(valueMap, cancelButtonName) => cancel(valueMap, response)
        case _ if buttonIs(valueMap, runButtonName) => runIfDataValid(valueMap, request, response, runTrait)
        case _ => emptyForm(valueMap, response, runTrait)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }

  }

}