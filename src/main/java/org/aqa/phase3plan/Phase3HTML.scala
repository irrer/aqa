package org.aqa.phase3plan

import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.Machine
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.aqa.web.WebUtil._
class Phase3HTML extends Restlet with SubUrlRoot with Logging{

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach(): Unit =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if user.isEmpty => updateMach()
        case _ if machine.isEmpty => updateMach()
        case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()
        case _ if buttonIs(valueMap, backButton) => updateMach()

        case _ if makeList.exists(make => valueMap.contains(make.makeButton.label)) => validateAndMake(valueMap, response)

        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
