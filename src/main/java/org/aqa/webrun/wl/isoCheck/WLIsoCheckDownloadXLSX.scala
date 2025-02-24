package org.aqa.webrun.wl.isoCheck

import edu.umro.ScalaUtil.FileUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.web.WebUtil
import org.aqa.AnonymizeUtil
import org.aqa.Util
import org.aqa.web.NotAuthorized
import org.aqa.Crypto
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.io.File

class WLIsoCheckDownloadXLSX extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt

      val output = Output.get(outputPK).get

      val machine = Machine.get(output.machinePK.get).get
      val institutionPK = machine.institutionPK

      val user = WebUtil.getUser(request).get

      if (WebUtil.userIsWhitelisted(request) || (user.institutionPK == institutionPK)) {

        val file = {
          val isoCheckDir = new File(output.dir, "WLIsoCheck")
          FileUtil.listFiles(isoCheckDir).find(f => f.getName.matches("IsoCheck.*.xlsx.encrypted")).head
        }

        val encryptedHex = Util.readTextFile(file).right.get
        val clearTextHex = AnonymizeUtil.decryptWithNonce(institutionPK, new String(encryptedHex))

        val clearTextBinary = Crypto.hexToByteArray(clearTextHex)

        val rep = new ByteArrayRepresentation(clearTextBinary)
        response.setEntity(rep)
        rep.setMediaType(MediaType.APPLICATION_MSOFFICE_XLSX)

        WebUtil.setDownloadName(response, file.getName.replaceAll(".encrypted$", ""))

        response.setStatus(Status.SUCCESS_OK)
      } else {
        response.redirectSeeOther(new NotAuthorized().pathOf)
      }

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
