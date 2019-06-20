package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import java.io.File
import java.io.FileInputStream
import org.restlet.data.Status
import org.restlet.data.MediaType
import org.aqa.Util
import org.restlet.representation.ByteArrayRepresentation

class MainIndex(staticDir: File) extends Restlet {

  private val index = {
    val file = new File(staticDir, "index.html")
    new String(Util.readBinaryFile(file).right.get)
  }

  private val favicon = {
    val file = new File(new File(staticDir, "images"), "favicon.ico")
    val bytes = Util.readBinaryFile(file).right.get
    val representation = new ByteArrayRepresentation(bytes, MediaType.IMAGE_ICON, bytes.size)
    representation.setMediaType(MediaType.IMAGE_ICON)
    representation
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    response.setStatus(Status.SUCCESS_OK)

    //    val ref = request.getResourceRef.getRemainingPart.toLowerCase
    //
    //    if (ref.contains("favicon.ico") && (!ref.contains("/static/"))) {
    //      //response.setEntity(favicon) // can not get this working.  Firefox complains:
    //      // The image "http://localhost/favicon.ico" cannot be displayed because it contains errors.
    //
    //      response.redirectSeeOther("/static/images/favicon.ico?")
    //    } else
    response.setEntity(index, MediaType.TEXT_HTML)

    //response.setStatus(Status.SUCCESS_OK)
  }
}
