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
    val representation = new ByteArrayRepresentation(bytes, MediaType.IMAGE_ICON)
    representation
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    if (request.getResourceRef.getRemainingPart.toLowerCase.endsWith("favicon.ico"))
      response.setEntity(favicon)
    else
      response.setEntity(index, MediaType.TEXT_HTML)

    response.setStatus(Status.SUCCESS_OK)
  }
}
