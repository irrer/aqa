package org.aqac.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import java.io.File
import java.io.FileInputStream
import org.restlet.data.Status
import org.restlet.data.MediaType

class MainIndex extends Restlet {

    private val content = {
        val file = new File(WebServer.STATIC_CONTENT_DIR, "index.html")
        val text: Array[Byte] = new Array[Byte](file.length.toInt)
        (new FileInputStream(file)).read(text)
        text
    }

    override def handle(request: Request, response: Response): Unit = {
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity(new String(content), MediaType.TEXT_HTML)

    }
}
