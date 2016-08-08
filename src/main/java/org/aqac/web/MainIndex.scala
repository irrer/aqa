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

class MainIndex(staticDir: File) extends Restlet {

    private val content = {
        val file = new File(staticDir, "index.html")
        val text: Array[Byte] = new Array[Byte](file.length.toInt)
        (new FileInputStream(file)).read(text)
        text
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity(new String(content), MediaType.TEXT_HTML)
    }
}
