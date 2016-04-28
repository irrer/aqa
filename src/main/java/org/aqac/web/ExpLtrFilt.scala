package org.aqac.web

import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.Context
import java.util.Date
import org.restlet.routing.Filter

object ExpLtrFilt {
    val path = "/UserList"

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class ExpLtrFilt(context: Context, interval: Long) extends Filter(context) {

    private def setExpiration(response: Response): Unit = {
        val expirationDate = new Date(System.currentTimeMillis + interval)
        println("expirationDate: " + expirationDate) // TODO rm
        val ent = response.getEntity // TODO rm
        if (ent == null) // TODO rm
            println("badness") // TODO rm
        if (response.getEntity != null) response.getEntity.setExpirationDate(expirationDate)
    }
    
    override def beforeHandle(request: Request, response: Response): Int = {
        setExpiration(response)
        Filter.CONTINUE
    }

    override def afterHandle(request: Request, response: Response): Unit = {
        setExpiration(response)
    }
}
