/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


import org.restlet.Application
import org.restlet.Restlet
import org.restlet.Component
import org.restlet.routing.Router
import org.restlet.routing.Template
import org.restlet.data.Protocol

object WebServ extends Application {

    val HTTP_PORT = 9050

    private val component = new Component

    /**
     * Standard Restlet override defines how to serve web pages.
     */
    override def createInboundRoot: Restlet = {
        val router = new Router(getContext.createChildContext)
        router.setDefaultMatchingMode(Template.MODE_STARTS_WITH)

        component.getClients.add(Protocol.FILE)

        router.attach("", new Upload)

        router
    }

    private def addHttp = {
        println("Using port " + HTTP_PORT)
        component.getServers().add(Protocol.HTTP, HTTP_PORT)
    }

    /*
    override def handle(request: Request, response: Response): Unit = {
        println("WebServ request: " + request)
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity("Upload : " + (new java.io.File(???)), MediaType.TEXT_PLAIN)
    }
    */

    class Upload extends Restlet {
        /*
        override def handle(request: Request, response: Response): Unit = {
            println("Upload request: " + request)
            response.setStatus(Status.SUCCESS_OK)
            response.setEntity("Upload : " + (new java.io.File(???)), MediaType.TEXT_PLAIN)
        }
    */
    }

    def main(args: Array[String]): Unit = {
        println("Starting...")
        addHttp
        component.getDefaultHost.attach(this)
        component.start

    }
}