package org.aqac.web

import org.restlet.Application
import org.restlet.Restlet
import org.restlet.Component
import org.aqac.Logging._
import java.io.File
import edu.umro.RestletUtil.RestletHttps
import edu.umro.RestletUtil.ExpiresLaterFilter
import org.restlet.routing.Router
import org.aqac.Config
import org.restlet.routing.Template
import org.restlet.data.Protocol
import org.restlet.resource.Directory
import edu.umro.util.Utility
import edu.umro.util.General

object WebServer {
    def STATIC_CONTENT_DIR = new File(Config.DataDirectory, "static")
}

class WebServer extends Application {

    private val component = new Component

    private def addHttps: Unit = {
        val status = RestletHttps.addHttps(component, Config.HTTPSPort, Config.JavaKeyStoreFileList, List(Config.JavaKeyStorePassword))
        if (status.isLeft) {
            logSevere("Unable to use HTTPS.  Error: " + status.left.get)
        }
        else {
            logInfo("Using keystore file " + status.right.get.keyStoreFile.getAbsolutePath)
        }
    }

    private def routeDir(router: Router, dir: File): Unit = {
        val uri = ("file:///" + dir.getCanonicalPath).replace('\\', '/') + "/"
        val directory = new Directory(getContext.createChildContext, uri)
        directory.setListingAllowed(true)
        val relativeName = dir.getAbsolutePath.substring(Config.DataDirectory.getCanonicalPath.size).replace('\\', '/')

        if (true) {
            // TODO why does this not work?  The browser reloads this content anyway.
            val interval: Long = 35 * 24 * 60 * 60 * 1000.toLong
            val expiresOneHourLaterFilter = new ExpiresLaterFilter(getContext, interval, directory)

            router.attach(relativeName, expiresOneHourLaterFilter)
        }
        else
            router.attach(relativeName, directory)
    }

    /**
     * Directory containing the definitive static files.
     */
    private val installDir: File = {
        val local = new File("""src\main\resources\static""") // if running in the development system
        if (local.isDirectory) local
        else new File(WebServer.STATIC_CONTENT_DIR.getName)
    }

    /**
     * If the static directory is different from the install directory, then delete it
     * and copy the latest install directory.
     */
    private def initStaticContentDir = {
        if (!Utility.compareFolders(installDir, WebServer.STATIC_CONTENT_DIR)) {
            logInfo("Updating directory " + WebServer.STATIC_CONTENT_DIR.getAbsolutePath + " from " + installDir.getAbsolutePath)
            Utility.deleteFileTree(WebServer.STATIC_CONTENT_DIR)
            Utility.copyFileTree(installDir, WebServer.STATIC_CONTENT_DIR)
        }
    }

    /**
     * Standard Restlet override defines how to serve web pages.
     */
    override def createInboundRoot: Restlet = {
        val router = new Router(getContext.createChildContext)
        router.setDefaultMatchingMode(Template.MODE_STARTS_WITH)

        component.getClients.add(Protocol.FILE)

        routeDir(router, WebServer.STATIC_CONTENT_DIR)

        val restletList: List[Restlet] = List(
            new InstitutionUpdate,
            new InstitutionList,
            new MachineTypeUpdate,
            new MachineList,
            new MachineUpdate,
            new MachineTypeList,
            new UserUpdate,
            new UserList,
            new ProcedureUpdate,
            new ProcedureList,
            new RunWinstonLutz)

        restletList.map(r => router.attach(WebUtil.pathOf(r.getClass.getName), r))

        router.attach("", new MainIndex)
        /*
        router.attach("", new WebHome)
        val authentication = initAuthentication(router)

        val auditFilter = new AuditFilter(getContext)
        auditFilter.setNext(authentication)
        
        auditFilter
        */
        router // TODO should authenticate
    }

    /**
     * Initialize and start the service.
     */
    private def init: Unit = {
        addHttps
        initStaticContentDir
        // component.getServers().add(Protocol.HTTP, 80) // TODO remove.  For testing only
        // authenticationHealthCheck
        component.getDefaultHost.attach(this)
        logInfo("Starting web service on port: " + Config.HTTPSPort)
        component.start
        // setWebLogLevel
        while (!component.isStarted) {
            println("waiting ...") // TODO remove
            Thread.sleep(100)
        }
    }

    // constructor code
    init

}