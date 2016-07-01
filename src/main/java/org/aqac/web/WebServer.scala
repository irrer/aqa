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
import org.aqac.db.Procedure
import org.aqac.webrun.WinstonLutz_1
import org.aqac.webrun.WebRun

object WebServer {
    val staticDirName = "static"
    def DATA_DIR = new File(Config.DataDir, "data")

    val dataDirBaseUrl = "/data"
    val tmpDirBaseUrl = "/tmp"
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

    private def getRelativeName(dir: File): String = dir.getAbsolutePath.substring(Config.DataDir.getCanonicalPath.size).replace('\\', '/')

    private def makeDirectory(dir: File): Directory = {
        val uri = ("file:///" + dir.getCanonicalPath).replace('\\', '/') + "/"
        val directory = new Directory(getContext.createChildContext, uri)
        directory.setListingAllowed(true)
        directory
    }

    private def routeStaticDir(router: Router): Unit = {
        val directory = makeDirectory(staticDir)
        router.attach("/" + WebServer.staticDirName, directory)

        /*
        if (false) {
            // TODO why does this not work?  The browser reloads this content anyway.
            val interval: Long = 35 * 24 * 60 * 60 * 1000.toLong
            val expiresOneHourLaterFilter = new ExpiresLaterFilter(getContext, interval, directory)

            router.attach("/" + WebServer.staticDirName, expiresOneHourLaterFilter)
        }
        */
    }

    /**
     * Directory containing the definitive static files.
     */
    private val staticDir: File = {
        val locations = List(""".\""", """src\main\resources\""").map(name => new File(name + WebServer.staticDirName))

        val dirList = locations.filter(f => f.isDirectory)

        if (dirList.isEmpty) {
            val fileNameList = locations.foldLeft("")((t, f) => t + f.getAbsolutePath)
            val msg = "Unable to find static directory in " + fileNameList
            logSevere(msg)
            throw new RuntimeException(msg)
        }
        logInfo("Using static directory " + dirList.head.getAbsolutePath)
        dirList.head
    }

    /**
     * If the static directory is different from the install directory, then delete it
     * and copy the latest install directory.
     * private def initStaticContentDir = {
     * if (!Utility.compareFolders(installDir, WebServer.STATIC_CONTENT_DIR)) {
     * logInfo("Updating directory " + WebServer.STATIC_CONTENT_DIR.getAbsolutePath + " from " + installDir.getAbsolutePath)
     * Utility.deleteFileTree(WebServer.STATIC_CONTENT_DIR)
     * Utility.copyFileTree(installDir, WebServer.STATIC_CONTENT_DIR)
     * }
     * }
     */

    def attachProcedures(router: Router): Unit = {
        Procedure.list.map(p => {
            router.attach(p.webUrl, new WinstonLutz_1(p))
        })
    }

    /**
     * Standard Restlet override defines how to serve web pages.
     */
    override def createInboundRoot: Restlet = {
        val router = new Router(getContext.createChildContext)
        router.setDefaultMatchingMode(Template.MODE_STARTS_WITH)

        component.getClients.add(Protocol.FILE)

        routeStaticDir(router)

        router.attach(WebServer.dataDirBaseUrl, makeDirectory(WebServer.DATA_DIR))
        router.attach(WebServer.tmpDirBaseUrl, makeDirectory(Config.tmpDir))

        val adminRestletList: Seq[Restlet] = Seq(
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
            new OutputList,
            new WebRunIndex)

        adminRestletList.map(r => router.attach("/admin" + WebUtil.pathOf(r.getClass.getName), r))

        val viewOutput = new ViewOutput
        router.attach(WebUtil.pathOf(viewOutput.getClass.getName), viewOutput)

        attachProcedures(router)

        router.attach("", new MainIndex(staticDir))
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
    def init: Unit = {
        addHttps
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

    init
}