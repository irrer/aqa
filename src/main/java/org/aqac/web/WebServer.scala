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
import org.aqac.webrun.MaxLeafGap_1
import org.aqac.webrun.WebRun
import org.restlet.security.ChallengeAuthenticator
import org.restlet.data.ChallengeScheme
import org.restlet.security.MapVerifier
import org.restlet.Context
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.ChallengeRequest
import org.restlet.data.ChallengeResponse
import org.restlet.routing.Filter
import org.aqac.db.User
import org.aqac.db.UserRole
import org.restlet.routing.TemplateRoute

object WebServer {
    val staticDirName = "static"

    val challengeScheme = ChallengeScheme.HTTP_BASIC

    lazy val dataDir = new File(Config.DataDir, "data")

    val dataDirBaseUrl = "/data"
    val tmpDirBaseUrl = "/tmp"

    def urlOfDataPath(filePath: String): String = {
        (dataDirBaseUrl + "/" + filePath.replace('\\', '/')).replaceAll("///*", "/")
    }

    def urlOfDataFile(file: File): String = urlOfDataPath(fileToDataPath(file))

    def fileOfDataPath(filePath: String): File = new File(dataDir, filePath)

    def fileToDataPath(file: File): String = file.getAbsolutePath.substring(dataDir.getAbsolutePath.size)
}

class WebServer extends Application {

    private lazy val component = new Component

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

    /**
     * Directory containing the definitive static files.
     */
    private lazy val staticDirFile: File = {
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

    private def attach(router: Router, url: String, restlet: Restlet): Unit = {
        val name = restlet.getClass.getName.replaceAll(".*\\.", "")
        val rootRef = if (restlet.isInstanceOf[Directory]) (" : " + restlet.asInstanceOf[Directory].getRootRef) else ""
        logInfo("attaching router to " + url + " ==> " + name + rootRef)
        router.attach(url, restlet)
    }

    private def attachProcedures(router: Router): Unit = {
        Procedure.list.map(p => {
            // TODO this should not be so hard-codey
            if (p.webInterface.toLowerCase.contains("winston")) attach(router, p.webUrl, new WinstonLutz_1(p))
            if (p.webInterface.toLowerCase.contains("maxleaf")) attach(router, p.webUrl, new MaxLeafGap_1(p))
        })
    }

    private lazy val router = new Router(getContext.createChildContext)

    private lazy val staticDir = makeDirectory(staticDirFile)

    /*
        if (false) {
            // TODO why does this not work?  The browser reloads this content anyway.
            val interval: Long = 35 * 24 * 60 * 60 * 1000.toLong
            val expiresOneHourLaterFilter = new ExpiresLaterFilter(getContext, interval, staticDir)

            attach(router, "/" + WebServer.staticDirName, expiresOneHourLaterFilter)
        }
        */

    private lazy val dataDir = makeDirectory(WebServer.dataDir)

    private lazy val tmpDir = makeDirectory(Config.tmpDir)

    private lazy val login = new Login
    
    private lazy val setPassword = new SetPassword
    
    

    private lazy val webRunIndex = new WebRunIndex

    private lazy val mainIndex = new MainIndex(staticDirFile)

    /**
     * Determine the role (authorization level) that the request is for.  This is the rules are
     * defined for authorization, or in other words, given a request, what UserRole is required
     * to use it?
     */
    private def getRequestedRole(request: Request, response: Response): UserRole.Value = {
        val templateRoute = router.getNext(request, response).asInstanceOf[TemplateRoute]
        val restlet = templateRoute.getNext

        val role: UserRole.Value = restlet match {
            case `staticDir` => UserRole.publik
            case `mainIndex` => UserRole.publik
            case `login` => UserRole.publik
            case `setPassword` => UserRole.publik
            case `dataDir` => UserRole.guest
            case `webRunIndex` => UserRole.user
            case `tmpDir` => UserRole.user

            case _ => UserRole.admin // default to most restrictive access for everything else
        }

        role
    }

    private def initAuthentication(restlet: Restlet): Restlet = {

        val challAuthn = new ChallengeAuthenticator(getContext.createChildContext, ChallengeScheme.HTTP_BASIC, "enter password now")   // TODO remove when we figure out how to make a real login page
        //val challAuthn = new ChallAuth(getContext.createChildContext, false, WebServer.challengeScheme, getRequestedRole)    // TODO put back when we figure out how to make a real login page
        challAuthn.setVerifier(new AuthenticationVerifier)
        challAuthn.setNext(restlet)
        challAuthn
    }

    /**
     * Standard Restlet override defines how to serve web pages.
     */
    override def createInboundRoot: Restlet = {

        router.setDefaultMatchingMode(Template.MODE_STARTS_WITH)

        component.getClients.add(Protocol.FILE)

        attach(router, "/" + WebServer.staticDirName, staticDir)
        attach(router, WebServer.dataDirBaseUrl, dataDir)
        attach(router, WebServer.tmpDirBaseUrl, tmpDir)

        val restletList: Seq[Restlet with WebUtil.SubUrl] = Seq(
            new MachineUpdate,
            new InstitutionUpdate,
            new InstitutionList,
            new MachineTypeUpdate,
            new MachineList,
            new MachineTypeList,
            new UserUpdate,
            new UserList,
            new ProcedureUpdate,
            new ProcedureList,
            new OutputList,
            webRunIndex,
            login,
            setPassword)

        restletList.map(r => attach(router, WebUtil.SubUrl.url(r.subUrl, WebUtil.cleanClassName(r.getClass.getName)), r))

        val viewOutput = new ViewOutput
        attach(router, WebUtil.SubUrl.url(viewOutput.subUrl, WebUtil.cleanClassName(viewOutput.getClass.getName)), viewOutput)

        attachProcedures(router)

        attach(router, "", mainIndex)
        /*
        attach(router, "", new WebHome)
        val authentication = initAuthentication(router)

        val auditFilter = new AuditFilter(getContext)
        auditFilter.setNext(authentication)
        
        auditFilter
        */
        initAuthentication(router) // TODO remove if auth fails
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