package org.aqa.web

import org.restlet.Application
import org.restlet.Restlet
import org.restlet.Component
import org.aqa.Logging
import java.io.File
import edu.umro.RestletUtil.RestletHttps
import edu.umro.RestletUtil.ExpiresLaterFilter
import org.restlet.routing.Router
import org.restlet.routing.Redirector
import org.aqa.Config
import org.restlet.routing.Template
import org.restlet.data.Protocol
import org.restlet.resource.Directory
import org.restlet.data.Status
import org.aqa.db.Procedure
import org.aqa.webrun.WebRun
import org.restlet.security.ChallengeAuthenticator
import org.restlet.data.ChallengeScheme
import org.restlet.data.Reference
import org.restlet.security.MapVerifier
import org.restlet.Context
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.ChallengeRequest
import org.restlet.data.ChallengeResponse
import org.restlet.routing.Filter
import org.aqa.db.User
import org.aqa.db.UserRole
import org.restlet.routing.TemplateRoute
import edu.umro.ScalaUtil.Trace._
import org.aqa.db.CachedUser
import org.aqa.db.Output
import org.aqa.db.Input

object WebServer {
  val challengeScheme = ChallengeScheme.HTTP_BASIC

  //  lazy val resultsDir = new File(Config.DataDir, Config.resultsDirName)

  val staticDirBaseUrl = "/" + Config.staticDirName

  val resultsDirBaseUrl = "/" + Config.resultsDirName
  val tmpDirBaseUrl = "/" + Config.tmpDirName
  val machineConfigurationDirBaseUrl = "/" + Config.machineConfigurationDirName

  def urlOfPath(baseUrl: String, filePath: String): String = (baseUrl + "/" + filePath.replace('\\', '/')).replaceAll("///*", "/")

  def urlOfResultsPath(filePath: String): String = urlOfPath(resultsDirBaseUrl, filePath)

  def urlOfMachineConfigurationPath(filePath: String): String = urlOfPath(machineConfigurationDirBaseUrl, filePath)

  def urlOfResultsFile(file: File): String = urlOfResultsPath(fileToResultsPath(file))

  def urlOfMachineConfigurationFile(file: File): String = urlOfMachineConfigurationPath(fileToMachineConfigurationPath(file))

  def fileOfResultsPath(filePath: String): File = new File(Config.resultsDirFile, filePath)

  def fileToResultsPath(file: File): String = file.getAbsolutePath.substring(Config.resultsDirFile.getAbsolutePath.size)

  def fileToMachineConfigurationPath(file: File): String = file.getAbsolutePath.substring(Config.machineConfigurationDir.getAbsolutePath.size)

}

class WebServer extends Application with Logging {

  private lazy val component = new Component

  /**
   * If the Config.HTTPSPort is defined, then use HTTPS, otherwise use HTTP
   */
  private def addProtocol: Unit = {
    if (Config.HTTPSPort.isDefined) {
      val status = RestletHttps.addHttps(component, Config.HTTPSPort.get, Config.JavaKeyStoreFileList, List(Config.JavaKeyStorePassword))
      if (status.isLeft) {
        logger.error("Unable to use HTTPS.  Error: " + status.left.get)
      } else {
        logger.info("Using protocol " + Protocol.HTTPS + " on port " + Config.HTTPSPort.get)
        logger.info("Using keystore file " + status.right.get.keyStoreFile.getAbsolutePath)
      }
    } else {
      val server = component.getServers.add(Protocol.HTTP, 80)
      server.getProtocols.toArray.map(p => logger.info("Using protocol " + p + " on port " + server.getPort))
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
    val locations = List(""".\""", """src\main\resources\""").map(name => new File(name + Config.staticDirName))

    val dirList = locations.filter(f => f.isDirectory)

    if (dirList.isEmpty) {
      val fileNameList = locations.foldLeft("")((t, f) => t + "    " + f.getAbsolutePath)
      val msg = "Unable to find static directory in " + fileNameList
      logger.error(msg)
      throw new RuntimeException(msg)
    }
    logger.info("Using static directory " + dirList.head.getAbsolutePath)
    dirList.head
  }

  /**
   * If the static directory is different from the install directory, then delete it
   * and copy the latest install directory.
   * private def initStaticContentDir = {
   * if (!Utility.compareFolders(installDir, WebServer.STATIC_CONTENT_DIR)) {
   * logger.info("Updating directory " + WebServer.STATIC_CONTENT_DIR.getAbsolutePath + " from " + installDir.getAbsolutePath)
   * Utility.deleteFileTree(WebServer.STATIC_CONTENT_DIR)
   * Utility.copyFileTree(installDir, WebServer.STATIC_CONTENT_DIR)
   * }
   * }
   */

  private def attach(router: Router, url: String, restlet: Restlet): Unit = {
    val name = restlet.getClass.getName.replaceAll(".*\\.", "")
    val rootRef = if (restlet.isInstanceOf[Directory]) (" : " + restlet.asInstanceOf[Directory].getRootRef) else ""
    logger.info("attaching router to " + url + " ==> " + name + rootRef)
    router.attach(url, restlet)
  }

  private lazy val router = {
    val r = new Router(getContext.createChildContext)
    val redirector = new Redirector(getContext, "/target?referer={fi}", Redirector.MODE_CLIENT_SEE_OTHER)
    r
  }

  private lazy val staticDirRestlet = makeDirectory(staticDirFile)

  private lazy val machineConfigurationDirRestlet = makeDirectory(Config.machineConfigurationDirFile)

  /*
        if (false) {
            // TODO why does this not work?  The browser reloads this content anyway.
            val interval: Long = 35 * 24 * 60 * 60 * 1000.toLong
            val expiresOneHourLaterFilter = new ExpiresLaterFilter(getContext, interval, staticDir)

            attach(router, "/" + WebServer.staticDirName, expiresOneHourLaterFilter)
        }
  */

  /**
   * Allow access to the results directory, which contains the input and output files used by
   * procedures.  If a file is requested that does not exist, try to get it from the database.
   */
  private class ResultsDir extends Filter {

    /**
     * Attempt to restore the requested file from the database.  See if its directory matches
     * anything in input or output, and if it does, restore it.
     */
    private def restoreFile(file: File) = {
      try {
        def fileNameSuffix(f: File) = f.getAbsolutePath.substring(Config.resultsDirFile.getAbsolutePath.size)
        val outputDir = file.getParentFile
        val inputDir = outputDir.getParentFile

        Output.getByDirectory(fileNameSuffix(outputDir)) match {
          case Some(output) => {
            Output.getFilesFromDatabase(output.outputPK.get, outputDir.getParentFile)
            logger.info("Restored ouput directory from database: " + outputDir.getAbsolutePath)
          }
          case _ =>
        }

        Input.getByDirectory(fileNameSuffix(inputDir)) match {
          case Some(input) => {
            Input.getFilesFromDatabase(input.inputPK.get, inputDir.getParentFile)
            logger.info("Restored input directory from database: " + inputDir.getAbsolutePath)
          }
          case _ =>
        }
      } catch {
        case t: Throwable => {
          logger.warn("Unable to restore input/output files from database for file " + file.getAbsolutePath + " : " + t)
        }
      }
    }

    override def beforeHandle(request: Request, response: Response): Int = {
      val fileName = Config.DataDir.getAbsolutePath + request.getOriginalRef.getPath.replace("/", File.separator)
      val file = new File(fileName)
      if (!file.canRead) restoreFile(file)
      Filter.CONTINUE
    }
    this.setNext(makeDirectory(Config.resultsDirFile))
  }

  private lazy val resultsDirectoryRestlet = new ResultsDir

  private lazy val tmpDirectoryRestlet = makeDirectory(Config.tmpDirFile)

  private lazy val login = new Login

  private lazy val notAuthorized = new NotAuthorized

  private lazy val notAuthenticated = new NotAuthenticated

  private lazy val setPassword = new SetPassword

  private lazy val webRunIndex = new WebRunIndex

  private lazy val outputList = new OutputList

  private lazy val viewOutput = new ViewOutput

  private lazy val termsOfUse = new TermsOfUse

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
      case `staticDirRestlet` => UserRole.publik
      case `mainIndex` => UserRole.publik
      case `login` => UserRole.publik
      case `notAuthorized` => UserRole.publik
      case `notAuthenticated` => UserRole.publik
      case `termsOfUse` => UserRole.publik
      case `setPassword` => UserRole.guest
      case `resultsDirectoryRestlet` => UserRole.guest
      case `webRunIndex` => UserRole.user
      case `viewOutput` => UserRole.user
      case `outputList` => UserRole.user
      case `tmpDirectoryRestlet` => UserRole.user
      case `machineConfigurationDirRestlet` => UserRole.user
      case _ => {
        logger.info("admin role requested by " + WebUtil.getUserIdOrDefault(request, "unknown"))
        UserRole.admin // default to most restrictive access for everything else
      }
    }

    role
  }

  private def initAuthentication(restlet: Restlet): Restlet = {
    val challAuthn = new ChallengeAuthenticator(getContext.createChildContext, ChallengeScheme.HTTP_BASIC, "Please enter your AQA password") // TODO remove when we figure out how to make a real login page
    challAuthn.setVerifier(new AuthenticationVerifier(getRequestedRole _))
    challAuthn.setNext(restlet)

    case class UserAndRole(user: User, role: UserRole.Value);

    def checkAuthorization(request: Request, response: Response, challResp: ChallengeResponse): Unit = {
      val requestedRole = getRequestedRole(request, response)

      if (requestedRole.id != UserRole.publik.id) { // let anyone into public areas
        val userAndRole = { for (u <- User.getUserById(challResp.getIdentifier); r <- UserRole.stringToUserRole(u.role)) yield new UserAndRole(u, r) }
        userAndRole match {
          case Some(uar) => {
            if (uar.role.id < requestedRole.id) {
              logger.warn("Authorization violation.  User " + uar.user.id +
                " attempted to access " + request.toString + " that requires role " + requestedRole + " but their role is only " + uar.role)
              response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
              //response.redirectSeeOther("/NotAuthorized")   // TODO rm
              response.redirectSeeOther(notAuthorized.pathOf)
            } else response.setStatus(Status.SUCCESS_OK)
          }
          case _ => {
            logger.warn("Internal authorization error.  Can not identify user.")
            response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
          }
        }
      }
    }

    def needsToAgreeToTerms(id: String): Boolean = {
      User.getUserById(id) match {
        case Some(user) => user.termsOfUseAcknowledgment.isEmpty
        case _ => false
      }
      false // TODO remove this when we figure out how to fix 'aggree to terms of use'
    }

    class RedirectUnauthorizedToLogin extends Filter {
      override def beforeHandle(request: Request, response: Response): Int = {
        // If there are no credentials, then let the authenticator decide whether to
        // accept or reject the request.  If rejected (not publik), then it will
        // send a challenge request to the client.
        request.getChallengeResponse match {
          case null => Filter.CONTINUE
          case cr => {
            if (needsToAgreeToTerms(cr.getIdentifier)) {
              termsOfUse.handle(request, response)
              Filter.SKIP
            } else {
              checkAuthorization(request, response, cr)
              if (response.getStatus.getCode != Status.SUCCESS_OK.getCode) Filter.SKIP
              else Filter.CONTINUE
            }
          }
        }
      }
    }

    val rutl = new RedirectUnauthorizedToLogin
    rutl.setNext(challAuthn)
    rutl

    //challAuthn
  }

  class ResponseToReferrer(request: Request) extends Response(request) {
    //        override def setLocationRef(locationUri: String): Unit = {
    //            println("Setting ref: " + locationUri)
    //            if (getRequest.getResourceRef != null) {
    //                val baseRef: Reference =
    //                    if (getRequest.getResourceRef.getBaseRef != null) {
    //                        getRequest.getResourceRef.getBaseRef
    //                    }
    //                    else {
    //                        getRequest.getResourceRef
    //                    }
    //                setLocationRef(new Reference(baseRef, locationUri).getTargetRef)
    //            }
    //        }
  }

  /**
   * Filter all messages to fix REDIRECTION_SEE_OTHER messages so that they go to the
   * 'right' place.  The AWS security layer translates https to http and back, but this
   * server only sees them as http.  So when it does a redirection, it otherwise redirects
   * the browser to http.  This is what this filter fixes.  This also works locally on
   * the AWS host where the client does not go through the security layer but instead
   * goes directly to the service via http.
   */
  private def resolveToReferer(restlet: Restlet): Restlet = {

    class ResolveToRefererFilter extends Filter {

      override def afterHandle(request: Request, response: Response): Unit = {
        if ((response.getStatus == Status.REDIRECTION_SEE_OTHER) && (request.getReferrerRef != null)) {
          val locRef = response.getLocationRef
          val desiredHost = request.getReferrerRef.getHostIdentifier
          val path = locRef.toString.replaceAll("^" + request.getResourceRef.getHostIdentifier, desiredHost)
          response.redirectSeeOther(path)
          logger.info("Redirecting response via REDIRECTION_SEE_OTHER from/to:" +
            "\n    " + locRef +
            "\n    " + path)
        }
        // TODO Problem : If css and js is served from Cloudflare, they work, but if served from AQA, they don't.
        // The problem shows up for tabs in the online spreadsheet viewer and timedatepicker.
        //                if (request.toString.contains("css")) {
        //                    val headers = response.getHeaders.toArray.toSeq
        //                    val entity = response.getEntity
        //                    val mediaType = if (entity == null) null else entity.getMediaType
        //                    val charSet = if (entity == null) null else entity.getCharacterSet
        //                    if (entity != null) {
        //                        entity.setCharacterSet(null)
        //                    }
        //                    println("request: " + request)
        //                    println("    media type: " + mediaType)
        //                    println("    charSet: " + charSet)
        //                    println("    num headers: " + headers.size)
        //                    headers.map(h => println("    " + h))
        //                }
      }
    }

    val rToR = new ResolveToRefererFilter
    rToR.setNext(restlet)
    rToR
  }

  /**
   * Ensure that the user has agreed to the terms in the legal statement for using this service.
   */
  private def initLegal(restlet: Restlet): Restlet = {

    class LegalFilter extends Filter {

      override def afterHandle(request: Request, response: Response): Unit = {

        CachedUser.get(request) match {
          case Some(user) => if (!user.termsOfUseAcknowledgment.isDefined) response.redirectSeeOther("/")
          case _ =>
        }

        //                val user = WebUtil.getUserIdOrDefault(request, "")
        //                if ((response.getStatus == Status.REDIRECTION_SEE_OTHER) && (request.getReferrerRef != null)) {
        //                    val locRef = response.getLocationRef
        //                    val desiredHost = request.getReferrerRef.getHostIdentifier
        //                    val path = locRef.toString.replaceAll("^" + request.getResourceRef.getHostIdentifier, desiredHost)
        //                    response.redirectSeeOther(path)
        //                    logger.info("Redirecting response via REDIRECTION_SEE_OTHER from/to:" +
        //                        "\n    " + locRef +
        //                        "\n    " + path)

      }
    }

    val legalFilter = new LegalFilter
    legalFilter.setNext(restlet)
    legalFilter
  }

  /**
   * Standard Restlet override defines how to serve web pages.
   */
  override def createInboundRoot: Restlet = {

    try {
      router.setDefaultMatchingMode(Template.MODE_STARTS_WITH)

      component.getClients.add(Protocol.FILE)

      attach(router, WebServer.staticDirBaseUrl, staticDirRestlet)
      attach(router, WebServer.resultsDirBaseUrl, resultsDirectoryRestlet)
      attach(router, WebServer.tmpDirBaseUrl, tmpDirectoryRestlet)
      attach(router, WebServer.machineConfigurationDirBaseUrl, machineConfigurationDirRestlet)

      val restletList: Seq[Restlet with WebUtil.SubUrlTrait] = Seq(
        new MachineUpdate,
        new InstitutionUpdate,
        new InstitutionList,
        new MachineTypeUpdate,
        new MachineList,
        new MachineTypeList,
        new MultileafCollimatorList,
        new EPIDList,
        new EPIDUpdate,
        new MultileafCollimatorUpdate,
        new PMIList,
        new PMIUpdate,
        new UserUpdate,
        new UserList,
        new ProcedureUpdate,
        new ProcedureList,
        new ServiceInfo,
        new ServiceInstance,
        termsOfUse,
        outputList,
        webRunIndex,
        login,
        notAuthorized,
        notAuthenticated,
        setPassword)

      restletList.map(r => attach(router, WebUtil.SubUrl.url(r.subUrl, WebUtil.cleanClassName(r.getClass.getName)), r))

      restletList.map(r => attach(router, r.pathOf, r))

      attach(router, WebUtil.SubUrl.url(viewOutput.subUrl, WebUtil.cleanClassName(viewOutput.getClass.getName)), viewOutput)

      // This attaches the execution of all procedures
      router.attach("/run", new WebRun)

      attach(router, "", mainIndex)
      //        val auditFilter = new AuditFilter(getContext)
      //        auditFilter.setNext(authentication)
      //        auditFilter

      val auth = initAuthentication(router)
      resolveToReferer(auth)
    } catch {
      case t: Throwable => {
        println("WebServer.createInboundRoot unexpected error: " + t) // TODO extreme badness
        router
      }
    }
  }

  private def waitForWebServiceToStart = {
    if (!component.isStarted) Thread.sleep(100)
    if (!component.isStarted) {
      val timeout = System.currentTimeMillis + (10 * 1000) // wait up to 10 seconds for the component to start.  It should be only a few milliseconds.
      while ((!component.isStarted) && (System.currentTimeMillis < timeout)) {
        logger.info("Waiting for web service to start ...")
        Thread.sleep(1000)
      }
    }
  }

  /**
   * Initialize and start the service.
   */
  def init: Unit = {
    addProtocol
    component.getDefaultHost.attach(this)
    component.start
    waitForWebServiceToStart
  }

  init
}