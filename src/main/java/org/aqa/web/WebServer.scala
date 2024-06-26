/*
 * Copyright 2023 Regents of the University of Michigan
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

package org.aqa.web

import edu.umro.RestletUtil.NetworkIpFilter
import edu.umro.RestletUtil.RestletHttps
import org.aqa.Config
import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
import org.aqa.customizeRtPlan.phase3plan.Phase3HTML
import org.aqa.db.CachedUser
import org.aqa.db.Input
import org.aqa.db.Output
import org.aqa.db.UserRole
import org.aqa.simpleRtPlan.SimpleRtPlanInterface
import org.aqa.webrun.WebRun
import org.aqa.webrun.bbByCBCT.BBbyCBCTChartHistoryRestlet
import org.aqa.webrun.bbByEpid.BBbyEPIDChartHistoryPartialRestlet
import org.aqa.webrun.bbByEpid.BBbyEPIDChartHistoryRestlet
import org.aqa.webrun.dailyQA.DailyQASummary
import org.aqa.webrun.focalSpot.FSHistoryRestlet
import org.aqa.webrun.gapSkew.GapSkewHistoryRestlet
import org.aqa.webrun.gapSkew.GapSkewLatestHtml
import org.aqa.webrun.phase2.centerDose.CenterDoseChartHistoryRestlet
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringChartHistoryRestlet
import org.aqa.webrun.phase2.phase2csv.CsvApi
import org.aqa.webrun.phase2.phase2csv.Phase2Csv
import org.aqa.webrun.phase2.phase2csv.Phase2CsvRestlet
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessHistoryRestlet
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessSubHTML
import org.aqa.webrun.phase2.vmat.VMATChartHistoryRestlet
import org.aqa.webrun.phase2.wedge.WedgeChartHistoryRestlet
import org.aqa.webrun.phase2.wedge.WedgeUseAsBaseline
import org.aqa.webrun.wl.WLHistoryRestlet
import org.aqa.webrun.wl.WLNav
import org.aqa.webrun.wl.WLUpdateRestlet
import org.restlet.Application
import org.restlet.Component
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.ChallengeResponse
import org.restlet.data.ChallengeScheme
import org.restlet.data.Protocol
import org.restlet.data.Status
import org.restlet.resource.Directory
import org.restlet.routing.Filter
import org.restlet.routing.Router
import org.restlet.routing.Template
import org.restlet.routing.TemplateRoute
import org.restlet.security.ChallengeAuthenticator

import java.io.File

/**
  * Provide support for serving web pages.  Check incoming requests for proper authentication and authorization and route them to the proper web page.
  *
  * @author Jim Irrer irrer@med.umich.edu
  */
object WebServer {
  val challengeScheme: ChallengeScheme = ChallengeScheme.HTTP_BASIC

  private val staticDirBaseUrl: String = "/" + Config.staticDirName

  private val resultsDirBaseUrl: String = "/" + Config.resultsDirName
  val tmpDirBaseUrl: String = "/" + Config.tmpDirName
  private val machineConfigurationDirBaseUrl: String = "/" + Config.machineConfigurationDirName

  private def urlOfPath(baseUrl: String, filePath: String): String = (baseUrl + "/" + filePath.replace('\\', '/')).replaceAll("//+", "/")

  def urlOfResultsPath(filePath: String): String = urlOfPath(resultsDirBaseUrl, filePath)

  private def urlOfTmpPath(filePath: String): String = urlOfPath(tmpDirBaseUrl, filePath)

  def urlOfMachineConfigurationPath(filePath: String): String = urlOfPath(machineConfigurationDirBaseUrl, filePath)

  def urlOfResultsFile(file: File): String = urlOfResultsPath(fileToResultsPath(file))

  def urlOfTmpFile(file: File): String = urlOfTmpPath(fileToTmpPath(file))

  def urlOfMachineConfigurationFile(file: File): String = urlOfMachineConfigurationPath(fileToMachineConfigurationPath(file))

  def fileOfResultsPath(filePath: String): File = new File(Config.resultsDirFile, filePath)

  def fileToResultsPath(file: File): String = file.getAbsolutePath.substring(Config.resultsDirFile.getAbsolutePath.length)

  private def fileToTmpPath(file: File): String = file.getAbsolutePath.substring(Config.tmpDirFile.getAbsolutePath.length)

  private def fileToMachineConfigurationPath(file: File): String = file.getAbsolutePath.substring(Config.machineConfigurationDir.getAbsolutePath.length)

}

class WebServer extends Application with Logging {

  private lazy val component = new Component

  /**
    * If the Config.HTTPSPort is defined, then use HTTPS, otherwise use HTTP
    */
  private def addProtocol(): Unit = {
    if (Config.HTTPSPort.isDefined) {
      // Sometimes reformatting the XML config file causes the password not to work, so try it with and without whitespace.
      val status = RestletHttps.addHttps(component, Config.HTTPSPort.get, Config.JavaKeyStoreFileList, Config.JavaKeyStorePasswordList)
      if (status.isLeft) {
        logger.error("Unable to use HTTPS.  Error: " + status.left.get)
      } else {
        logger.info("Using protocol " + Protocol.HTTPS + " on port " + Config.HTTPSPort.get)
        logger.info("Using keystore file " + status.right.get.keyStoreFile.getAbsolutePath)
      }
    } else {
      val server = component.getServers.add(Protocol.HTTP, Config.HTTPPort)
      server.getProtocols.toArray.foreach(p => logger.info("Using protocol " + p + " on port " + server.getPort))
    }
  }

  private def makeDirectory(dir: File): Directory = {
    val uri = ("file:///" + dir.getCanonicalPath).replace('\\', '/') + "/"
    val directory = new Directory(getContext.createChildContext, uri)
    directory.setListingAllowed(true)
    directory
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
    //logger.info("attaching router to " + url + " ==> " + name + rootRef)
    router.attach(url, restlet)
  }

  private lazy val router = {
    val r = new Router(getContext.createChildContext)
    // val redirector = new Redirector(getContext, "/target?referer={fi}", Redirector.MODE_CLIENT_SEE_OTHER)
    r
  }

  private lazy val staticDirRestlet = {
    logger.info("staticDirRestlet being constructed")
    lazy val docRestlet = new Doc
    val staticDir = makeDirectory(Config.staticDirFile)

    docRestlet.setNext(staticDir)
    docRestlet
  }

  private lazy val anonymousTranslate = new AnonymousTranslate

  private lazy val getSeries = new GetSeries

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
    private def restoreFile(file: File): Unit = {
      try {
        def fileNameSuffix(f: File) = f.getAbsolutePath.substring(Config.resultsDirFile.getAbsolutePath.length)

        val outputDir = file.getParentFile
        val inputDir = outputDir.getParentFile

        Output.getByDirectory(fileNameSuffix(outputDir)) match {
          case Some(output) =>
            Output.getFilesFromDatabase(output.outputPK.get, outputDir.getParentFile)
            logger.info("Restored output directory from database: " + outputDir.getAbsolutePath)
          case _ =>
        }

        Input.getByDirectory(fileNameSuffix(inputDir)) match {
          case Some(input) =>
            Input.restoreFilesFromDatabase(input.inputPK.get, inputDir.getParentFile)
            logger.info("Restored input directory from database: " + inputDir.getAbsolutePath)
          case _ =>
        }
      } catch {
        case t: Throwable =>
          logger.warn("Unable to restore input/output files from database for file " + file.getAbsolutePath + " : " + t)
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

  private lazy val systemModificationUpdate = new SystemModificationUpdate

  private lazy val csvApi = new CsvApi

  private lazy val webRunIndex = new WebRunIndex

  private lazy val outputList = new OutputList

  private lazy val viewOutput = new ViewOutput

  private lazy val termsOfUse = new TermsOfUse

  private lazy val mainIndex = new MainIndex(Config.staticDirFile)

  /**
    * Referencing this value will force all of the lazy values to be constructed.
    */
  private lazy val forceConstruction = {

    val list = Seq(
      staticDirRestlet,
      anonymousTranslate,
      getSeries,
      mainIndex,
      login,
      notAuthorized,
      notAuthenticated,
      termsOfUse,
      setPassword,
      systemModificationUpdate,
      resultsDirectoryRestlet,
      webRunIndex,
      viewOutput,
      outputList,
      tmpDirectoryRestlet,
      machineConfigurationDirRestlet,
      csvApi
    )

    val numNull = list.count(_ == null)
    numNull
  }

  /**
    * Determine the role (authorization level) that the request is for.  This is the rules are
    * defined for authorization, or in other words, given a request, what UserRole is required
    * to use it?
    *
    * This function is related to <code>forceConstruction</code>, which forces the construction of restlets.
    */
  private def getRequestedRole(request: Request, response: Response): UserRole.Value = {
    val templateRoute = router.getNext(request, response).asInstanceOf[TemplateRoute]
    val restlet = templateRoute.getNext

    def isCsvDir: Boolean = {
      val prefix = "/" + Config.resultsDirName + "/" + Phase2Csv.consortiumCsvDir.getName + "/"
      val is = request.getOriginalRef.toString.drop(request.getHostRef.toString.length).startsWith(prefix)
      is
    }

    val role: UserRole.Value = restlet match {
      case `staticDirRestlet`                    => UserRole.publik
      case `anonymousTranslate`                  => UserRole.guest // tough lesson: This MUST be privileged to at least require a password.
      case `mainIndex`                           => UserRole.publik
      case `login`                               => UserRole.publik
      case `notAuthorized`                       => UserRole.publik
      case `notAuthenticated`                    => UserRole.publik
      case `termsOfUse`                          => UserRole.publik
      case `setPassword`                         => UserRole.guest
      case `systemModificationUpdate`            => UserRole.publik
      case `resultsDirectoryRestlet` if isCsvDir => UserRole.publik
      case `resultsDirectoryRestlet`             => UserRole.user
      case `csvApi`                              => UserRole.guest
      case `webRunIndex`                         => UserRole.user
      case `getSeries`                           => UserRole.user
      case `viewOutput`                          => UserRole.user
      case `outputList`                          => UserRole.user
      case `tmpDirectoryRestlet`                 => UserRole.user
      case `machineConfigurationDirRestlet`      => UserRole.user
      case _ =>
        logger.info("admin role requested by " + WebUtil.getUserIdOrDefault(request, "unknown"))
        UserRole.admin // default to most restrictive access for everything else
    }

    role
  }

  private val maxHttpActiveCount = 1000000
  private val httpActiveRequests = new java.util.concurrent.Semaphore(maxHttpActiveCount)
  // maximum number of simultaneous HTTP connections.
  private var maxAttainedHttpActiveCount = -1

  private def initAuthentication(restlet: Restlet): Restlet = {
    val challengeAuthentication =
      new ChallengeAuthenticator(getContext.createChildContext, ChallengeScheme.HTTP_BASIC, Config.PasswordPrompt) // TODO remove when we figure out how to make a real login page
    challengeAuthentication.setVerifier(new AuthenticationVerifier(getRequestedRole))
    challengeAuthentication.setNext(restlet)

    def checkAuthorization(request: Request, response: Response, challengeResp: ChallengeResponse): Unit = {
      try {
        val requestedRole = getRequestedRole(request, response)

        if (requestedRole.id != UserRole.publik.id) { // let anyone into public areas
          val userOpt = CachedUser.get(challengeResp.getIdentifier, new String(challengeResp.getSecret))
          userOpt match {
            case Some(user) =>
              if (user.getRole.get.id < requestedRole.id) {
                logger.warn(
                  "Authorization violation.  User " + user.id +
                    " attempted to access " + request.toString + " that requires role " + requestedRole + " but their role is only " + user.getRole
                )
                response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
                response.redirectSeeOther(notAuthorized.pathOf)
              } else
                response.setStatus(Status.SUCCESS_OK)

            case _ =>
              logger.warn("Internal authorization error.  Can not identify user.")
              response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
          }
        }
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception during authentication.  Can not identify user from request: " + request.toString + " :\n" + fmtEx(t))
          response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
      }
    }

    class RedirectUnauthorizedToLogin extends Filter {
      override def beforeHandle(request: Request, response: Response): Int = {

        httpActiveRequests.acquire()
        val inUseCount = maxHttpActiveCount - httpActiveRequests.availablePermits()

        // show usage if it is large-ish.
        if ((inUseCount > maxAttainedHttpActiveCount) || (inUseCount > 2)) {
          maxAttainedHttpActiveCount = inUseCount
          logger.info("Starting HTTP operation.   inUseCount: " + inUseCount + "    Maximum number of simultaneous HTTP connections since server was started: " + maxAttainedHttpActiveCount)
        }

        //noinspection SpellCheckingInspection
        /* If there are no credentials, then let the authenticator decide whether to
         * accept or reject the request.  If rejected (not publik), then it will
         * send a challenge request to the client.
         */
        request.getChallengeResponse match {
          case null => Filter.CONTINUE
          case cr =>
            checkAuthorization(request, response, cr)
            if (response.getStatus.getCode == Status.SUCCESS_OK.getCode)
              Filter.CONTINUE
            else
              Filter.SKIP
        }
      }

      override def afterHandle(request: Request, response: Response): Unit = {
        super.afterHandle(request, response)
        httpActiveRequests.release()
        // val inUseCount = maxHttpActiveCount - httpActiveRequests.availablePermits()
        // Trace.trace("Finished HTTP operation.     inUseCount: " + inUseCount)
      }
    }

    val ru = new RedirectUnauthorizedToLogin
    ru.setNext(challengeAuthentication)
    if (Config.AllowedHttpIpList.isEmpty)
      ru
    else {
      val networkIpFilter = new NetworkIpFilter(getContext, Config.AllowedHttpIpList)
      networkIpFilter.setNext(ru)
      networkIpFilter
    }
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
          logger.info(
            "Redirecting response via REDIRECTION_SEE_OTHER from/to:" +
              "\n    " + locRef +
              "\n    " + path
          )
        }
        //noinspection SpellCheckingInspection
        /* TODO Problem : If css and js is served from Cloudflare, they work, but if served from AQA, they don't.
         * The problem shows up for tabs in the online spreadsheet viewer and timedatepicker.
         *                if (request.toString.contains("css")) {
         *                    val headers = response.getHeaders.toArray.toSeq
         *                    val entity = response.getEntity
         *                    val mediaType = if (entity == null) null else entity.getMediaType
         *                    val charSet = if (entity == null) null else entity.getCharacterSet
         *                    if (entity != null) {
         *                        entity.setCharacterSet(null)
         *                    }
         *                    println("request: " + request)
         *                    println("    media type: " + mediaType)
         *                    println("    charSet: " + charSet)
         *                    println("    num headers: " + headers.size)
         *                    headers.map(h => println("    " + h))
         *                 }
         */
      }
    }

    val rToR = new ResolveToRefererFilter
    rToR.setNext(restlet)
    rToR
  }

  /**
    * Ensure that the user has agreed to the terms in the legal statement for using this service.
    */
  /*
  private def initLegal(restlet: Restlet): Restlet = {

    class LegalFilter extends Filter {

      override def afterHandle(request: Request, response: Response): Unit = {

        CachedUser.get(request) match {
          case Some(user) => if (user.termsOfUseAcknowledgment.isEmpty) response.redirectSeeOther("/")
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
   */

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
        new MachineDailyQAList,
        new MachineWLList,
        new EPIDList,
        new EPIDUpdate,
        new MultileafCollimatorUpdate,
        new MaintenanceRecordList,
        new MachineDailyQAUpdate,
        new MachineWLUpdate,
        new MaintenanceRecordUpdate,
        new UserUpdate,
        new UserList,
        new PatientProcedureList,
        new PatientProcedureUpdate,
        new PatientProcedureXml,
        new MachineXml,
        new ProcedureUpdate,
        new ProcedureList,
        new ServiceInfo,
        new ServiceInstance,
        new SystemModificationList,
        systemModificationUpdate,
        new WedgeUseAsBaseline,
        new CenterDoseChartHistoryRestlet,
        new CollimatorCenteringChartHistoryRestlet,
        new BBbyCBCTChartHistoryRestlet,
        new BBbyEPIDChartHistoryRestlet,
        new WLNav,
        new BBbyEPIDChartHistoryPartialRestlet,
        new VMATChartHistoryRestlet,
        new WedgeChartHistoryRestlet,
        new SymmetryAndFlatnessHistoryRestlet,
        new WLHistoryRestlet,
        new DailyQASummary,
        new DataCollectionSummary,
        new SymmetryAndFlatnessSubHTML,
        new GapSkewHistoryRestlet,
        new GapSkewLatestHtml,
        new FSHistoryRestlet,
        new Phase2CsvRestlet,
        new MachineLogXml,
        csvApi,
        new WLUpdateRestlet,
        new OutputHeading,
        anonymousTranslate,
        getSeries,
        new CustomizeRtPlanInterface,
        new Phase3HTML,
        new SimpleRtPlanInterface,
        termsOfUse,
        outputList,
        webRunIndex,
        login,
        notAuthorized,
        notAuthenticated,
        setPassword
      )

      restletList.foreach(r => attach(router, WebUtil.SubUrl.url(r.subUrl, WebUtil.cleanClassName(r.getClass.getName)), r))

      restletList.foreach(r => attach(router, r.pathOf, r))

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
      case t: Throwable =>
        val msg = "WebServer.createInboundRoot unexpected error: " + fmtEx(t)
        logger.error(msg)
        router
    }
  }

  private def waitForWebServiceToStart(): Unit = {
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
    * Set min and max threads to avoid Restlet errors similar to:
    *   [Thread-4] WARN  Server:135 - Unable to run the following server-side task: sun.net.httpserver.ServerImpl$Exchange@4b9315ab
    *
    * References:
    *   org.restlet.engine.connector.NetServerHelper.java
    *     https://javadocs.restlet.talend.com/2.4/jse/engine/org/restlet/engine/connector/NetServerHelper.html
    *
    *   https://stackoverflow.com/questions/8247869/repeated-calls-by-restlet-client-to-restlet-server-hangs
    */
  private def setThreadPoolSize(): Unit = {
    val serverList = component.getServers
    (0 until serverList.size()).foreach(i => {
      val parameters = serverList.get(i).getContext.getParameters
      parameters.add("maxThreads", Config.RestletMaxThreads.toString)
      parameters.add("minThreads", Config.RestletMinThreads.toString)
      logger.info("set min and max threads: " + Config.RestletMinThreads.toString + " - " + Config.RestletMaxThreads.toString)
    })
  }

  /**
    * Initialize and start the service.
    */
  def init(): Unit = {
    logger.info("Beginning web service initialization.")
    addProtocol()

    setThreadPoolSize()

    component.getDefaultHost.attach(this)
    component.start()
    waitForWebServiceToStart()
    logger.info("Started web service.   Restlets that failed to be constructed: " + forceConstruction)
  }

  /**
    * Shut down the Restlet HTTP server.  This is only for testing/diagnostics.  See the <code>AQA.scala</code> for usage.
    */
  /*
  def shutdown(): Unit = {
    val serverList = component.getServers
    (0 until serverList.size()).foreach(i => {

      val server = serverList.get(i)
      server.stop()
    })

    Thread.sleep(5 * 1000)

    // This was an attempt to restart the HTTP server, but it did not work
    // init()
  }
   */

  init()
}
