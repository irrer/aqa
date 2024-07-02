package org.aqa.webrun.convertDicomDev

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.Procedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.ButtonType
import org.aqa.web.WebUtil.FormButton
import org.aqa.web.WebUtil.WebForm
import org.aqa.web.WebUtil.WebRow
import org.aqa.web.WebUtil.dicomFilesInSession
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

object SeriesMaker {
  private val path = new String((new SeriesMaker).pathOf)
}

class SeriesMaker extends Restlet with SubUrlRoot with Logging {

  private val pageTitle = "Series Maker"

  class FormButtonProcedure(name: String, val procedure: Option[Procedure]) extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def nextButton = makeButton("Next", ButtonType.BtnDefault)

  private def makeUploadForm(): WebForm = {

    val buttonRow = List(nextButton, cancelButton)

    val action = SeriesMaker.path // TODO
    val title = Some(pageTitle)
    val rowList: List[WebRow] = List(buttonRow)

    val form = new WebForm(action = action, title = title, rowList = rowList, fileUpload = 10, runScript = None)

    form
  }

  /**
    * Find the RTPLAN to use.  Give first priority to the one uploaded, but if no RTPLAN was
    * uploaded, then use the one referenced by the RTIMAGE file(s).
    *
    * If there was more than one RTPLAN uploaded or the RTIMAGE files reference more than
    * one RTPLAN, then return an error.  Also return an error if an RTPLAN file can not be found.
    *
    * Note that a 'known' RTPLAN file means that it was one that is in the AQA database.
    *
    * @param alList List of DICOM files uploaded.
    * @return The RTPLAN to use, or, an error message.
    */
  private def rtplanToUse(alList: Seq[AttributeList]): Either[String, AttributeList] = {
    val rtplanList = alList.filter(al => Util.isRtplan(al))
    0 match {
      case _ if rtplanList.size == 1 => // User uploaded exactly one plan, so use it.
        Right(rtplanList.head)

      case _ if rtplanList.size > 1 => // User uploaded more than one plan, so it is ambiguous as to which should be used.
        Left(s"More than one RTPLAN was uploaded.  You should either upload one or zero. \\n If zero, then one of the RTIMAGE files should reference a known RTPLAN.")

      case _ if rtplanList.isEmpty => // User did not upload a plan.  Look to see if any of the RTIMAGE files reference a plan that is in the database.
        val rtimageList = alList.filter(al => Util.isRtimage(al))
        val referencedRtplanUidList = rtimageList.flatMap(ri => Util.getRtplanSop(ri)).distinct

        def getRtplanOfImage(rtplanUid: String): Option[AttributeList] = {
          DicomSeries.getBySopInstanceUID(rtplanUid).flatMap(_.attributeListList).find(planAl => Util.sopOfAl(planAl).equals(rtplanUid))
        }

        // list of plans from the database that are referenced by the uploaded RTIMAGE files
        val referencedRtplanList = referencedRtplanUidList.flatMap(getRtplanOfImage)

        0 match {
          case _ if referencedRtplanList.isEmpty => // RTIMAGE files did not reference any plans
            Left("No RTPLAN was uploaded and none of the RTIMAGE files reference a known RTPLAN.")

          case _ if referencedRtplanList.size == 1 => // RTIMAGE files referenced exactly one plan that was found in the database, so use it.
            Right(referencedRtplanList.head) // SUCCESS!

          case _ if referencedRtplanList.isEmpty => // RTIMAGE files referenced more than one plan that was found in the database, so this is ambiguous.
            Left(s"No RTPLAN was uploaded and none of the RTIMAGE files reference ${referencedRtplanList.size} known RTPLANS.")
        }
    }
  }

  /**
    * Make a list of the RTIMAGE files that the user uploaded.
    * @param alList All DICOM files.
    * @return RTIMAGE files.
    */
  private def rtimageList(alList: Seq[AttributeList]): Seq[AttributeList] = alList.filter(al => Util.isRtimage(al))

  /**
    * Determine if the uploaded files can be processed.  If so, return None, otherwise return a
    * message for the user indicating the problem.
    * @param valueMap List of parameters and values
    * @param alList List of uploaded DICOM files.
    * @return None if value, message if not valid.
    */
  private def isValidFileSet(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String] = {

    val rtimagePresent: Boolean = alList.exists(al => Util.isRtimage(al))

    val rtplan = rtplanToUse(alList)

    0 match {
      case _ if rtplan.isLeft =>
        Some(rtplan.left.get)
      case _ if !rtimagePresent =>
        Some("No RTIMAGE files were uploaded.")
      case _ => None
    }

  }

  /**
    * Respond when the user clicks the 'Next' button.  Validate files.  If good, go to assign beams page.  If bad, show error message.
    * @param valueMap Web parameters.
    * @param alList List of DICOM files.
    * @param response HTTP response.
    */
  private def processNext(valueMap: ValueMapT, alList: Seq[AttributeList], response: Response): Unit = {
    isValidFileSet(valueMap, alList) match {
      case Some(errMsg) => // TODO set bad response
        val form = makeUploadForm()
        val errorMap = WebUtil.Error.make(WebUtil.uploadFileLabel, errMsg)
        form.setFormResponse(valueMap, errorMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)

      case _ => // TODO go to new page
        Trace.trace("TODO")
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    def buttonIs(button: FormButton): Boolean = {
      val value = valueMap.get(button.label)
      value.isDefined && value.get.equals(button.label)
    }

    try {
      val user = CachedUser.get(request)
      val alList = dicomFilesInSession(valueMap).filter(_.attributeList.isDefined).map(_.attributeList.get)

      0 match {
        // case _ if user.isEmpty                                                                                   => updateMach()

        case _ if buttonIs(cancelButton) =>
          response.redirectSeeOther("/")

        case _ if buttonIs(nextButton) =>
          processNext(valueMap, alList, response)

        case _ =>
          val form = makeUploadForm()
          form.setFormResponse(valueMap, errorMap = WebUtil.styleNone, pageTitle, response, Status.SUCCESS_OK)

        // case _ if machine.isEmpty                                                                                => updateMach()
        // case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        // case _ if buttonIs(valueMap, cancelButton)                                                               => updateMach()
        // case _ if buttonIs(valueMap, backButton)                                                                 => updateMach()

        // case _ if makeList.exists(make => valueMap.contains(make.makeButton.label)) => validateAndMake(valueMap, response)

        // case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

    /*
     */
  }
}
