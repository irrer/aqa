package org.aqa.webrun.phase2

import org.aqa.web.WebUtil._
import com.pixelmed.dicom.SOPClass
import org.aqa.db.Machine
import org.aqa.Util
import org.aqa.DicomFile
import org.aqa.db.ImageIdentification
import org.aqa.Config

object ImageIdentificationValidation {

  private val MIN_IMAGES = 3

  private def formErr(msg: String, uploadFileInput: Option[IsInput]) = Left(Error.make(uploadFileInput.get, msg))

  /**
   * Identify the input files.
   */
  private def analyzeImageIdentification(dicomList: Seq[DicomFile], machine: Machine, uploadFileInput: Option[IsInput]): Either[StyleMapT, (DicomFile, Seq[ImageIdentificationFile])] = {
    val planList = Phase2Util.getPlanList(dicomList)
    val imageList = dicomList.filter(df => df.isModality(SOPClass.RTImageStorage))

    // associate each image with a plan
    val planGroups = planList.map(plan => (plan, imageList.filter(img => Phase2Util.imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    0 match {
      case _ if (planList.isEmpty) => formErr("No RTPLANS found", uploadFileInput)
      case _ if (imageList.isEmpty) => formErr("No RTIMAGEs given", uploadFileInput)
      case _ if (planGroups.isEmpty) => formErr("No RTPLAN found for RTIMAGEs", uploadFileInput)
      case _ if (planGroups.size > 1) => formErr("The RTIMAGEs reference multiple plans.  Only one plan per run is permitted.", uploadFileInput)
      case _ => {
        val plan = planGroups.head._1
        val imageList = planGroups.head._2
        val results = imageList.
          map(image => (image, ImageIdentificationAnalysis.makeImageIdentification(plan.attributeList.get, image.attributeList.get))).
          filter(ii => ii._2.nonEmpty && Config.ImageIdentificationBeamNameList.contains(ii._2.get.beamName)).
          map(ii => new ImageIdentificationFile(ii._1, ii._2.get))
        // only let one result in for each beam
        val distinct = results.map(iif => (iif.imageIdentification.beamName, iif)).toMap.values.toSeq
        0 match {
          case _ if (results.size != distinct.size) => {
            val multiImageBeams = distinct.diff(results).map(iff => iff.imageIdentification.beamName).distinct
            formErr("Multiple RTIMAGEs were taken from beam " + multiImageBeams, uploadFileInput)
          }
          case _ if (results.size < MIN_IMAGES) => formErr("Your must provide at least " + MIN_IMAGES + " but only " + results.size + " were found", uploadFileInput)
          case _ => Right(plan, results)
        }
      }
    }
  }

  /**
   * Validate the given data, and, if it is valid, organize it into a <code>ImageIdentificationRunRequirements</code> object.  If
   * it is not valid, then return a message indicating the problem.
   */
  def validate(valueMap: ValueMapT, outputPK: Option[Long], machine: Machine, uploadFileInput: Option[IsInput]): Either[StyleMapT, ImageIdentificationRunRequirements] = {
    val rtimageList = dicomFilesInSession(valueMap).filter(df => df.isModality(SOPClass.RTImageStorage))
    val rtplanList = dicomFilesInSession(valueMap).filter(df => df.isModality(SOPClass.RTPlanStorage))
    val dicomList = rtplanList ++ rtimageList

    // machines that DICOM files reference (based on device serial numbers)
    val machList = rtimageList.map(df => attributeListToMachine(df.attributeList.get)).flatten.distinct

    val imgIdent = analyzeImageIdentification(dicomList, machine, uploadFileInput)

    val result = sessionDir(valueMap) match {
      case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded", uploadFileInput)
      case _ if (dicomList.isEmpty) => formErr("No DICOM files have been uploaded.", uploadFileInput)
      case _ if (machList.size > 1) => formErr("Files from more than one machine were found.  Click Cancel to start over.", uploadFileInput)
      case _ if (imgIdent.isLeft) => Left(imgIdent.left.get)
      case Some(dir) => Right(new ImageIdentificationRunRequirements(machine, dir, imgIdent.right.get._1, imgIdent.right.get._2))
    }
    result
  }

}