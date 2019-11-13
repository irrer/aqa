package org.aqa.client

import scala.collection.mutable.ArrayBuffer
import java.io.File
import org.aqa.Util
import org.aqa.Logging

/**
 * CRUD interface for the list of patient IDs used for fetching DICOM files.
 * 
 * Currently the only read access.
 */

object PatientIDList extends Logging {

  private val comment = "//"

  private val PatientIDFileName = "PatientIDFile"

  val PatientIDFile = new File(ClientConfig.DataDir, PatientIDFileName)

  private val patientIDList = ArrayBuffer[String]()

  private def parse(text: String): Seq[String] = {
    val patIDList = text.replace('\r', '\n').split('\n').toSeq.filter(line => line.nonEmpty).map(line => line.replaceAll(comment + ".*", "").trim)
    patIDList
  }

  private def createFileIfItDoesNotExist = {
    if (!PatientIDFile.canRead) {
      if (!PatientIDFile.createNewFile)
        logger.error("Unable to read or create file containing list of patient IDs: " + PatientIDFile.getAbsolutePath)
    }
  }

  private def read =
    patientIDList.synchronized({
      patientIDList.clear
      createFileIfItDoesNotExist
      Util.readTextFile(PatientIDFile) match {
        case Right(content) => parse(content).map(patId => patientIDList += patId)
        case Left(t) => logger.warn("Error reading patient ID file " + PatientIDFile.getAbsolutePath + " : " + fmtEx(t))
      }
    })

  /**
   * Get a read-only list of patient IDs.
   */
  def getPatientIDList = patientIDList.synchronized(patientIDList.toSeq)

  /**
   * Initialize list of patient IDs.
   */
  def init = {
    read
  }

}