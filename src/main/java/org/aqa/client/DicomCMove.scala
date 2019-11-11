package org.aqa.client

import edu.umro.ScalaUtil.DicomReceiver
import org.aqa.Logging
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.dicom.AttributeList

object DicomCMove extends Logging {

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String) = {
      logger.info("Received file " + fileName)
    }
  }

  private lazy val dicomReceiver = {
    logger.info("Started DicomReceiver")
    val dr = new DicomReceiver(ClientConfig.tmpDir, ClientConfig.DICOMClient, new MyReceivedObjectHandler)
    logger.info("Started DicomReceiver: " + ClientConfig.DICOMClient)
    dr
  }

  def get(SeriesInstanceUID: String): Seq[AttributeList] = {
    ???
  }
}