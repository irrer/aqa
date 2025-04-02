package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.Util

import java.io.File

/**
 * Make a simple web page to show the RTPLAN.
 *
 * @param extendedData Metadata
 * @param rtplan The RTPLAN DICOM
 */

case class PlanHTML(extendedData: ExtendedData, rtplan: AttributeList) extends Logging {

  val fileName = "RTPlan.html"

  private val htmlFile = new File(extendedData.output.dir, fileName)

  private val content = {
    <div>
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          <h3>RTPlan</h3>
          <pre>
            {"\n" + DicomUtil.attributeListToString(rtplan)}
          </pre>
        </div>
      </div>
    </div>
  }

  private val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)

  Util.writeFile(htmlFile, text)
  logger.info("Wrote RTPlan HTML file " + htmlFile.getAbsolutePath)

}
