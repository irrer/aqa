package org.aqa.webrun.stakitt.stakittHTML

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Util

import java.io.File
import scala.xml.Elem

class StakittHTML(extendedData: ExtendedData, analysisList: Seq[Either[Analysis.Failure, Analysis]], rtplan: AttributeList) extends Logging {

  /**
    * Add tab related fields to the result of each image.
    * @param foa Failure or Analysis
    */
  private case class Tab(foa: Either[Analysis.Failure, Analysis], index: Int) {

    val isFailure: Boolean = foa.isLeft

    /** DICOM for this result. */
    val rtimage: AttributeList = {
      if (isFailure)
        foa.left.get.rtimage
      else
        foa.right.get.rtimage
    }

    val beamName: String = {
      Util.getBeamNameOfRtimage(rtimage, rtplan) match {
        case Some(name) => name
        case _          => s"Stakitt ${index + 1}"
      }
    }

    /** ID used for HTML id and subdirectory.  Use index to guarantee uniqueness. */
    val id: String = WebUtil.textToId(beamName) + "_" + index

    /** Put content for this beam here. */
    val dir: File = new File(extendedData.output.dir, id)

    // Ensure that the directory has been created.
    dir.mkdirs()
  }

  private val tabList = analysisList.zipWithIndex.map(foaIndex => Tab(foaIndex._1, foaIndex._2))

  /**
    * Make the HTML content for a failed beam.  This lets the user know why it fails and shows the
    * image to help them understand.  Failure often happens because the image was not a Stakitt image,
    * and just looking at the image will instantly convey that to the user.
    *
    * @param tab Failure with Tab info.
    * @return HTML content describing failure.
    */
  private def makeFailureHtml(tab: Tab): Elem = {

    val bufImg = {
      val di = new DicomImage(tab.rtimage)
      di.toDeepColorBufferedImage(0.01)
    }

    val file = new File(tab.dir, "fullImage.png")
    val imageUrl = tab.dir.getName + "/" + file.getName

    Util.writePng(bufImg, file)

    val elem = {
      <div>
        <div style="border:2px solid red; border-radius: 5px; margin:15px;">
          <h4 style="margin:8px;">
            Error:
            {tab.foa.left.get.msg}
          </h4>
        </div>
        {WebUtil.makeZoom(imageUrl, width = 768, "Stakitt Full Image")}
      </div>
    }
    elem
  }

  private def makeAnalysisHtml(tab: Tab): Elem = {
    val ha = HtmlAnalysis(tab.foa.right.get, tab.dir, tab.id, tab.index)
    ha.makeHtml()
  }

  private def makeTabContent(tab: Tab): Elem = {
    if (tab.isFailure)
      makeFailureHtml(tab)
    else
      makeAnalysisHtml(tab)
  }

  /**
    * Make the element that comprises the tab, used for navigation.
    * @param tab Failure or Analysis
    * @return Tab HTML.
    */
  private def makeTabHeader(tab: Tab): Elem = {
    val active: String = if (tab.index == 0) "active" else ""
    val elem = {
      <li class={active} style="">
        <a data-toggle="tab" href={"#" + tab.id} style="text-align:center;">
          {tab.beamName}
        </a>
      </li>
    }
    elem
  }

  private def multipleImages(): Elem = {
    val tabHeaderList: Seq[Elem] = tabList.map(makeTabHeader)
    val tabContentList: Seq[Elem] = tabList.map(makeTabContent)

    <div>
      <ul class="nav nav-tabs">
        {tabHeaderList}
      </ul>
      <div class="tab-content" style="margin-right:20px;">
        {tabContentList}
      </div>
    </div>
  }

  def makeHTML(): Unit = {

    val content = {

      val innerElem = {
        if (tabList.size == 1)
          makeTabContent(tabList.head)
        else
          multipleImages()
      }

      <div class="row">
        <div class="col-md-7 col-md-offset-1">
          {innerElem}
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "Stakitt", refresh = None, c3 = true, runScript = None)

    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)
  }

}
