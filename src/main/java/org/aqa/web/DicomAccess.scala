package org.aqa.web

import com.pixelmed.dicom.AttributeList
import java.io.File
import org.aqa.DicomFile
import org.aqa.Logging
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import scala.xml.Elem
import org.aqa.ColorScheme
import javax.imageio.ImageIO

object DicomAccess extends Logging {

  val indexName = "index.html"

  def makeTitle(attrList: Seq[AttributeList]): String = {
    val SeriesDescription = attrList.map(al => al.get(TagFromName.SeriesDescription)).distinct.mkString("   ")
    val Modality = attrList.map(al => al.get(TagFromName.Modality)).distinct.mkString("   ")
    Modality + " : " + SeriesDescription
  }

  private def makePages(dicomList: Seq[DicomFile], title: String, dir: File, colorScheme: ColorScheme.Value) = {
    ??? // TODO
  }

  private def makeIndex(dicomList: Seq[DicomFile], title: String, dir: File, colorScheme: ColorScheme.Value) = {

    
    def makeItem(df: DicomFile) {
      df.getImage(colorScheme) match {
        case Some(image) => {
          val file = new File(dir, df.file.getName.replaceAll(".dcm", "").replaceAll(".DCM", "") + ".png")
          ImageIO.write(image, "png", file)
          <td><img src={ file.getName } width="64"/></td>
        }
        case _ => {
          val modality = {
            if (df.attributeList.isDefined) df.attributeList.get.get(TagFromName.Modality).getSingleStringValueOrDefault("NA")
            else "NA"
          }
          // TODO should link to text page of metadata
          <td><a href={ "???" }>{ modality }</a></td>
        }
      }
    }

    def makeRow(group: Seq[DicomFile]): Elem = {
      <tr>
        { group.map(df => 4) }
      </tr>
    }

    val html = {
      <div title="DICOM Files.  Mouse over images to enlarge, click images for details" class="row col-md-10 col-md-offset-1">
        <h2>{ title }</h2>
        <table>
          {
            val groupList = edu.umro.ScalaUtil.Util.sizedGroups(dicomList, 16)
            groupList.map(g => makeRow(g))
          }
        </table>
      </div>
    }
  }

  /**
   * Create a page for viewing and downloading DICOM files.
   *
   * @param fileList: List of DICOM files
   *
   * @param dir: Directory in which to write the HTML.
   */
  def write(dicomList: Seq[DicomFile], dir: File, colorScheme: ColorScheme.Value) = {
    try {
      dir.mkdirs
      val attrList = DicomUtil.sortDicom(dicomList.map(df => df.attributeList).flatten)

      makePages(dicomList, makeTitle(attrList), dir, colorScheme)
      
      makeIndex(dicomList, makeTitle(attrList), dir, colorScheme)

    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while generating DICOM view for directory " + dir.getAbsolutePath + " : " + fmtEx(t))
      }
    }
  }
}
