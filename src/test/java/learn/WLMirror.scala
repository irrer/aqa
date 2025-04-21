package learn

import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil

import java.awt.Rectangle
import java.io.File
import scala.xml.Elem

object WLMirror {

  private val horzHeight = 50
  private val horzWidth = 52

  private val vertHeight = horzWidth
  private val vertWidth = horzHeight

  private val htmlDir = new File("""D:\AQA_Data\results\dev\WLMirror""")

  private def makeHtml(name: String, content: Elem, runScript: String): Unit = {
    val file = new File(htmlDir, name)

    val text = WebUtil.wrapBody(content = content, pageTitle = name, refresh = None, c3 = true, runScript = Some(runScript))

    Util.writeFile(file, text)
    Trace.trace("Wrote file " + file.getAbsolutePath)
  }

  private def horzProfile(image: DicomImage): Seq[Double] = {
    val rectList = (0 until image.width).map(x => image.getSubArray(new Rectangle(x, 0, 1, image.height)))

    val threshold: Double = {
      val sorted = image.pixelData.flatten.sorted
      val count = (sorted.size * 0.05).round.toInt

      val lo = sorted.take(count).sum / count
      val hi = sorted.takeRight(count).sum / count

      val j = sorted.take(10)
      val j2 = sorted.takeRight(10)
      Trace.trace(j ++ j2)

      (lo + hi) / 2
    }

    rectList.map(r => {
      LocateEdge.locateEdge(r.flatten, threshold)
    })
  }

  private def vertProfile(image: DicomImage): Seq[Double] = {
    val rectList = (0 until image.height).map(y => image.getSubArray(new Rectangle(0, y, image.width, 1)))

    val threshold: Double = {
      val sorted = image.pixelData.flatten.sorted
      val count = (sorted.size * 0.05).round.toInt

      val lo = sorted.take(count).sum / count
      val hi = sorted.takeRight(count).sum / count

      val j = sorted.take(10)
      val j2 = sorted.takeRight(10)
      Trace.trace(j ++ j2)

      (lo + hi) / 2
    }

    rectList.map(r => LocateEdge.locateEdge(r.flatten, threshold))
  }

  private def makePngFile(image: DicomImage): String = {
    val bufImg = image.toDeepColorBufferedImage(0.1)
    val file = new File(htmlDir, System.currentTimeMillis() + ".png")
    Thread.sleep(50)
    Util.writePng(bufImg, file)
    file.getName
  }

  def makeChart(data: Seq[Double]): C3Chart = {
    new C3Chart(
      width = None,
      height = None,
      xAxisLabel = "X Axis",
      xDataLabel = "Y Axis",
      xValueList = data.indices.map(_.toDouble),
      xFormat = ".4g",
      yAxisLabels = Seq("Y"),
      yDataLabel = "Level",
      yValues = Seq(data),
      yFormat = ".4g",
      yColorList = Seq()
    )
  }

  private def doFile(df: DicomFile): Unit = {

    val image = new DicomImage(df.al)

    val xChart = makeChart(image.columnSums.map(_.toDouble))
    val yChart = makeChart(image.rowSums.map(_.toDouble))

    val top = image.getSubimage(new Rectangle(568, 525, horzWidth, horzHeight))
    val bottom = image.getSubimage(new Rectangle(568, 615, horzWidth, horzHeight))

    val left = image.getSubimage(new Rectangle(524, 564, vertWidth, vertHeight))
    val right = image.getSubimage(new Rectangle(611, 564, vertWidth, vertHeight))

    val topImageFile: String = makePngFile(top)
    val bottomImageFile: String = makePngFile(bottom)

    val leftImageFile: String = makePngFile(left)
    val rightImageFile: String = makePngFile(right)

    val topProfile = horzProfile(top)
    val bottomProfile = horzProfile(bottom)

    val leftProfile = vertProfile(left)
    val rightProfile = vertProfile(right)

    val topChart = makeChart(topProfile)
    val bottomChart = makeChart(bottomProfile)

    val leftChart = makeChart(leftProfile)
    val rightChart = makeChart(rightProfile)

    def getAngle(tag: AttributeTag): String = {
      val angle = DicomUtil.findAllSingle(df.al, tag).head.getDoubleValues.head
      "%3d".format(Util.angleRoundedTo90(angle))
    }

    val ganCol = s"Gantry: ${getAngle(TagByName.GantryAngle)}   Collimator: ${getAngle(TagByName.BeamLimitingDeviceAngle)}"

    def stats(name: String, profile: Seq[Double]) = {
      val fmt = "%6.4f"
      val text =
        s"${"%-9s".format(name)}" +
          s" ${ganCol}" +
          s" | StdDev: ${fmt.format(ImageUtil.stdDev(profile.map(_.toFloat)))}" +
          s" | Min: ${fmt.format(profile.min)}" +
          s" | Max: ${fmt.format(profile.max)}" +
          s" | Range: ${fmt.format(profile.max - profile.min)}" +
          s" | Mean: ${fmt.format(profile.sum / profile.length)}"
      println(text)
      <h3>
        {text}
      </h3>
    }

    val metadata = {
      <h3>
      </h3>
    }

    val content = {
      <div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {metadata}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>
              X Profile
            </h3>
            {xChart.html}
            <h3>
              Y Profile
            </h3>
            {yChart.html}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>
              {stats("Top", topProfile)}
            </h3>
            <img src={topImageFile} width={(horzWidth * 2).toString}/>
            {topChart.html}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>
              {stats("Bottom", bottomProfile)}
            </h3>
            <img src={bottomImageFile} width={(horzWidth * 2).toString}/>
            {bottomChart.html}
          </div>
        </div>


        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>
              Left StdDev: {"%6.4f".format(ImageUtil.stdDev(leftProfile.map(_.toFloat)))}
              {stats("Left", leftProfile)}
            </h3>
            <img src={leftImageFile} height={(vertHeight * 2).toString}/>
            {leftChart.html}
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>
              {stats("Right", rightProfile)}
            </h3>
            <img src={rightImageFile} height={(vertHeight * 2).toString}/>
            {rightChart.html}
          </div>
        </div>

      </div>
    }

    val runScript = Seq(
      """<script>""",
      xChart.javascript,
      yChart.javascript,
      topChart.javascript,
      bottomChart.javascript,
      leftChart.javascript,
      rightChart.javascript,
      """</script>"""
    ).mkString("\n")

    val fileName = {
      df.al.get(TagByName.RadiationMachineName).getSingleStringValueOrEmptyString + "_" +
        df.al.get(TagByName.AcquisitionDate).getSingleStringValueOrEmptyString + "_" +
        df.al.get(TagByName.AcquisitionTime).getSingleStringValueOrEmptyString + ".html"
    }

    makeHtml(fileName, content, runScript)

  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")

    val dir = new File("""D:\aqa\wl\mirror\TB5\20250410""")

    val fileList = Util.listDirFiles(dir).filter(_.getName.toLowerCase().endsWith(".dcm")).map(f => new DicomFile(f))

    fileList.foreach(doFile)

    Trace.trace("Done")
  }

}
