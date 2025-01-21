package org.aqa.webrun.wl.wlMonthly.wlMonthlyHTML

import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.wl.wlMonthly.WLMonthly
import org.aqa.webrun.wl.wlMonthly.WLTable
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLCollimator
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.Logging

import java.io.File
import scala.xml.Elem

/**
  * Generate HTML to display all gradients.
  * @param extendedData Metadata.
  * @param monthly Monthly data.
  * @param table Table data.
  * @param collimator Collimator data.
  */
case class WLGradientHTML(extendedData: ExtendedData, monthly: WLMonthly, table: WLTable, collimator: WLCollimator) extends Logging {

  private val gradientDirName = "Gradient"

  private def fmtD(d: Double): String = d.formatted("%10.3f").trim

  private val gradientDir = new File(WLMonthlyHTML.dir(extendedData), gradientDirName)
  gradientDir.mkdirs()

  /**
    * Specify either the X or Y coordinate of the image to be created.
    * @param length Total span of coordinate.
    * @param center Center value of coordinate.
    * @param pix Number of pixels to render.
    */
  private case class CoordinateSpec(
      name: String,
      length: Double = 1.0,
      center: Double,
      pix: Int = 1000
  ) {
    private val inc = length / pix
    private val lo = -pix / 2
    private val hi = pix / 2
    private val incList = (lo until hi).map(_ * inc)

    val list: Seq[Double] = incList.map(_ + center)
  }

  private case class GradientImage(name: String, x: CoordinateSpec, y: CoordinateSpec, function: (Double, Double) => Double, description: String) {

    private val htmlFileName: String = s"$name.html"
    private val pngFileName: String = s"$name.png"

    private val htmlFile: File = new File(gradientDir, htmlFileName)
    private val pngFile: File = new File(gradientDir, pngFileName)

    private val array = {
      for (x <- x.list) yield {
        for (z <- y.list) yield {
          function(x, z).toFloat
        }
      }
    }

    private val di = new DicomImage(array.asInstanceOf[IndexedSeq[IndexedSeq[Float]]])

    private val buf = di.toDeepColorBufferedImage(di.minPixelValue, di.maxPixelValue)
    pngFile.delete()

    Util.writePng(buf, pngFile)
    logger.info(s"Wrote file $pngFile")

    private val htmlContent: Elem = {
      <div>
        <h3>Gradient Image</h3>
        <h4>{description}</h4>
        <br/>
        <em>X: {x.name + Util.fmtDbl(x.length) + " mm wide centered at " + fmtD(x.center)}</em>
        <br/>
        <em>Y: {y.name + Util.fmtDbl(x.length) + " mm wide centered at " + fmtD(y.center)}</em>
        <br/>
        <img src={pngFileName}/>
      </div>
    }

    private val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, htmlContent), pageTitle = name, runScript = None)

    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote file ${htmlFile.getAbsoluteFile}")

    val htmlRef: Elem = {
      <div>
        <a href={htmlFileName}>
          {gradientDirName + "/" + htmlFileName}
          <br> </br>
          <img src={pngFileName} width="200"/>
        </a>
      </div>
    }
  }

  /**
    * Make all gradient HTML, including the main page and a page for each gradient.  There are seven in
    * all, one for the collimator, and six for the table.
    *
    * The table gradients are all the combinations of dX, dZ, TableX, and TableZ as pairs:
    * <ol>
    *   <li> dX     dZ</li>
    *   <li> dX     TableX</li>
    *   <li> dX     TableZ</li>
    *   <li> dZ     TableX</li>
    *   <li> dZ     TableZ</li>
    *   <li> TableX TableZ</li>
    * </ol>
    *
    * @return An HTML snippet that serves as a reference to the gradient main page.
    */
  def makeHtml(): Elem = {

    val collimatorImage = {
      val x = CoordinateSpec(center = collimator.Coll_X_Optimized, name = "Coll X")
      val y = CoordinateSpec(center = collimator.Coll_Z_Optimized, name = "Coll Z")
      GradientImage("Collimator", x, y, collimator.MinCA_Rpp, "Collimator X vs Z")
    }

    val tableImageDXDZ = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "dX")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "dZ")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(x, y, table.get_Table_X_Optimized, table.get_Table_Z_Optimized)
      GradientImage("Table dX vs dZ", x, y, func, "Table dX vs dZ")
    }

    val tableImageDXTableX = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "dX")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "TableX")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(x, table.get_dZT__0_Optimized, y, table.get_Table_Z_Optimized)
      GradientImage("Table dX vs TableX", x, y, func, "Table dX vs TableX")
    }

    val tableImageDXTableZ = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "dX")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "TableZ")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(x, table.get_dZT__0_Optimized, table.get_Table_X_Optimized, y)
      GradientImage("Table dX vs TableZ", x, y, func, "Table dX vs TableZ")
    }

    val tableImageDZTableX = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "dZ")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "TableX")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(table.get_dXT__0_Optimized, x, y, table.get_Table_Z_Optimized)
      GradientImage("Table dZ vs TableX", x, y, func, "Table dZ vs TableX")
    }

    val tableImageDZTableZ = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "dZ")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "TableZ")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(table.get_dXT__0_Optimized, x, table.get_Table_X_Optimized, y)
      GradientImage("Table dZ vs TableZ", x, y, func, "Table dZ vs TableZ")
    }

    val tableImageTableXTableZ = {
      val x = CoordinateSpec(center = table.get_dXT__0_Optimized, name = "TableX")
      val y = CoordinateSpec(center = table.get_dZT__0_Optimized, name = "TableZ")
      def func(x: Double, y: Double) = table.minSquareOfBBDisplacement(table.get_dXT__0_Optimized, table.get_dZT__0_Optimized, x, y)
      GradientImage("Table TableX vs TableZ", x, y, func, "Table TableX vs TableZ")
    }

    // group the content as collimator by itself, and then the six table gradients.
    val htmlContent: Elem = {
      <div>
        <h2>Gradient Images</h2>
        <table class="table table-bordered">
          <tr>
            <td>
              {collimatorImage.htmlRef}
            </td>
          </tr>

          <tr>
            <td>
              {tableImageDXDZ.htmlRef}
            </td>
            <td>
              {tableImageDXTableX.htmlRef}
            </td>
            <td>
              {tableImageDXTableZ.htmlRef}
            </td>
          </tr>

          <tr>
            <td>
              {tableImageDZTableX.htmlRef}
            </td>
            <td>
              {tableImageDZTableZ.htmlRef}
            </td>
            <td>
              {tableImageTableXTableZ.htmlRef}
            </td>
          </tr>
        </table>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, htmlContent), pageTitle = "Gradients", runScript = None)

    val htmlFile = new File(gradientDir, "index.html")
    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote file ${htmlFile.getAbsoluteFile}")

    <a href={gradientDirName + "/index.html"}>Gradients</a>
  }

}
