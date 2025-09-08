/*
 * Copyright 2021 Regents of the University of Michigan
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

package learn

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.DicomFile
import org.aqa.Util

import java.io.File

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object XBiCubicImage {

  def main(args: Array[String]): Unit = {

    Trace.trace

    // val file = new File("""D:\tmp\wl\nonorth\0010.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\1\0001.dcm""")
    val file = new File("""D:\tmp\wl\nonorth\psm\0018.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\TB5_Aug_20\0002.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\BR1_Phase2\0014.dcm""")

    val al = new DicomFile(file).attributeList.get

    val trans = new IsoImagePlaneTranslator(al)

    val dicomImage = new DicomImage(al)

    // val xCoordinateList = (0 until dicomImage.width).map(x => trans.pix2IsoCoordX(x.toDouble)).toArray
    // val yCoordinateList = (0 until dicomImage.height).map(y => trans.pix2IsoCoordY(y.toDouble)).toArray

    val xCoordinateList = (0 until dicomImage.width).map(_.toDouble).toArray
    val yCoordinateList = (0 until dicomImage.height).map(_.toDouble).toArray

    val valueMatrix = dicomImage.pixelData.map(row => row.map(_.toDouble).toArray).toArray

    Trace.trace()
    val interpolator = new PiecewiseBicubicSplineInterpolator()
    Trace.trace()
    val function = interpolator.interpolate(xCoordinateList, yCoordinateList, valueMatrix)
    def get(x: Double, y: Double): Double = function.value(y, x)

    if (false) {
      Trace.trace("x varying")
      (1 until dicomImage.width).foreach(x => println(get(x - .3, 440)))
      Trace.trace("y varying")
      (1 until dicomImage.height).foreach(y => println(get(888, y - .3)))
      Trace.trace()
      Trace.trace(get(9.3, 3.7))
      Trace.trace()
    }

    val angle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    Trace.trace(" 0: " + math.tan(Math.toRadians(0)))
    Trace.trace("90: " + math.tan(Math.toRadians(90)))
    val m = Math.tan(Math.toRadians(angle))

    /*
    def row(xEdge: Double, yEdge: Double): Seq[Double] = {
      val b = yEdge

      def func(x: Double) : Double= (m * x) + b

      (0 until dicomImage.width).map(x => )

    }
    */


    if (true) {
      val bufImg = dicomImage.toDeepColorBufferedImage(0.01)

      val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
      Util.writePng(bufImg, pngFile)

      Trace.trace(s"wrote $pngFile")
    }
    System.exit(0)
  }

}
