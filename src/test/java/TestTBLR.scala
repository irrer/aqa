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


import scala.util.Random
import edu.umro.ImageUtil.DicomImage
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.phase2.MeasureTBLREdges
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import java.awt.Point
import org.aqa.Util
import edu.umro.ScalaUtil.Trace

object TestTBLR {

  def main(args: Array[String]): Unit = {

    println("Starting")

    val dir = new File("""D:\tmp\aqa\msk_wl""")
    val dicomFile090 = new DicomFile(new File(dir, "aqa-check-090.dcm"))
    val dicomFile270 = new DicomFile(new File(dir, "aqa-check-270.dcm"))
    val image090 = new DicomImage(dicomFile090.attributeList.get)
    val image270 = new DicomImage(dicomFile270.attributeList.get)

    val translator090 = new IsoImagePlaneTranslator(dicomFile090.attributeList.get)
    val translator270 = new IsoImagePlaneTranslator(dicomFile270.attributeList.get)

    val pointZero = new Point(0, 0)
    val edgePercent = 0.5
    val expected = new MeasureTBLREdges.TBLR(592, 686, 598, 685).pix2iso(translator090)
    val tblr090 = MeasureTBLREdges.measure(image090, translator090, Some(expected), 90, image090, pointZero, edgePercent)
    val tblr270 = MeasureTBLREdges.measure(image270, translator270, Some(expected), 270, image270, pointZero, edgePercent)

    def fmt(d: Double) = d.formatted("%10.7f")

    def show(m: MeasureTBLREdges.AnalysisResult, trans: IsoImagePlaneTranslator): String = {
      val tblr = m.measurementSet.pix2iso(trans)

      "top: " + fmt(tblr.top) +
        "    bottom: " + fmt(tblr.bottom) +
        "    left: " + fmt(tblr.left) +
        "    right: " + fmt(tblr.right) +
        "    center X, Y: " + fmt(tblr.center.getX) +
        ", " + fmt(tblr.center.getY)
    }
    println("------------------------------------------------------------------")

    println("tblr090 iso: " + show(tblr090, translator090))
    println("tblr270 iso: " + show(tblr270, translator270))

    val cx =
      (tblr090.measurementSet.pix2iso(translator090).center.getX +
        tblr270.measurementSet.pix2iso(translator270).center.getX) / 2
    val cy =
      (tblr090.measurementSet.pix2iso(translator090).center.getY +
        tblr270.measurementSet.pix2iso(translator270).center.getY) / 2

    println("Center of rotation: " + fmt(cx) + ", " + fmt(cy))

    val png090 = new File(dir, "090.png")
    png090.delete
    Util.writePng(tblr090.bufferedImage, png090)

    val png270 = new File(dir, "270.png")
    png270.delete
    Util.writePng(tblr270.bufferedImage, png270)

    println("Done.  Images in " + dir.getAbsolutePath)

  }

}