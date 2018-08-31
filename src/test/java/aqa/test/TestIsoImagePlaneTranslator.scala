
package aqa.test

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeFactory
import java.awt.geom.Point2D
import org.scalactic.source.Position.apply
import scala.collection.Seq
import org.aqa.webrun.phase2.IsoImagePlaneTranslator
import com.pixelmed.dicom.ValueRepresentation

/**
 * Test IsoImagePlaneTranslator.
 *
 */

class TestIsoImagePlaneTranslator extends FlatSpec with Matchers {

  private val al = new AttributeList

  private def put(tag: AttributeTag, valueList: Seq[String]): Unit = {
    val a = AttributeFactory.newAttribute(tag)
    val vr = a.getVR

    def add(v: String) = {
      vr match {
        case _ if ValueRepresentation.isDecimalStringVR(vr) => a.addValue(v.toDouble)
        case _ if ValueRepresentation.isIntegerStringVR(vr) => a.addValue(v.toInt)
        case _ => a.addValue(v)
      }
    }
    valueList.map(v => add(v))
    al.put(a)
  }

  private def put(tag: AttributeTag, value: String): Unit = put(tag, Seq(value))

  put(TagFromName.RTImageSID, "1500.01778082533")
  put(TagFromName.RadiationMachineSAD, "1000")
  put(TagFromName.Rows, "1190")
  put(TagFromName.Columns, "1190")
  put(TagFromName.ImagePlanePixelSpacing, Seq("0.336", "0.336"))

  val originIso = new Point2D.Double(0.0, 0.0)
  val originImage = new Point2D.Double(0.0, 0.0)

  "isopoint" should "translate to image plane" in {

    val j = al.get(TagFromName.ImagePlanePixelSpacing) // TODO rm
    println("j: " + j)

    val translator = new IsoImagePlaneTranslator(al)
    val isoCenterPoint = new Point2D.Double(0.0, 0.0)
    val imageCenterPoint = translator.iso2Pix(isoCenterPoint)
    println("imageCenterPoint: " + imageCenterPoint)
    
    // TODO next: Add more tests.  Maybe parameter testing?
    
    (Util.randomSecureHash.size > 10) should be(true)
  }

  //  "randomSecureHash" should "be different each time" in {
  //    val size = 100
  //    val list = (0 until size).map(i => Util.randomSecureHash).distinct
  //    list.size should be(size)
  //  }
}
