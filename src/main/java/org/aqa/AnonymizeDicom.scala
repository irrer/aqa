package org.aqa

import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.ValueRepresentation

object AnonymizeDicom {
  type Replacement = Map[String, String]
}

import AnonymizeDicom.Replacement

class AnonymizeDicom(history: Map[AttributeTag, Replacement], phi: Map[AttributeTag, String]) extends Logging {

  def anonymize(source: AttributeList): AttributeList = {
    val dest = DicomUtil.clone(source)

    def anon(at: Attribute): Unit = {
      val tag = at.getTag

      /**
       * Look to see if this attribute should have a new value based on the old value, and if so, replace it.
       *
       * Next, if the value should be anonymized, but not with anything based on its current value, then replace it with the generic value.
       *
       * Finally, if the attribute has not shown up on any of these lists, then it is not PHI, so do nothing.
       */
      def replaceOrdinaryAttribute = {
        val oldValue = at.getSingleStringValueOrEmptyString

        def isReplaceableByHistory = {
          history.get(tag).isDefined &&
            history(tag).get(oldValue).isDefined
        }

        if (isReplaceableByHistory) {
          val newValue = history(tag)(oldValue)
          at.removeValues
          at.addValue(new String(newValue))
        } else {
          if (phi.get(tag).isDefined) {
            at.removeValues
            at.addValue(new String(phi(tag)))
          }
          // else this does not need to be anonymized, so do nothing
        }
      }

      def isUid = {
        val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
        (vr != null) && ValueRepresentation.isUniqueIdentifierVR(vr)
      }

      0 match {
        case _ if at.isInstanceOf[SequenceAttribute] => {
          val seq = at.asInstanceOf[SequenceAttribute]
          val list = (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList)
          list.map(al => anonAl(al))
        }
        case _ => replaceOrdinaryAttribute
      }
    }

    def anonAl(al: AttributeList) = al.values.toArray.map(a => anon(a.asInstanceOf[Attribute]))

    val j = dest.values.toArray.map(a => a.asInstanceOf[Attribute])

    anonAl(dest)
    dest
  }
}
