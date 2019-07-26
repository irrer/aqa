package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import javax.vecmath.Matrix4d
import javax.vecmath.Point3d

/**
 * Support operations on image registration (spatial transforms) files.
 */

case class ImageRegistration(attrList: AttributeList) {

  private def getFrUid(al: AttributeList) = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString

  /** Frame of reference of this image registration. */
  val frameOfRefUID = getFrUid(attrList)

  private val otherImageSeq = {
    val regSeq = DicomUtil.seqToAttr(attrList, TagFromName.RegistrationSequence)
    regSeq.filterNot(rs => getFrUid(rs).equals(frameOfRefUID)).head
  }

  /** Frame of reference UID that this image registration allows you to transform points to. */
  val otherFrameOfRefUID = getFrUid(otherImageSeq)

  /** True if this can translates to the frame of reference of the given attribute list. */
  def worksWith(al: AttributeList): Boolean = getFrUid(al).equals(otherFrameOfRefUID)

  private val matrix = {
    val at = DicomUtil.seqToAttr(otherImageSeq, TagFromName.MatrixRegistrationSequence).head.get(TagFromName.FrameOfReferenceTransformationMatrix)
    new Matrix4d(at.getDoubleValues)
  }

  /**
   * Transform a point by this matrix and create a new point.
   */
  def transform(point: Point3d): Point3d = {
    val x = point.clone.asInstanceOf[Point3d]
    matrix.transform(x)
    x
  }
}

