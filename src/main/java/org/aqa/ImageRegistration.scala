package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import javax.vecmath.Matrix4d
import javax.vecmath.Point3d
import edu.umro.ScalaUtil.Trace

/**
 * Support operations on image registration (spatial transforms) files.
 */

case class ImageRegistration(attrList: AttributeList) {

  private def getFrUid(al: AttributeList) = {
    al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  }

  /** Frame of reference of this image registration.  Should be the same as the RTPLAN's. */
  val frameOfRefUID = getFrUid(attrList)

  private val otherImageSeq = {
    val regSeq = DicomUtil.seqToAttr(attrList, TagFromName.RegistrationSequence)
    regSeq.filterNot(rs => getFrUid(rs).equals(frameOfRefUID)).head
  }

  /** Frame of reference UID that this image registration allows you to transform points to. */
  val otherFrameOfRefUID = getFrUid(otherImageSeq)

  /** True if this can translates to the frame of reference of the given attribute list. */
  def sameFrameOfRef(al: AttributeList): Boolean = getFrUid(al).equals(frameOfRefUID)

  private val matrix = {
    val mrs = DicomUtil.seqToAttr(otherImageSeq, TagFromName.MatrixRegistrationSequence).head
    val ms = DicomUtil.seqToAttr(mrs, TagFromName.MatrixSequence).head
    val fortm = ms.get(TagFromName.FrameOfReferenceTransformationMatrix)
    new Matrix4d(fortm.getDoubleValues)
  }
  
  /**
   * Get a copy of the matrix.  Use clone because matrixes are mutable.  Wish they weren't.
   */
  def getMatrix = matrix.clone.asInstanceOf[Matrix4d]

  /**
   * Transform a point by this matrix and create a new point.
   */
  def transform(point: Point3d): Point3d = {
    val x = point.clone.asInstanceOf[Point3d]
    matrix.transform(x)
    x
  }

  override def toString: String = {
    "frameOfRefUID: " + frameOfRefUID + "    otherFrameOfRefUID: " + otherFrameOfRefUID
  }
}

