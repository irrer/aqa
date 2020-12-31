package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import javax.vecmath.Matrix4d
import javax.vecmath.Point3d
import edu.umro.ScalaUtil.Trace
import edu.umro.DicomDict.TagByName

/**
 * Support operations on image registration (spatial transforms) files.
 */

case class ImageRegistration(attrList: AttributeList) {
  def this(dicomFile: DicomFile) = this(dicomFile.attributeList.get)

  private def getFrUid(al: AttributeList) = {
    al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  }

  /** Frame of reference of this image registration.  Should be the same as the RTPLAN's. */
  val frameOfRefUID = getFrUid(attrList)

  private val otherImageSeq = {
    val regSeq = DicomUtil.seqToAttr(attrList, TagByName.RegistrationSequence)
    regSeq.filterNot(rs => getFrUid(rs).equals(frameOfRefUID)).head
  }

  /** Frame of reference UID that this image registration allows you to transform points to. */
  val otherFrameOfRefUID = getFrUid(otherImageSeq)

  def supportsFrameOfRef(frameOfRef: String) = frameOfRef.equals(otherFrameOfRefUID)
  def supportsFrameOfRef(al: AttributeList) = getFrUid(al).equals(otherFrameOfRefUID)

  /** True if this can translates to the frame of reference of the given attribute list. */
  def sameFrameOfRef(al: AttributeList): Boolean = getFrUid(al).equals(frameOfRefUID)

  private val matrix = {
    val mrs = DicomUtil.seqToAttr(otherImageSeq, TagByName.MatrixRegistrationSequence).head
    val ms = DicomUtil.seqToAttr(mrs, TagByName.MatrixSequence).head
    val fortm = ms.get(TagByName.FrameOfReferenceTransformationMatrix)
    new Matrix4d(fortm.getDoubleValues)
  }

  /**
   * Get a copy of the matrix.  Use clone because matrixes are mutable.  Wish they weren't.
   */
  def getMatrix = matrix.clone.asInstanceOf[Matrix4d]

  /**
   * Transform a point by this matrix and create a new point.
   */
  def transform(point: Point3d): Point3d = Util.transform(matrix, point)

  /**
   * Transform a point by an inverse of this matrix and create a new point.
   */
  def invTransform(point: Point3d): Point3d = Util.invTransform(matrix, point)

  override def toString: String = {
    "frameOfRefUID: " + frameOfRefUID + "    otherFrameOfRefUID: " + otherFrameOfRefUID
  }
}

