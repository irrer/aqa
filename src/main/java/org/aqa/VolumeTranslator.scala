package org.aqa

import com.pixelmed.dicom.AttributeList
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import org.aqa.webrun.phase2.Phase2Util
import javax.vecmath.Point3d

/**
 * Support mapping points between the coordinates in voxels verses the coordinates in mm.
 */

class VolumeTranslator(alList: Seq[AttributeList]) {

  val sorted = Util.sortByZ(alList)
  val voxSize = Util.getVoxSize_mm(sorted)

  private def intOf(tag: AttributeTag): Int = sorted.head.get(tag).getIntegerValues.head

  val width = intOf(TagFromName.Columns)
  val height = intOf(TagFromName.Rows)

  // Image XY center in pixels
  val imageSliceCenter = new Point2D.Double(((width - 1.0) / 2), ((height - 1.0) / 2))

  //  val width = intOf(TagFromName.Columns)
  //  val height = intOf(TagFromName.Rows)
  //  val depth = alList.size

  // base position of XYZ coordinate system in mm.
  val ImagePositionPatient = sorted.head.get(TagFromName.ImagePositionPatient).getDoubleValues.toSeq

  def vox2mm(point: Point3d): Point3d = {
    val x = (point.getX * voxSize.getX) + ImagePositionPatient(0)
    val y = (point.getY * voxSize.getY) + ImagePositionPatient(1)
    val z = (point.getZ * voxSize.getZ) + ImagePositionPatient(2)
    new Point3d(x, y, z)
  }

  def mm2vox(point: Point3d): Point3d = {
    val x = (point.getX - ImagePositionPatient(0)) / voxSize.getX
    val y = (point.getY - ImagePositionPatient(1)) / voxSize.getY
    val z = (point.getZ - ImagePositionPatient(2)) / voxSize.getZ
    new Point3d(x, y, z)
  }

}

