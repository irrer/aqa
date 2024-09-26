package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import org.aqa.db.PSMBeam
import org.aqa.Logging

import java.awt.image.BufferedImage
import javax.vecmath.Point2i

/**
  * Container for beam results with extra values that do not need to go into the database.
  * @param psmBeam Analysis results that go in the database.
  * @param rtimage Original DICOM
  * @param bufferedImage Whole image picture.
  * @param pixelList List of coordinates used to sample circle in center of beam.
  */
case class PSMBeamAnalysisResult(
    psmBeam: PSMBeam,
    rtimage: AttributeList,
    bufferedImage: BufferedImage,
    pixelList: Map[Point2i, Double]
) extends Logging {}
