package org.aqa.webrun.focalSpot

import edu.umro.DicomDict.TagByName

object FSMatlab {

  def fsToMatlab(fsMeasure: FSMeasure): String = {

    val NominalBeamEnergyText = { if (fsMeasure.NominalBeamEnergy == fsMeasure.NominalBeamEnergy.round) fsMeasure.NominalBeamEnergy.round.toString else fsMeasure.NominalBeamEnergy.toString }

    val typeOf = if (fsMeasure.isMLC) "MLC" else "Jaw"
    val prefix = "MV" + NominalBeamEnergyText + "_" + typeOf + "_" + fsMeasure.collimatorAngleRounded_deg.formatted("%03d") + "_"

    val translation = fsMeasure.rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues.toSeq
    val RTImageSID = fsMeasure.rtimage.get(TagByName.RTImageSID).getDoubleValues.head
    val RadiationMachineSAD = fsMeasure.rtimage.get(TagByName.RadiationMachineSAD).getDoubleValues.head
    val ImagePlanePixelSpacing = fsMeasure.rtimage.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.toSeq
    val RTImagePosition = fsMeasure.rtimage.get(TagByName.RTImagePosition).getDoubleValues.toSeq

    val top_px = fsMeasure.analysisResult.measurementSet.top
    val bottom_px = fsMeasure.analysisResult.measurementSet.bottom
    val left_px = fsMeasure.analysisResult.measurementSet.left
    val right_px = fsMeasure.analysisResult.measurementSet.right

    // format all numbers like this
    val f = "%20.16f"

    val matlab: String =
      s"""
         |
         |% --------------------------------------------------------------------------------------------------------
         |
         |fprintf("Matlab code calculating focal spot values for $typeOf ${fsMeasure.collimatorAngleRounded_deg} for MV $NominalBeamEnergyText\\n");
         |
         |% DICOM 3002,000D XRayImageReceptorTranslation : https://dicom.innolitics.com/ciods/rt-image/rt-image/3002000d
         |${prefix}transX = ${translation.head};
         |${prefix}transY = ${translation(1)};
         |${prefix}transZ = ${translation(2)};
         |fprintf("${prefix}transX: $f\\n", ${prefix}transX);
         |fprintf("${prefix}transY: $f\\n", ${prefix}transY);
         |fprintf("${prefix}transZ: $f\\n", ${prefix}transZ);
         |
         |% DICOM 3002,0026 RTImageSID : Distance from radiation machine source to image plane (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020026
         |${prefix}SID = $RTImageSID;
         |fprintf("${prefix}SID: $f\\n", ${prefix}SID);
         |
         |% DICOM 3002,0022 RadiationMachineSAD  : Radiation source to Gantry rotation axis distance (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020022
         |${prefix}SAD = $RadiationMachineSAD;
         |fprintf("${prefix}SAD: $f\\n", ${prefix}SAD);
         |
         |% DICOM 3002,0011 ImagePlanePixelSpacing : Pixel size X and Y (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020011
         |${prefix}pixSizeX = ${ImagePlanePixelSpacing.head};
         |${prefix}pixSizeY = ${ImagePlanePixelSpacing(1)};
         |fprintf("${prefix}pixSizeX: $f\\n", ${prefix}pixSizeX);
         |fprintf("${prefix}pixSizeY: $f\\n", ${prefix}pixSizeY);
         |
         |% Expansion ratio between isoplane and image plane
         |${prefix}beamExpansion = ${prefix}SID / ${prefix}SAD;
         |fprintf("${prefix}beamExpansion: $f\\n", ${prefix}beamExpansion);
         |fprintf("\\n");
         |
         |% DICOM 3002,0012 RTImagePosition : RT Image Plane, Position and Orientation (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020012
         |${prefix}imagePosX = ${RTImagePosition.head} / ${prefix}beamExpansion;
         |${prefix}imagePosY = ${RTImagePosition(1)} / ${prefix}beamExpansion;
         |fprintf("${prefix}imagePosX: $f\\n", ${prefix}imagePosX);
         |fprintf("${prefix}imagePosY: $f\\n", ${prefix}imagePosY);
         |fprintf("\\n");
         |
         |% Expansion ratio between isoplane and image plane for X and Y pixels
         |${prefix}expandX = ${prefix}beamExpansion / ${prefix}pixSizeX;
         |${prefix}expandY = ${prefix}beamExpansion / ${prefix}pixSizeY;
         |fprintf("${prefix}expandX: $f\\n", ${prefix}expandX);
         |fprintf("${prefix}expandY: $f\\n", ${prefix}expandY);
         |fprintf("\\n");
         |
         |% Edge measurements in pixel coordinates for $prefix
         |${prefix}top_px    = $top_px;
         |${prefix}bottom_px = $bottom_px;
         |${prefix}left_px   = $left_px;
         |${prefix}right_px  = $right_px;
         |fprintf("${prefix}top_px    : $f\\n", ${prefix}top_px);
         |fprintf("${prefix}bottom_px : $f\\n", ${prefix}bottom_px);
         |fprintf("${prefix}left_px   : $f\\n", ${prefix}left_px);
         |fprintf("${prefix}right_px  : $f\\n", ${prefix}right_px);
         |fprintf("\\n");
         |
         |% Edge measurements in mm in isoplane for $prefix
         |${prefix}top_mm    = ( ${prefix}top_px    / ${prefix}expandY ) - ${prefix}imagePosY - ${prefix}transY;
         |${prefix}bottom_mm = ( ${prefix}bottom_px / ${prefix}expandY ) - ${prefix}imagePosY - ${prefix}transY;
         |${prefix}left_mm   = ( ${prefix}left_px   / ${prefix}expandX ) + ${prefix}imagePosX + ${prefix}transX;
         |${prefix}right_mm  = ( ${prefix}right_px  / ${prefix}expandX ) + ${prefix}imagePosX + ${prefix}transX;
         |fprintf("${prefix}top_mm    : $f\\n", ${prefix}top_mm);
         |fprintf("${prefix}bottom_mm : $f\\n", ${prefix}bottom_mm);
         |fprintf("${prefix}left_mm   : $f\\n", ${prefix}left_mm);
         |fprintf("${prefix}right_mm  : $f\\n", ${prefix}right_mm);
         |fprintf("\\n");
         |
         |fprintf("Edge results calculated by AQA that were stored in the database (for verification).\\n");
         |fprintf("$prefix top    AQA_mm : $f\\n", ${fsMeasure.focalSpot.topEdge_mm});
         |fprintf("$prefix bottom AQA_mm : $f\\n", ${fsMeasure.focalSpot.bottomEdge_mm});
         |fprintf("$prefix left   AQA_mm : $f\\n", ${fsMeasure.focalSpot.leftEdge_mm});
         |fprintf("$prefix right  AQA_mm : $f\\n", ${fsMeasure.focalSpot.rightEdge_mm});
         |fprintf("\\n");
         |
         |${prefix}centerX_mm = ( ${prefix}left_mm + ${prefix}right_mm  ) / 2.0;
         |${prefix}centerY_mm = ( ${prefix}top_mm  + ${prefix}bottom_mm ) / 2.0;
         |fprintf("${prefix}centerX_mm  : $f\\n", ${prefix}centerX_mm);
         |fprintf("${prefix}centerY_mm  : $f\\n", ${prefix}centerY_mm);
         |fprintf("\\n-----------------------------\\n");
         |
         |""".stripMargin

    matlab
  }

}
